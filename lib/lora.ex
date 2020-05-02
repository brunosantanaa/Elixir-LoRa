defmodule LoRa do
  use Bitwise
  use GenServer

  alias ElixirALE.GPIO
  alias ElixirALE.SPI
  alias LoRa.Modem
  alias LoRa.Communicator

  require Logger

  # SPI
  @lora_default_spi "spidev0.0"
  @lora_default_spi_frequency 8_000_000
  @lora_default_ss_pin 24
  @lora_default_reset_pin 25
  # @lora_default_dio0_pin 22

  # REG
  @reg_fifo 0x00
  @reg_op_mode 0x01
  @reg_frf_msb 0x06
  @reg_frf_mid 0x07
  @reg_frf_lsb 0x08
  @reg_pa_config 0x09
  @reg_ocp 0x0B
  @reg_lna 0x0C
  @reg_fifo_addr_ptr 0x0D
  @reg_fifo_tx_base_addr 0x0E
  @reg_fifo_rx_base_addr 0x0F
  # @reg_fifo_rx_current_addr 0x10
  @reg_irq_flags 0x12
  # @reg_rx_nb_bytes 0x13
  # @reg_pkt_snr_value 0x19
  # @reg_pkt_rssi_value 0x1A
  @reg_modem_config_1 0x1D
  @reg_modem_config_2 0x1E
  # @reg_preamble_msb 0x20
  # @reg_preamble_lsb 0x21
  @reg_payload_length 0x22
  @reg_modem_config_3 0x26
  # @reg_freq_error_msb 0x28
  # @reg_freq_error_mid 0x29
  # @reg_freq_error_lsb 0x2A
  # @reg_rssi_wideband 0x2C
  @reg_detection_optimize 0x31
  # @reg_invertiq 0x33
  @reg_detection_threshold 0x37
  # @reg_sync_word 0x39
  # @reg_invertiq2 0x3B
  # @reg_dio_mapping_1 0x40
  @reg_version 0x42
  @reg_pa_dac 0x4D

  # modes
  @mode_long_range_mode 0x80
  @mode_sleep 0x00
  @mode_stdby 0x01
  @mode_tx 0x03
  # @mode_rx_continuous 0x05
  # @mode_rx_single 0x06

  # pa config
  @pa_boost 0x80

  # irq masks
  @irq_tx_done_mask 0x08

  # @irq_payload_crc_error_mask 0x20
  # @irq_rx_done_mask 0x40

  @max_pkt_length 255
  # @pa_output_rfo_pin 0

  # End packet
  @verify_end_packet_cycles 10_000

  @bw_freqs %{
    0 => 7.8e3,
    1 => 10.4e3,
    2 => 15.6e3,
    3 => 20.8e3,
    4 => 31.25e3,
    5 => 41.7e3,
    6 => 62.5e3,
    7 => 125.0e3,
    8 => 250.0e3,
    9 => 250.0e3
  }

  def start_link(config \\ []), do: GenServer.start_link(__MODULE__, config, name: __MODULE__)
  def begin(frequency), do: GenServer.call(__MODULE__, {:begin, frequency})
  def end_lora(), do: GenServer.cast(__MODULE__, :end_lora)

  def set_spreading_factor(sf \\ 6) when sf >= 6 and sf <= 12,
    do: GenServer.cast(__MODULE__, {:set_sf, sf})

  def set_signal_band_width(sbw), do: GenServer.cast(__MODULE__, {:set_sbw, sbw})

  def begin_packet(implicit_header \\ false),
    do: GenServer.cast(__MODULE__, {:begin_packet, implicit_header})

  def end_packet(), do: GenServer.cast(__MODULE__, :end_packet)
  def print(text), do: GenServer.cast(__MODULE__, {:print, text})
  def enable_crc(), do: GenServer.cast(__MODULE__, :enable_crc)
  def disable_crc(), do: GenServer.cast(__MODULE__, :disable_crc)

  def init(config) do
    pin_ss = Keyword.get(config, :ss, @lora_default_ss_pin)
    pin_reset = Keyword.get(config, :rst, @lora_default_reset_pin)
    device = Keyword.get(config, :spi, @lora_default_spi)
    speed_hz = Keyword.get(config, :spi_speed, @lora_default_spi_frequency)

    {:ok, ss} = GPIO.start_link(pin_ss, :output)
    GPIO.write(ss, 1)

    {:ok, spi} = SPI.start_link(device, speed_hz: speed_hz, mode: 0)

    state = %{spi: nil, rst: nil, config: nil}

    Logger.info("Start LoRa Device")
    {:ok, rst} = GPIO.start_link(pin_reset, :output)
    Modem.reset(rst)

    {:ok,
     %{
       state
       | spi: %{pid: spi, ss: ss},
         rst: rst,
         config: %{frequency: 0, impl_header: false, packet_index: 0, on_receive: nil}
     }}
  end

  def handle_info({:DOWN, _ref, :process, _from, type}, state) do
    Logger.debug("send packet - Exit with #{type}")
    {:noreply, state}
  end

  def handle_call({:begin, frequency}, _from, state) do
    version = Communicator.read_register(state[:spi], @reg_version)

    if version == 0x12 do
      # Sleep mode
      Modem.sleep(state[:spi])
      # Set frequency
      Modem.set_frequency(state[:spi], frequency)
      # Set base addresses
      Communicator.write_register(state[:spi], @reg_fifo_tx_base_addr, 0)
      Communicator.write_register(state[:spi], @reg_fifo_rx_base_addr, 0)
      # Set LNA boost
      Communicator.write_register(state[:spi], @reg_lna, Communicator.read_register(state[:spi], @reg_lna) ||| 0x03)
      # Set auto AGC
      Communicator.write_register(state[:spi], @reg_modem_config_3, 0x04)
      # Set output power to 17 dBm
      Modem.set_tx_power(state[:spi], 17)
      # put in standby mode
      Modem.idle(state[:spi])

      {:reply, :ok, %{state | :config => %{state[:config] | :frequency => frequency}}}
    else
      Logger.error("LoRa Version")
      {:reply, {:error, :version}, state}
    end
  end

  def handle_cast({:begin_packet, implicit_header}, state) do
    # Put in standby mode
    Modem.idle(state[:spi])

    if implicit_header do
      GenServer.cast(__MODULE__, :impl_header_mode)
    else
      GenServer.cast(__MODULE__, :expl_header_mode)
    end

    {:noreply, state}
  end

  def handle_cast({:set_sf, sf}, state) do
    if sf == 6 do
      Communicator.write_register(state[:spi], @reg_detection_optimize, 0xC5)
      Communicator.write_register(state[:spi], @reg_detection_threshold, 0x0C)
    else
      Communicator.write_register(state[:spi], @reg_detection_optimize, 0xC3)
      Communicator.write_register(state[:spi], @reg_detection_threshold, 0x0A)
    end

    config2 = Communicator.read_register(state[:spi], @reg_modem_config_2)

    sf_ = (config2 &&& 0x0F) ||| (sf <<< 4 &&& 0xF0)
    Communicator.write_register(state[:spi], @reg_modem_config_2, sf_)

    Modem.set_ldo_flag(state[:spi])

    {:noreply, state}
  end

  def handle_cast({:set_sbw, sbw}, state) do
    reg = Communicator.read_register(state.spi, @reg_modem_config_1)

    @bw_freqs
    |> Enum.filter(fn {_i, f} -> sbw <= f end)
    |> List.first()
    |> Modem.set_bw(state.spi, reg)

    Modem.set_ldo_flag(state.spi)
    {:noreply, state}
  end

  def handle_cast({:print, text}, state) do
    current_length = Communicator.read_register(state[:spi], @reg_payload_length)
    bytelist = text |> String.to_charlist()

    if current_length + length(bytelist) < @max_pkt_length,
      do: Communicator.write(state[:spi], bytelist, length(bytelist)),
      else: Communicator.write(state[:spi], bytelist, @max_pkt_length - current_length)

    {:noreply, state}
  end

  def handle_cast(:end_lora, state) do
    # Put in sleep mode
    Modem.sleep(state[:spi])
    SPI.release(state[:spi])
    {:noreply, state}
  end

  def handle_cast(:end_packet, state) do
    # Put in TX mode
    Communicator.write_register(state[:spi], @reg_op_mode, @mode_long_range_mode ||| @mode_tx)

    pid = spawn_link(__MODULE__, :verify_end_packet, [state])
    ref = Process.monitor(pid)
    owner = self()
    Task.yield(%Task{pid: pid, ref: ref, owner: owner}, 2000)
    {:noreply, state}
  end

  def handle_cast(:enable_crc, state) do
    Communicator.write_register(
      state[:spi],
      @reg_modem_config_2,
      Communicator.read_register(state[:spi], @reg_modem_config_2) ||| 0x04
    )

    {:noreply, state}
  end

  def handle_cast(:disable_crc, state) do
    Communicator.write_register(
      state[:spi],
      @reg_modem_config_2,
      Communicator.read_register(state[:spi], @reg_modem_config_2) ||| 0xFB
    )

    {:noreply, state}
  end

  def handle_cast(:impl_header_mode, state) do
    Communicator.write_register(
      state[:spi],
      @reg_modem_config_1,
      Communicator.read_register(state[:spi], @reg_modem_config_1) ||| 0x01
    )

    Modem.reset_fifo_payload(state)

    {:noreply, %{state | :config => %{state[:config] | :impl_header => true}}}
  end

  def handle_cast(:expl_header_mode, state) do
    Communicator.write_register(
      state[:spi],
      @reg_modem_config_1,
      Communicator.read_register(state[:spi], @reg_modem_config_1) ||| 0xFE
    )

    Modem.reset_fifo_payload(state)

    {:noreply, %{state | :config => %{state[:config] | :impl_header => false}}}
  end

  def verify_end_packet(state, counter \\ 0) do
    if (Communicator.read_register(state.spi, @reg_irq_flags) &&& @irq_tx_done_mask) == 0 do
      :timer.sleep(1)

      if counter <= @verify_end_packet_cycles,
        do: verify_end_packet(state, counter + 1),
        else: Logger.error("Timeout")
    else
      Communicator.write_register(state.spi, @reg_irq_flags, @irq_tx_done_mask)
      Modem.reset(state.rst)
      begin(state.config.frequency)
    end
  end
  # defp change_third_bit(value, bit), do: if(bit == 0, do: value &&& 0xF7, else: value ||| 8)
end
