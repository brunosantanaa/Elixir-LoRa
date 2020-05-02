defmodule LoRa do
  use Bitwise
  use GenServer

  alias ElixirALE.GPIO
  alias ElixirALE.SPI

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
  # deprecated
  def crc(), do: enable_crc()
  def disable_crc(), do: GenServer.cast(__MODULE__, :disable_crc)
  # deprecated
  def no_crc(), do: disable_crc()

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

    cond do
      pin_reset > 0 ->
        {:ok, rst} = GPIO.start_link(pin_reset, :output)
        GPIO.write(rst, 1)
        :timer.sleep(20)
        GPIO.write(rst, 0)
        :timer.sleep(20)
        GPIO.write(rst, 1)
        :timer.sleep(10)

        {:ok,
         %{
           state
           | spi: %{pid: spi, ss: ss},
             rst: rst,
             config: %{frequency: 0, impl_header: false, packet_index: 0, on_receive: nil}
         }}

      pin_reset == nil ->
        {:ok,
         %{
           state
           | spi: %{pid: spi, ss: ss},
             config: %{frequency: 0, impl_header: false, packet_index: 0, on_receive: nil}
         }}
    end
  end

  def handle_info({:DOWN, _ref, :process, _from, type}, state) do
    Logger.debug("send packet - Exit with #{type}")
    {:noreply, state}
  end

  def handle_call({:begin, frequency}, _from, state) do
    version = read_register(state[:spi], @reg_version)

    if version == 0x12 do
      # Sleep mode
      sleep(state[:spi])
      # Set frequency
      set_frequency(state[:spi], frequency)
      # Set base addresses
      write_register(state[:spi], @reg_fifo_tx_base_addr, 0)
      write_register(state[:spi], @reg_fifo_rx_base_addr, 0)
      # Set LNA boost
      write_register(state[:spi], @reg_lna, read_register(state[:spi], @reg_lna) ||| 0x03)
      # Set auto AGC
      write_register(state[:spi], @reg_modem_config_3, 0x04)
      # Set output power to 17 dBm
      set_tx_power(state[:spi], 17)
      # put in standby mode
      idle(state[:spi])

      {:reply, :ok, %{state | :config => %{state[:config] | :frequency => frequency}}}
    else
      Logger.error("LoRa Version")
      {:reply, {:error, :version}, state}
    end
  end

  def handle_cast({:begin_packet, implicit_header}, state) do
    # Put in standby mode
    idle(state[:spi])

    if implicit_header do
      GenServer.cast(__MODULE__, :impl_header_mode)
    else
      GenServer.cast(__MODULE__, :expl_header_mode)
    end

    # Reset FIFO address and payload length
    write_register(state[:spi], @reg_fifo_addr_ptr, 0)
    write_register(state[:spi], @reg_payload_length, 0)

    {:noreply, state}
  end

  def handle_cast({:set_sf, sf}, state) do
    if sf == 6 do
      write_register(state[:spi], @reg_detection_optimize, 0xC5)
      write_register(state[:spi], @reg_detection_threshold, 0x0C)
    else
      write_register(state[:spi], @reg_detection_optimize, 0xC3)
      write_register(state[:spi], @reg_detection_threshold, 0x0A)
    end

    config2 = read_register(state[:spi], @reg_modem_config_2)

    sf_ = (config2 &&& 0x0F) ||| (sf <<< 4 &&& 0xF0)
    write_register(state[:spi], @reg_modem_config_2, sf_)

    set_ldo_flag(state[:spi])

    {:noreply, state}
  end

  def handle_cast({:set_sbw, sbw}, state) do
    reg = read_register(state.spi, @reg_modem_config_1)

    @bw_freqs
    |> Enum.filter(fn {_i, f} -> sbw <= f end)
    |> List.first()
    |> set_bw(state.spi, reg)

    set_ldo_flag(state.spi)
    {:noreply, state}
  end

  def handle_cast({:print, text}, state) do
    current_length = read_register(state[:spi], @reg_payload_length)
    bytelist = text |> String.to_charlist()

    if current_length + length(bytelist) < @max_pkt_length,
      do: write(state[:spi], bytelist, length(bytelist)),
      else: write(state[:spi], bytelist, @max_pkt_length - current_length)

    {:noreply, state}
  end

  def handle_cast(:end_lora, state) do
    # Put in sleep mode
    sleep(state[:spi])
    SPI.release(state[:spi])
    {:noreply, state}
  end

  def handle_cast(:end_packet, state) do
    # Put in TX mode
    write_register(state[:spi], @reg_op_mode, @mode_long_range_mode ||| @mode_tx)

    pid = spawn_link(__MODULE__, :verify_end_packet, [state])
    ref = Process.monitor(pid)
    owner = self()
    Task.yield(%Task{pid: pid, ref: ref, owner: owner}, 2000)
    {:noreply, state}
  end

  def handle_cast(:enable_crc, state) do
    write_register(
      state[:spi],
      @reg_modem_config_2,
      read_register(state[:spi], @reg_modem_config_2) ||| 0x04
    )

    {:noreply, state}
  end

  def handle_cast(:disable_crc, state) do
    write_register(
      state[:spi],
      @reg_modem_config_2,
      read_register(state[:spi], @reg_modem_config_2) ||| 0xFB
    )

    {:noreply, state}
  end

  def handle_cast(:impl_header_mode, state) do
    write_register(
      state[:spi],
      @reg_modem_config_1,
      read_register(state[:spi], @reg_modem_config_1) ||| 0x01
    )

    {:noreply, %{state | :config => %{state[:config] | :impl_header => true}}}
  end

  def handle_cast(:expl_header_mode, state) do
    write_register(
      state[:spi],
      @reg_modem_config_1,
      read_register(state[:spi], @reg_modem_config_1) ||| 0xFE
    )

    {:noreply, %{state | :config => %{state[:config] | :impl_header => false}}}
  end

  defp set_bw({bw, _freq}, spi, reg) do
    write_register(spi, @reg_modem_config_1, (reg &&& 0x0F) ||| bw <<< 4)
  end

  defp write(spi, bytelist, size) do
    for i <- 0..(size - 1) do
      :timer.sleep(1)
      write_register(spi, @reg_fifo, Enum.at(bytelist, i))
    end

    write_register(spi, @reg_payload_length, size)
  end

  def verify_end_packet(state, counter \\ 0) do
    if (read_register(state.spi, @reg_irq_flags) &&& @irq_tx_done_mask) == 0 do
      :timer.sleep(1)
      if counter <= @verify_end_packet_cycles,
        do: verify_end_packet(state, counter + 1),
        else: Logger.error("Timeout")
    else
      write_register(state.spi, @reg_irq_flags, @irq_tx_done_mask)
    end
  end

  # defp transmitting?(spi) do
  #   irq_flags = read_register(spi, @reg_irq_flags)

  #   unless (irq_flags &&& @irq_tx_done_mask) == 0,
  #     do: write_register(spi, @reg_irq_flags, @irq_tx_done_mask)

  #   if (read_register(spi, @reg_op_mode) &&& @mode_tx) == @mode_tx, do: true, else: false
  # end

  defp sleep(spi) do
    write_register(spi, @reg_op_mode, @mode_long_range_mode ||| @mode_sleep)
  end

  defp idle(spi) do
    write_register(spi, @reg_op_mode, @mode_long_range_mode ||| @mode_stdby)
  end

  defp set_frequency(spi, freq) do
    frt = trunc((trunc(freq) <<< 19) / 32_000_000)
    write_register(spi, @reg_frf_msb, frt >>> 16)
    write_register(spi, @reg_frf_mid, frt >>> 8)
    write_register(spi, @reg_frf_lsb, frt >>> 0)
  end

  defp read_register(spi, address) do
    single_transfer(spi, address &&& 0x7F, 0x00)
  end

  defp write_register(spi, address, value) do
    single_transfer(spi, address ||| 0x80, value)
  end

  defp single_transfer(spi, address, value) do
    GPIO.write(spi.ss, 0)
    <<_, resp>> = SPI.transfer(spi.pid, <<address, value>>)
    GPIO.write(spi.ss, 1)
    resp
  end

  # defp set_tx_power(spi, level, output_pin) when output_pin == @pa_output_rfo_pin do
  #   cond do
  #     level < 0 -> write_register(spi, @reg_pa_config, 0x70 ||| 0)
  #     level > 14 -> write_register(spi, @reg_pa_config, 0x70 ||| 14)
  #     level >= 0 -> write_register(spi, @reg_pa_config, 0x70 ||| level)
  #   end
  # end

  defp set_tx_power(spi, level) do
    if level > 17 do
      write_register(spi, @reg_pa_dac, 0x87)
      set_ocp(spi, 140)

      if level > 20,
        do: write_register(spi, @reg_pa_config, @pa_boost ||| 15),
        else: write_register(spi, @reg_pa_config, @pa_boost ||| level - 5)
    else
      write_register(spi, @reg_pa_dac, 0x84)
      set_ocp(spi, 100)

      if level < 2,
        do: write_register(spi, @reg_pa_config, @pa_boost ||| 0),
        else: write_register(spi, @reg_pa_config, @pa_boost ||| level - 2)
    end
  end

  defp set_ocp(spi, ocp) do
    cond do
      ocp <= 120 ->
        write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& uint8((uint8(ocp) - 45) / 5)))

      ocp <= 240 ->
        write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& uint8((uint8(ocp) + 30) / 10)))

      ocp > 240 ->
        write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& 27))
    end
  end

  defp set_ldo_flag(spi) do
    spf = get_spreading_factor(spi)
    bw = get_signal_band_width(spi)

    unless bw == nil do 
      symbol_duration = 1000 / (bw / (1 <<< spf))
  
      ldo_on = if symbol_duration > 16, do: 1, else: 0

      write_register(
        spi,
        @reg_modem_config_3,
        bit_write(read_register(spi, @reg_modem_config_3), 3, ldo_on)
      )
    end
  end

  defp get_signal_band_width(spi) do
    bw = read_register(spi, @reg_modem_config_1) >>> 4

    @bw_freqs[bw]
  end

  defp get_spreading_factor(spi) do
    config = read_register(spi, @reg_modem_config_2)

    config >>> 4
  end

  defp uint8(val) do
    cond do
      val < 0 ->
        teste = 256 + trunc(val)

        if teste < 0 do
          uint8(teste)
        else
          teste
        end

      val <= 255 ->
        trunc(val)

      val > 255 ->
        rem(trunc(val), 256)
    end
  end

  # defp change_third_bit(value, bit), do: if(bit == 0, do: value &&& 0xF7, else: value ||| 8)

  def bit_write(value, bit, subs) do
    {ini, fim} = list_bits(value) |> add_zeros(bit) |> Enum.split(bit)
    [_h | t] = fim
    Enum.reverse(ini ++ [subs] ++ t) |> listbits_to_integer()
  end

  defp add_zeros(list, bit, state \\ [], i \\ 0) do
    if length(list) <= bit and length(state) <= bit do
      if i <= length(list) - 1 do
        add_zeros(list, bit, state ++ [Enum.at(list, i)], i + 1)
      else
        add_zeros(list, bit, state ++ [0], i + 1)
      end
    else
      if bit <= length(list) - 1, do: list, else: state
    end
  end

  defp listbits_to_integer(list, state \\ 0, pot \\ 0) do
    if pot < length(list) do
      val = list |> Enum.reverse() |> Enum.at(pot)
      listbits_to_integer(list, state + :math.pow(2, pot) * val, pot + 1)
    else
      trunc(state)
    end
  end

  defp list_bits(value, state \\ []) do
    unless div(value, 2) == 0 do
      list_bits(div(value, 2), state ++ [rem(value, 2)])
    else
      state ++ [rem(value, 2)]
    end
  end
end
