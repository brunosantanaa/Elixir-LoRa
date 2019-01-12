defmodule LoRa do

  use Bitwise
  use GenServer

  alias ELixirALE.GPIO
  alias ElixirALE.SPI

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
  @reg_fifo_rx_current_addr 0x10
  @reg_irq_flags 0x12
  @reg_rx_nb_bytes 0x13
  @reg_pkt_snr_value 0x19
  @reg_pkt_rssi_value 0x1A
  @reg_modem_config_1 0x1D
  @reg_modem_config_2 0x1E
  @reg_preamble_msb 0x20
  @reg_preamble_lsb 0x21
  @reg_payload_length 0x22
  @reg_modem_config_3 0x26
  @reg_freq_error_msb 0x28
  @reg_freq_error_mid 0x29
  @reg_freq_error_lsb 0x2A
  @reg_rssi_wideband 0x2C
  @reg_detection_optimize 0x31
  @reg_invertiq 0x33
  @reg_detection_threshold 0x37
  @reg_sync_word 0x39
  @reg_invertiq2 0x3B
  @reg_dio_mapping_1 0x40
  @reg_version 0x42
  @reg_pa_dac 0x4D

  # modes
  @mode_long_range_mode 0x80
  @mode_sleep 0x00
  @mode_stdby 0x01
  @mode_tx 0x03
  @mode_rx_continuous 0x05
  @mode_rx_single 0x06

  # pa config
  @pa_boost 0x80

  # irq masks
  @irq_tx_done_mask 0x08
  @irq_payload_crc_error_mask 0x20
  @irq_rx_done_mask 0x40

  @max_pkt_length 255
  @pa_output_rfo_pin 0

  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  def begin(frequency), do: GenServer.cast(__MODULE__, {:begin, frequency})

  def set_spreading_factor(sf \\ 6) when sf >= 6 and sf <= 12,
    do: GenServer.cast(__MODULE__, {:set_sf, sf})

  def set_signal_band_width(sbw), do: GenServer.cast(__MODULE__, {:set_sbw, sbw})

  def begin_packet(implicit_header \\ false),
    do: GenServer.cast(__MODULE__, {:begin_packet, implicit_header})

  def end_packet(async \\ false), do: GenServer.cast(__MODULE__, {:end_packet, async})

  def print(text), do: GenServer.cast(__MODULE__, {:print, text})

  def init(config) do
    # {:ok, ss} = GPIO.start_link(8, :output)
    # GPIO.write(ss, 1)
    {:ok, reset} = GPIO.start_link(config.reset, :output)
    GPIO.write(reset, 1)
    :timer.sleep(10)
    GPIO.write(reset, 0)
    :timer.sleep(10)
    {:ok, spi} = SPI.start_link("spidev0.0", speed_hz: 8_000_000)

    {:ok,
     %{
       spi: spi,
       rst: reset,
       config: %{frequency: 0, impl_header: false, packet_index: 0, on_receive: nil}
     }}
  end

  def handle_cast({:begin, frequency}, state) do
    sleep(state.spi)
    set_frequency(frequency, state)

    write_register(state.spi, @reg_fifo_tx_base_addr, 0)
    write_register(state.spi, @reg_fifo_rx_base_addr, 0)

    {:ok, read} = read_register(state.spi, @reg_lna)

    write_register(state.spi, @reg_lna, read ||| 0x03)

    write_register(state.spi, @reg_modem_config_3, 0x04)

    set_tx_power(state.spi, 17)

    idle()

    {:noreply, %{state | :config => %{state[:config] | :frequency => frequency}}}
  end

  def handle_cast({:set_sf, sf}, state) do
    if sf == 6 do
      write_register(state.spi, @reg_detection_optimize, 0xC5)
      write_register(state.spi, @reg_detection_threshold, 0x0C)
    else
      write_register(state.spi, @reg_detection_optimize, 0xC3)
      write_register(state.spi, @reg_detection_threshold, 0x0A)
    end

    {:ok, read} = read_register(state.spi, @reg_modem_config_2)
    write_register(state.spi, @reg_modem_config_2, (read &&& 0x0F) ||| (sf <<< 4 &&& 0xF0))
    set_ldo_flag(state.spi)
    {:noreply, state}
  end

  def handle_cast({:set_sbw, sbw}, state) do
    {:ok, reg} = read_register(state.spi, @reg_modem_config_1)

    cond do
      sbw <= 7.8e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 0 <<< 4)
      sbw <= 10.4e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 1 <<< 4)
      sbw <= 15.6e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 2 <<< 4)
      sbw <= 20.8e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 3 <<< 4)
      sbw <= 31.25e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 4 <<< 4)
      sbw <= 41.7e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 5 <<< 4)
      sbw <= 62.5e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 6 <<< 4)
      sbw <= 125.0e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 7 <<< 4)
      sbw <= 250.0e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 8 <<< 4)
      sbw > 250.0e3 -> write_register(state.spi, @reg_modem_config_1, (reg &&& 0x0F) ||| 9 <<< 4)
    end

    set_ldo_flag(state.spi)
  end

  def handle_cast({:begin_packet, implicit_header}, state) do
    if transmitting?(state.spi) do
      {:noreply, state}
    else
      idle(state.spi)

      if implicit_header do
        GenServer.cast(__MODULE__, :impl_header_mode)
      else
        GenServer.cast(__MODULE__, :expl_header_mode)
      end

      write_register(state.spi, @reg_fifo_addr_ptr, 0)
      write_register(state.spi, @reg_payload_length, 0)

      {:noreply, state}
    end
  end

  def handle_cast({:end_packet, async}, state) do
    write_register(state.spi, @reg_op_mode, @mode_long_range_mode ||| @mode_tx)

    if async do
      :timer.sleep(1)
    else
      pid = spawn_link(__MODULE__, :verify_end_packet, [state.spi])
      ref = Process.monitor(pid)
      owner = self()
      Task.yield(%Task{pid: pid, ref: ref, owner: owner})

      write_register(state.spi, @reg_irq_flags, @irq_tx_done_mask)
    end
  end

  def handle_cast(:impl_header_mode, state) do
    {:ok, read} = read_register(state.spi, @reg_modem_config_1)
    write_register(state.spi, @reg_modem_config_1, read ||| 0x01)
    {:noreply, %{state | :config => %{state[:config] | :impl_header => true}}}
  end

  def handle_cast(:expl_header_mode, state) do
    {:ok, read} = read_register(state.spi, @reg_modem_config_1)
    write_register(state.spi, @reg_modem_config_1, read ||| 0xFE)
    {:noreply, %{state | :config => %{state[:config] | :impl_header => false}}}
  end

  def handle_cast({:print, text}, state) do
    {:ok, current_Length} = read_register(state.spi, @reg_payload_length)
    bytelist = text |> String.to_charlist()

    if current_Length + length(bytelist) < @max_pkt_length do
      Enum.map(bytelist, fn x -> write_register(state.spi, @reg_fifo, x) end)
      write_register(state.spi, @reg_payload_length, current_Length + length(bytelist))
    else
      size = @max_pkt_length - current_Length
      for n <- 0..size, do: write_register(state.spi, @reg_fifo, Enum.at(bytelist, n))
      write_register(state.spi, @reg_payload_length, current_Length + size)
    end
  end

  defp transmitting?(spi) do
    {:ok, op_mode} = read_register(spi, @reg_op_mode)
    {:ok, irq_flags} = read_register(spi, @reg_irq_flags)

    unless (irq_flags &&& @irq_tx_done_mask) == 0 do
      write_register(spi, @reg_irq_flags, @irq_tx_done_mask)
      false
    else
      false
    end

    if (op_mode &&& @mode_tx) == @mode_tx, do: true, else: false
  end

  defp sleep(spi) do
    SPI.transfer(spi, <<@reg_op_mode ||| @mode_sleep>>)
  end

  defp verify_end_packet(spi) do
    {:ok, read} = read_register(spi, @reg_irq_flags)
    unless (read &&& @irq_tx_done_mask) == 0, do: verify_end_packet(spi)
  end

  defp set_frequency(freq, state) do
    frt = (trunc(freq) <<< 19) / 32_000_000

    write_register(state.spi, @reg_frf_msb, frt >>> 16)
    write_register(state.spi, @reg_frf_mid, frt >>> 8)
    write_register(state.spi, @reg_frf_lsb, frt >>> 0)
  end

  defp read_register(spi, address) do
    single_transfer(spi, address &&& 0x7F, 0x00)
  end

  defp write_register(spi, address, value) do
    single_transfer(address ||| 0x80, value)
  end

  defp single_transfer(spi, address, value) do
    # GPIO.write(ss, 0)
    SPI.transfer(pid, <<address>>)
    resp = SPI.transfer(pid, <<value>>)
    # GPIO.write(ss, 1)
    {:ok, resp}
  end

  defp set_tx_power(spi, level, output_pin) when output_pin == @pa_output_rfo_pin do
    cond do
      level < 0 -> write_register(spi, @reg_pa_config, 0x70 ||| 0)
      level > 14 -> write_register(spi, @reg_pa_config, 0x70 ||| 14)
      level -> write_register(spi, @reg_pa_config, 0x70 ||| level)
    end
  end

  defp set_tx_power(spi, level, output_pin \\ 1) do
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

  defp idle(spi) do
    write_register(spi, @reg_op_mode, @mode_long_range_mode ||| @mode_stdby)
  end

  defp set_ldo_flag(spi) do
    {:ok, spf} = get_spreading_factor(spi)
    symbol_duration = 1000 / (get_signal_band_width() / (1 <<< spf))

    ldo_on = if symbol_duration > 16, do: 1, else: 0

    {:ok, config3} = read_register(spi, @reg_modem_config_3)

    write_register(spi, @reg_modem_config_3, change_third_bit(config3, ldo_on))
  end

  defp get_signal_band_width(spi) do
    {:ok, bw} = read_register(spi, @reg_modem_config_1) >>> 4

    case bw do
      0 -> 7.8e3
      1 -> 10.4e3
      2 -> 15.6e3
      3 -> 20.8e3
      4 -> 31.25e3
      5 -> 41.7e3
      6 -> 62.5e3
      7 -> 125.0e3
      8 -> 250.0e3
      9 -> 500.0e3
    end
  end

  defp get_spreading_factor(spi) do
    read_register(spi, @reg_modem_config_2 >>> 4)
  end

  defp change_third_bit(value, bit), do: if(bit == 0, do: value &&& 0xF7, else: value ||| 8)

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

  defp list_bits(value, state \\ []) do
    unless div(value, 2) == 0 do
      conv(div(value, 2), state ++ [rem(value, 2)])
    else
      Enum.reverse(state ++ [rem(value, 2)])
    end
  end
end
