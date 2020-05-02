defmodule LoRa.Modem do
  use Bitwise

  alias ElixirALE.GPIO
  alias ElixirALE.SPI

  alias LoRa.Communicator

  # REG
  #@reg_fifo 0x00
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

  # def transmitting?(spi) do
  #   irq_flags = Communicator.read_register(spi, @reg_irq_flags)

  #   unless (irq_flags &&& @irq_tx_done_mask) == 0,
  #     do: Communicator.write_register(spi, @reg_irq_flags, @irq_tx_done_mask)

  #   if (Communicator.read_register(spi, @reg_op_mode) &&& @mode_tx) == @mode_tx, do: true, else: false
  # end

  def sleep(spi) do
    Communicator.write_register(spi, @reg_op_mode, @mode_long_range_mode ||| @mode_sleep)
  end

  def idle(spi) do
    Communicator.write_register(spi, @reg_op_mode, @mode_long_range_mode ||| @mode_stdby)
  end

  def reset(rst) do
    GPIO.write(rst, 1)
    :timer.sleep(20)
    GPIO.write(rst, 0)
    :timer.sleep(20)
    GPIO.write(rst, 1)
    :timer.sleep(10)
  end

  def reset_fifo_payload(state) do
    # Reset FIFO address and payload length
    Communicator.write_register(state[:spi], @reg_fifo_addr_ptr, 0)
    Communicator.write_register(state[:spi], @reg_payload_length, 0)
  end

  def set_frequency(spi, freq) do
    frt = trunc((trunc(freq) <<< 19) / 32_000_000)
    Communicator.write_register(spi, @reg_frf_msb, frt >>> 16)
    Communicator.write_register(spi, @reg_frf_mid, frt >>> 8)
    Communicator.write_register(spi, @reg_frf_lsb, frt >>> 0)
  end

  # def set_tx_power(spi, level, output_pin) when output_pin == @pa_output_rfo_pin do
  #   cond do
  #     level < 0 -> Communicator.write_register(spi, @reg_pa_config, 0x70 ||| 0)
  #     level > 14 -> Communicator.write_register(spi, @reg_pa_config, 0x70 ||| 14)
  #     level >= 0 -> Communicator.write_register(spi, @reg_pa_config, 0x70 ||| level)
  #   end
  # end

  def set_tx_power(spi, level) do
    if level > 17 do
      Communicator.write_register(spi, @reg_pa_dac, 0x87)
      set_ocp(spi, 140)

      if level > 20,
        do: Communicator.write_register(spi, @reg_pa_config, @pa_boost ||| 15),
        else: Communicator.write_register(spi, @reg_pa_config, @pa_boost ||| level - 5)
    else
      Communicator.write_register(spi, @reg_pa_dac, 0x84)
      set_ocp(spi, 100)

      if level < 2,
        do: Communicator.write_register(spi, @reg_pa_config, @pa_boost ||| 0),
        else: Communicator.write_register(spi, @reg_pa_config, @pa_boost ||| level - 2)
    end
  end

  def set_ocp(spi, ocp) do
    cond do
      ocp <= 120 ->
        Communicator.write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& uint8((uint8(ocp) - 45) / 5)))

      ocp <= 240 ->
        Communicator.write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& uint8((uint8(ocp) + 30) / 10)))

      ocp > 240 ->
        Communicator.write_register(spi, @reg_ocp, 0x20 ||| (0x1F &&& 27))
    end
  end

  def set_ldo_flag(spi) do
    spf = get_spreading_factor(spi)
    bw = get_signal_band_width(spi)

    unless bw == nil do
      symbol_duration = 1000 / (bw / (1 <<< spf))

      ldo_on = if symbol_duration > 16, do: 1, else: 0

      Communicator.write_register(
        spi,
        @reg_modem_config_3,
        bit_write(Communicator.read_register(spi, @reg_modem_config_3), 3, ldo_on)
      )
    end
  end

  def set_bw({bw, _freq}, spi, reg) do
    Communicator.write_register(spi, @reg_modem_config_1, (reg &&& 0x0F) ||| bw <<< 4)
  end

  def get_signal_band_width(spi) do
    bw = Communicator.read_register(spi, @reg_modem_config_1) >>> 4

    @bw_freqs[bw]
  end

  def get_spreading_factor(spi) do
    config = Communicator.read_register(spi, @reg_modem_config_2)

    config >>> 4
  end

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
end
