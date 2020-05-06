defmodule LoRa.Modem do
  use Bitwise
  require Logger

  alias ElixirALE.GPIO

  alias LoRa.Communicator
  alias LoRa.Parameters

  # def transmitting?(spi) do
  #   irq_flags = Communicator.read_register(spi, Parameters.register.irq_flags)

  #   unless (irq_flags &&& Parameters.irq.tx_done_mask) == 0,
  #     do: Communicator.write_register(spi, Parameters.register.irq_flags, Parameters.irq.tx_done_mask)

  #   if (Communicator.read_register(spi, Parameters.register.op_mode) &&& Parameters.mode.tx) == Parameters.mode.tx, do: true, else: false
  # end

  def begin(frequency, spi, power \\ 17) do
    # Sleep mode
    sleep(spi)
    # Set frequency
    set_frequency(frequency, spi)

    set_base_address(spi)
    # Set LNA boost
    set_LNA_boost(spi)
    # Set auto AGC
    set_auto_AGC(spi)
    # Set output power to 17 dBm
    set_tx_power(power, spi)
    # put in standby mode
    idle(spi)
  end

  def end_packet(from, spi, async? \\ false) do
    Communicator.write_register(
      spi,
      Parameters.register().op_mode,
      Parameters.mode().long_range_mode ||| Parameters.mode().tx
    )

    unless async? do
      pid = spawn_link(__MODULE__, :verify_end_packet, [spi, from])
      ref = Process.monitor(pid)

      Task.yield(%Task{pid: pid, ref: ref, owner: from}, 2000)
    end
  end

  def verify_end_packet(spi, from, counter \\ 0) do
    flags = Communicator.read_register(spi, Parameters.register().irq_flags)

    if (flags &&& Parameters.irq().tx_done_mask) == 0 do
      :timer.sleep(1)

      if counter <= Parameters.max().end_packet_cycles,
        do: verify_end_packet(spi, from, counter + 1),
        else: Logger.error("LoRa: send timeout")
    else
      # Reset flags
      Logger.debug("LoRa: verify end packet: iterations: #{counter}")
      send(from, :send_ok)
    end
  end

  def sleep(spi) do
    Communicator.write_register(
      spi,
      Parameters.register().op_mode,
      Parameters.mode().long_range_mode ||| Parameters.mode().sleep
    )
  end

  def idle(spi) do
    Communicator.write_register(
      spi,
      Parameters.register().op_mode,
      Parameters.mode().long_range_mode ||| Parameters.mode().stdby
    )
  end

  def read(frequency, owner, spi, index \\ 0, msg \\ []) do
    r_byte = Communicator.read_register(spi, Parameters.register().fifo)
    nb_bytes = Communicator.read_register(spi, Parameters.register().rx_nb_bytes) - 1

    if nb_bytes - index + 1 > 0,
      do: read(frequency, owner, spi, index + 1, msg ++ [r_byte]),
      else:
        Kernel.send(
          state.owner,
          {:lora,
           %{
             paket: List.to_string(msg),
             rssi: rssi(frequency, spi),
             snr: snr(spi),
             time: DateTime.now!("Etc/UTC")
           }}
        )
  end

  def snr(spi), do: Communicator.read_register(spi, Parameters.register().pkt_snr_value) * 0.25

  def rssi(frequency, spi) do
    rssi_value = Communicator.read_register(spi, Parameters.register().pkt_rssi_value)
    rssi_value - if frequency < 868.0e6, do: 164, else: 157
  end

  def parse_packet(from, spi, size \\ 0) do
    irq_flags = Communicator.read_register(spi, Parameters.register().irq_flags)

    if size > 0 do
      set_header_mode(false, spi)
      Communicator.write_register(spi, Parameters.register().payload_length, size &&& 0xFF)
    else
      set_header_mode(true, spi)
    end

    # Return irq flags
    Communicator.write_register(spi, Parameters.register().irq_flags, irq_flags)

    rx_done = irq_flags &&& Parameters.irq().rx_done_mask
    payload_crc = irq_flags &&& Parameters.irq().payload_crc_error_mask

    if rx_done != 0 and payload_crc != 0 == false do
      if size > 0 do
        packet_length = Communicator.read_register(spi, Parameters.register().payload_length)
        set_fifo_current_addr(spi)
        send(from, {:receive_msg, packet_length})
      else
        packet_length = Communicator.read_register(spi, Parameters.register().rx_nb_bytes)
        set_fifo_current_addr(spi)
        send(from, {:receive_msg, packet_length})
      end
    else
      op_mode = Communicator.read_register(spi, Parameters.register().op_mode)

      if op_mode != (Parameters.mode().long_range_mode ||| Parameters.mode().rx_single) do
        Communicator.write_register(spi, Parameters.register().fifo_addr_ptr, 0)
        new_op_mode = Parameters.mode().long_range_mode ||| Parameters.mode().rx_single
        Communicator.write_register(spi, Parameters.register().op_mode, new_op_mode)
      end

      false
    end
  end

  defp set_fifo_current_addr(spi) do
    current_addr = Communicator.read_register(spi, Parameters.register().fifo_rx_current_addr)
    Communicator.write_register(spi, Parameters.register().fifo_addr_ptr, current_addr)
    idle(spi)
  end

  def reset(rst) do
    GPIO.write(rst, 1)
    :timer.sleep(20)
    GPIO.write(rst, 0)
    :timer.sleep(20)
    GPIO.write(rst, 1)
    :timer.sleep(10)
  end

  def tx_done_flag(spi) do
    Communicator.write_register(
      spi,
      Parameters.register().irq_flags,
      Parameters.irq().tx_done_mask
    )
  end

  def reset_fifo_payload(spi) do
    # Reset FIFO address and payload length
    Communicator.write_register(spi, Parameters.register().fifo_addr_ptr, 0)
    Communicator.write_register(spi, Parameters.register().payload_length, 0)
  end

  def set_frequency(freq, spi) do
    frt = trunc((trunc(freq) <<< 19) / 32_000_000)
    Communicator.write_register(spi, Parameters.register().frf_msb, frt >>> 16)
    Communicator.write_register(spi, Parameters.register().frf_mid, frt >>> 8)
    Communicator.write_register(spi, Parameters.register().frf_lsb, frt >>> 0)
  end

  # def set_tx_power(spi, level, output_pin) when output_pin == Parameters.pa.output_rfo_pin do
  #   cond do
  #     level < 0 -> Communicator.write_register(spi, Parameters.register.pa_config, 0x70 ||| 0)
  #     level > 14 -> Communicator.write_register(spi, Parameters.register.pa_config, 0x70 ||| 14)
  #     level >= 0 -> Communicator.write_register(spi, Parameters.register.pa_config, 0x70 ||| level)
  #   end
  # end

  def set_tx_power(level, spi) do
    if level > 17 do
      Communicator.write_register(spi, Parameters.register().pa_dac, 0x87)
      set_ocp(140, spi)

      if level > 20,
        do:
          Communicator.write_register(
            spi,
            Parameters.register().pa_config,
            Parameters.pa().boost ||| 15
          ),
        else:
          Communicator.write_register(
            spi,
            Parameters.register().pa_config,
            Parameters.pa().boost ||| level - 5
          )
    else
      Communicator.write_register(spi, Parameters.register().pa_dac, 0x84)
      set_ocp(100, spi)

      if level < 2,
        do:
          Communicator.write_register(
            spi,
            Parameters.register().pa_config,
            Parameters.pa().boost ||| 0
          ),
        else:
          Communicator.write_register(
            spi,
            Parameters.register().pa_config,
            Parameters.pa().boost ||| level - 2
          )
    end
  end

  def set_ocp(ocp, spi) do
    cond do
      ocp <= 120 ->
        Communicator.write_register(
          spi,
          Parameters.register().ocp,
          0x20 ||| (0x1F &&& uint8((uint8(ocp) - 45) / 5))
        )

      ocp <= 240 ->
        Communicator.write_register(
          spi,
          Parameters.register().ocp,
          0x20 ||| (0x1F &&& uint8((uint8(ocp) + 30) / 10))
        )

      ocp > 240 ->
        Communicator.write_register(spi, Parameters.register().ocp, 0x20 ||| (0x1F &&& 27))
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
        Parameters.register().modem_config_3,
        bit_write(
          Communicator.read_register(spi, Parameters.register().modem_config_3),
          3,
          ldo_on
        )
      )
    end
  end

  def set_spreading_factor(sf, spi) do
    if sf == 6 do
      Communicator.write_register(spi, Parameters.register().detection_optimize, 0xC5)
      Communicator.write_register(spi, Parameters.register().detection_threshold, 0x0C)
    else
      Communicator.write_register(spi, Parameters.register().detection_optimize, 0xC3)
      Communicator.write_register(spi, Parameters.register().detection_threshold, 0x0A)
    end

    config2 = Communicator.read_register(spi, Parameters.register().modem_config_2)

    sf_ = (config2 &&& 0x0F) ||| (sf <<< 4 &&& 0xF0)
    Communicator.write_register(spi, Parameters.register().modem_config_2, sf_)

    set_ldo_flag(spi)
  end

  def set_bandwidth(sbw, spi) do
    reg = Communicator.read_register(spi, Parameters.register().modem_config_1)

    Parameters.bw_freqs()
    |> Enum.filter(fn {_i, f} -> sbw <= f end)
    |> List.first()
    |> set_bw(spi, reg)

    set_ldo_flag(spi)
  end

  defp set_bw({bw, _freq}, spi, reg) do
    Communicator.write_register(
      spi,
      Parameters.register().modem_config_1,
      (reg &&& 0x0F) ||| bw <<< 4
    )
  end

  def set_base_address(spi) do
    # Set base addresses
    Communicator.write_register(spi, Parameters.register().fifo_tx_base_addr, 0)
    Communicator.write_register(spi, Parameters.register().fifo_rx_base_addr, 0)
  end

  def set_LNA_boost(spi),
    do:
      Communicator.write_register(
        spi,
        Parameters.register().lna,
        Communicator.read_register(spi, Parameters.register().lna) ||| 0x03
      )

  def set_auto_AGC(spi),
    do: Communicator.write_register(spi, Parameters.register().modem_config_3, 0x04)

  def set_header_mode(expl, spi) do
    modem_config_1 = Communicator.read_register(spi, Parameters.register().modem_config_1)

    if expl,
      do:
        Communicator.write_register(
          spi,
          Parameters.register().modem_config_1,
          modem_config_1 &&& Parameters.header(expl)
        ),
      else:
        Communicator.write_register(
          spi,
          Parameters.register().modem_config_1,
          modem_config_1 ||| Parameters.header(expl)
        )

    reset_fifo_payload(spi)
  end

  def enable_crc(spi),
    do:
      Communicator.write_register(
        spi,
        Parameters.register().modem_config_2,
        Communicator.read_register(spi, Parameters.register().modem_config_2) ||| 0x04
      )

  def disable_crc(spi),
    do:
      Communicator.write_register(
        spi,
        Parameters.register().modem_config_2,
        Communicator.read_register(spi, Parameters.register().modem_config_2) ||| 0xFB
      )

  def get_signal_band_width(spi) do
    bw = Communicator.read_register(spi, Parameters.register().modem_config_1) >>> 4
    Parameters.bw_freqs()[bw]
  end

  def get_spreading_factor(spi) do
    config = Communicator.read_register(spi, Parameters.register().modem_config_2)

    config >>> 4
  end

  def get_version(spi), do: Communicator.read_register(spi, Parameters.register().version)

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

  # defp change_third_bit(value, bit), do: if(bit == 0, do: value &&& 0xF7, else: value ||| 8)
end
