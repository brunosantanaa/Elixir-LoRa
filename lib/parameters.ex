defmodule LoRa.Parameters do
  # REG

  @reg %{
    fifo: 0x00,
    op_mode: 0x01,
    frf_msb: 0x06,
    frf_mid: 0x07,
    frf_lsb: 0x08,
    pa_config: 0x09,
    ocp: 0x0B,
    lna: 0x0C,
    fifo_addr_ptr: 0x0D,
    fifo_tx_base_addr: 0x0E,
    fifo_rx_base_addr: 0x0F,
    # fifo_rx_current_addr: 0x10,
    irq_flags: 0x12,
    # rx_nb_bytes: 0x13,
    # pkt_snr_value: 0x19,
    # pkt_rssi_value: 0x1A,
    modem_config_1: 0x1D,
    modem_config_2: 0x1E,
    # preamble_msb: 0x20,
    # preamble_lsb: 0x21,
    payload_length: 0x22,
    modem_config_3: 0x26,
    # freq_error_msb: 0x28,
    # freq_error_mid: 0x29,
    # freq_error_lsb: 0x2A,
    # rssi_wideband: 0x2C,
    detection_optimize: 0x31,
    # invertiq: 0x33,
    detection_threshold: 0x37,
    # sync_word: 0x39,
    # invertiq2: 0x3B,
    # dio_mapping_1: 0x40,
    version: 0x42,
    pa_dac: 0x4D
  }

  # modes
  @mode %{
    long_range_mode: 0x80,
    sleep: 0x00,
    stdby: 0x01,
    tx: 0x03,
    rx_continuous: 0x05,
    rx_single: 0x06
  }

  # pa config
  @pa %{
    boost: 0x80,
    output_rfo_pin: 0
  }

  # irq masks
  @irq %{
    tx_done_mask: 0x08,
    payload_crc_error_mask: 0x20,
    rx_done_mask: 0x40
  }

  @max %{
    pkt_length: 255,
    end_packet_cycles: 10_000
  }

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

  def register, do: @reg
  def mode, do: @mode
  def pa, do: @pa
  def irq, do: @irq
  def bw_freqs, do: @bw_freqs
  def max, do: @max
  def header(set), do: if(set, do: 0xFE, else: 0x01)
end
