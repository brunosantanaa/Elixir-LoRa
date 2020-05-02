defmodule LoRaTest do
  use ExUnit.Case
  doctest LoRa

  test "LoRa Begin" do
    LoRa.start_link()
    assert LoRa.begin(433.0e6) == :ok
    assert LoRa.set_spreading_factor(10) == :ok
    assert LoRa.set_signal_band_width(62.5e3) == :ok
    assert LoRa.enable_crc() == :ok
  end

  test "Transmitting" do
    LoRa.begin_packet()
    LoRa.print("0")
    LoRa.end_packet()
    LoRa.begin_packet()
    LoRa.print("1 - Hello World")
    LoRa.end_packet()
    LoRa.begin_packet()
    LoRa.print("2 - Baltar é o terror")
    LoRa.end_packet()
    LoRa.begin_packet()
    LoRa.print("3 - ;)")
    LoRa.end_packet()
  end
end
