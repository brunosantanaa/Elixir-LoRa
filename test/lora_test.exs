defmodule LoRaTest do
  use ExUnit.Case
  doctest LoRa

  test "LoRa Begin" do
    assert LoRa.start_link() == {:ok, _}
    assert LoRa.begin(433.0e6) == :ok
    assert LoRa.set_spreading_factor(10) == :ok
    assert LoRa.set_signal_band_width(62.5e3) == :ok
    assert LoRa.enable_crc() == :ok
  end
end
