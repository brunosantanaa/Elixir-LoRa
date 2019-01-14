LoRa.start_link()
LoRa.begin(433.0e6)
LoRa.set_spreading_factor(10)
LoRa.set_signal_band_width(62.5e3)
LoRa.enable_crc()

LoRa.begin_packet()
LoRa.print("E")
LoRa.end_packet()

defmodule Teste do
  def counter(state \\ 0) do
    if state < 20 do
      LoRa.begin_packet()
      IO.puts("Elixir sender -> #{state}")
      LoRa.print("Elixir sender -> #{state}")
      LoRa.end_packet()
      :timer.sleep(2_000)
      counter(state + 1)
    end
  end
end

Teste.counter()
