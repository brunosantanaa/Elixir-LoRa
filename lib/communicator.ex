defmodule LoRa.Communicator do
  use Bitwise

  alias ElixirALE.SPI

  alias LoRa.Parameters

  def print(text, spi) do
    current_length = read_register(spi, Parameters.register().payload_length)
    bytelist = text |> String.to_charlist()

    if current_length + length(bytelist) < Parameters.max().pkt_length,
      do: write(spi, bytelist, length(bytelist)),
      else: write(spi, bytelist, Parameters.max().pkt_length - current_length)
  end

  def write(spi, bytelist, size) do
    for i <- 0..(size - 1) do
      :timer.sleep(1)
      write_register(spi, Parameters.register().fifo, Enum.at(bytelist, i))
    end

    write_register(spi, Parameters.register().payload_length, size)
  end

  def read_register(spi, address) do
    single_transfer(spi, address &&& 0x7F, 0x00)
  end

  def write_register(spi, address, value) do
    single_transfer(spi, address ||| 0x80, value)
  end

  def single_transfer(spi, address, value) do
    <<_, resp>> = SPI.transfer(spi.pid, <<address, value>>)
    resp
  end
end
