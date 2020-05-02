defmodule LoRa.Communicator do

  use Bitwise
  alias ElixirALE.GPIO
  alias ElixirALE.SPI

  @reg_fifo 0x00
  @reg_payload_length 0x22

  def write(spi, bytelist, size) do
    for i <- 0..(size - 1) do
      :timer.sleep(1)
      write_register(spi, @reg_fifo, Enum.at(bytelist, i))
    end

    write_register(spi, @reg_payload_length, size)
  end

  def read_register(spi, address) do
    single_transfer(spi, address &&& 0x7F, 0x00)
  end

  def write_register(spi, address, value) do
    single_transfer(spi, address ||| 0x80, value)
  end

  def single_transfer(spi, address, value) do
    GPIO.write(spi.ss, 0)
    <<_, resp>> = SPI.transfer(spi.pid, <<address, value>>)
    GPIO.write(spi.ss, 1)
    resp
  end

end