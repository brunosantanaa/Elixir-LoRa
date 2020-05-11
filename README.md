# LoRa
![Hex.pm](https://img.shields.io/hexpm/v/lora)

This is a module for transmitter data using LoRa Radios.

Available for Nerves Project with Semtech SX1276/77/78/79 based boards.

## Getting started

### Hardware 
Example using **Raspberry Pi** and **Adafruit RFM9xx Radio**

![Lora](assets/rasp_loraada.png)

### Installation

The package can be installed in your [Nerves Project](https://www.nerves-project.org/) by adding `LoRa` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:lora, "~> 1.0"}
  ]
end
```

### Configure SPI

Can be set the parameters of SPI and GPIO pin for the reset of radio.
  
standard values: `spi: "spidev0.0", spi_speed: 8_000_000, rst: 25`

```elixir
{:ok, lora_pid} = LoRa.start_link([spi: "spidev0.1", rst: 27])
```

### Configure Radio

```elixir
iex> LoRa.begin(lora_pid, 433.0e6)
:ok
iex> LoRa.set_spreading_factor(lora_pid, 8)
:ok
iex> LoRa.set_bandwidth(lora_pid, 62.5e3)
:ok
```

### Sending data

```elixir
iex> LoRa.send(lora_pid, [value: 10])
```

### Receiveing data

When the radio receives some data it sends a message to the process that started the link.

```elixir
iex> flush()
{:lora,
 %{
   packet: "hello world",
   rssi: -90,
   snr: 6,
   time: ~U[2020-05-06 21:13:11.161125Z]
 }}
:ok
```