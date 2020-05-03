defmodule LoRa do
  use Bitwise
  use GenServer

  alias ElixirALE.GPIO
  alias ElixirALE.SPI

  alias LoRa.Modem
  alias LoRa.Communicator

  require Logger

  # SPI
  @lora_default_spi "spidev0.0"
  @lora_default_spi_frequency 8_000_000
  @lora_default_ss_pin 24
  @lora_default_reset_pin 25

  # @lora_default_dio0_pin 22

  def start_link(config \\ []), do: GenServer.start_link(__MODULE__, config, name: __MODULE__)
  def begin(frequency), do: GenServer.call(__MODULE__, {:begin, frequency})
  def sleep(), do: GenServer.cast(__MODULE__, :sleep)
  def awake(), do: GenServer.cast(__MODULE__, :awake)

  def set_spreading_factor(sf \\ 6) when sf >= 6 and sf <= 12,
    do: GenServer.cast(__MODULE__, {:set_sf, sf})

  def set_signal_band_width(sbw), do: GenServer.cast(__MODULE__, {:set_sbw, sbw})
  def enable_crc(), do: GenServer.cast(__MODULE__, :enable_crc)
  def disable_crc(), do: GenServer.cast(__MODULE__, :disable_crc)

  def send(msg, header \\ true), do: GenServer.cast(__MODULE__, {:send, msg, header})

  def begin_packet, do: GenServer.cast(__MODULE__, :begin_packet)
  def end_packet, do: GenServer.cast(__MODULE__, :end_packet)
  def print(text), do: GenServer.cast(__MODULE__, {:print, text})

  def init(config) do
    pin_ss = Keyword.get(config, :ss, @lora_default_ss_pin)
    pin_reset = Keyword.get(config, :rst, @lora_default_reset_pin)
    device = Keyword.get(config, :spi, @lora_default_spi)
    speed_hz = Keyword.get(config, :spi_speed, @lora_default_spi_frequency)

    {:ok, ss} = GPIO.start_link(pin_ss, :output)
    GPIO.write(ss, 1)

    {:ok, spi} = SPI.start_link(device, speed_hz: speed_hz, mode: 0)

    state = %{spi: nil, rst: nil, config: nil}

    Logger.info("LoRa: Start Device")
    {:ok, rst} = GPIO.start_link(pin_reset, :output)
    Modem.reset(rst)

    {:ok,
     %{
       state
       | spi: %{pid: spi, ss: ss},
         rst: rst,
         config: %{frequency: 0, header: true, packet_index: 0, on_receive: nil}
     }}
  end

  def handle_info(:send_ok, state) do
    Logger.debug("LoRa: message sent")

    Modem.reset(state.rst)
    Modem.begin(state.config.frequency, state.spi)

    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, _from, type}, state) do
    Logger.debug("LoRa: send packet - Exit with #{type}")
    {:noreply, state}
  end

  def handle_info(:send_error, state) do
    Logger.error("LoRa: Send Timeout")
    {:noreply, state}
  end

  def handle_call({:begin, frequency}, _from, state) do
    version = Modem.get_version(state.spi)

    if version == 0x12 do
      Modem.begin(frequency, state.spi)
      {:reply, :ok, %{state | :config => %{state[:config] | :frequency => frequency}}}
    else
      Logger.error("LoRa: Not a Valid Version")
      {:reply, {:error, :version}, state}
    end
  end

  def handle_cast({:send, text, header}, state) do
    Modem.idle(state.spi)
    GenServer.cast(__MODULE__, {:header_mode, header})
    Communicator.print(text, state.spi)
    self() |> Modem.end_packet(state.spi)

    {:noreply, state}
  end

  def handle_cast({:set_sf, sf}, state) do
    Modem.set_spreading_factor(sf, state.spi)
    {:noreply, state}
  end

  def handle_cast({:set_sbw, sbw}, state) do
    Modem.set_bandwidth(sbw, state.spi)
    {:noreply, state}
  end

  def handle_cast(:sleep, state) do
    # Put in sleep mode
    Modem.sleep(state.spi)
    SPI.release(state.spi)
    {:noreply, state}
  end

  def handle_cast(:awake, state) do
    {:noreply, state}
  end

  def handle_cast(:enable_crc, state) do
    Modem.enable_crc(state.spi)
    {:noreply, state}
  end

  def handle_cast(:disable_crc, state) do
    Modem.disable_crc(state.spi)
    {:noreply, state}
  end

  def handle_cast({:header_mode, false}, state) do
    Modem.set_header_mode(state.spi, false)
    {:noreply, %{state | :config => %{state[:config] | :header => false}}}
  end

  def handle_cast({:header_mode, true}, state) do
    Modem.set_header_mode(state.spi, true)
    {:noreply, %{state | :config => %{state[:config] | :header => true}}}
  end
end
