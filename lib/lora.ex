defmodule LoRa do
  @moduledoc """
  This is a module for transmitter data using LoRa Radios.
  
  Radios:
      Semtech SX1276/77/78/79 based boards.
  """
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
  @lora_default_reset_pin 25
  # @lora_default_dio0_pin 22
  
  @doc """
  Start and link a new GenServer for LoRa radio.

  Can be set the parameters of SPI and GPIO pin for the reset of radio.
  
    # standard values: `spi: "spidev0.0", spi_speed: 8_000_000, rst: 25`
      {:ok, lora} = LoRa.start_link()
    or
      {:ok, lora} = LoRa.start_link([spi: "spidev0.1", spi_speed: 5_000_000, rst: 27])
  
  """
  def start_link(config \\ []), do: GenServer.start_link(__MODULE__, config ++ [owner: self()])
  @doc """
  Initialize the LoRa Radio and set your frequency work.
      {:ok, lora} = LoRa.start_link()
      LoRa.begin(lora, 433.0e6)
  """
  def begin(pid, frequency), do: GenServer.call(pid, {:begin, frequency})
  @doc """
  Set the LoRa Radio in sleep mode. 
      LoRa.sleep(lora_pid)
  """
  def sleep(pid), do: GenServer.cast(pid, :sleep)
  @doc """
  Awake the LoRa Radio.
      LoRa.awake(lora_pid)
  """
  def awake(pid), do: GenServer.cast(pid, :awake)
  @doc """
  Set Spreading Factor. `sf` is a value between 6 and 12. Standar value is 6.

      LoRa.set_spreading_factor(lora_pid, 10)
  """
  def set_spreading_factor(pid, sf \\ 6) when sf >= 6 and sf <= 12,
    do: GenServer.cast(pid, {:set_sf, sf})
  @doc """
  Set Trasmission Signal Bandwidth.

      LoRa.set_signal_bandwidth(lora_pid, 31.25e3)
  """
  def set_signal_bandwidth(pid, sbw), do: GenServer.cast(pid, {:set_sbw, sbw})
  @doc """
  This is a verify digit add in the data message.

      LoRa.enable_crc(lora_pid)
  """
  def enable_crc(pid), do: GenServer.cast(pid, :enable_crc)
  @doc """
  Remove the verify digit.

      LoRa.disable_crc(lora_pid)
  """
  def disable_crc(pid), do: GenServer.cast(pid, :disable_crc)
  @doc """
  Send data for other radios.

      LoRa.send(lora_pid, 'hello world')
      LoRa.send(lora_pid, "hello world")
      LoRa.send(lora_pid, %{value: 10})
  """
  def send(pid, data, header \\ true) do
    GenServer.cast(pid, :sender_mode)
    GenServer.cast(pid, {:send, data, header})
  end

  def init(config) do
    pin_reset = Keyword.get(config, :rst, @lora_default_reset_pin)
    device = Keyword.get(config, :spi, @lora_default_spi)
    speed_hz = Keyword.get(config, :spi_speed, @lora_default_spi_frequency)

    {:ok, spi} = start_spi(device, speed_hz)

    state = %{is_receiver?: true, owner: config[:owner], spi: nil, rst: nil, config: nil}

    Logger.info("LoRa: Start Device")
    {:ok, rst} = GPIO.start_link(pin_reset, :output)
    Modem.reset(rst)

    {:ok,
     %{
       state
       | spi: %{pid: spi, operator: %{device: device, speed_hz: speed_hz}},
         rst: rst,
         config: %{frequency: 0, header: true, packet_index: 0, on_receive: nil}
     }}
  end

  def handle_info(:receiver_mode, state) do
    if state.is_receiver? do
      Modem.parse_packet(self(), state.spi)
      Kernel.send(self(), :receiver_mode)
    end

    {:noreply, state}
  end

  def handle_info({:receive_msg, pkt_length}, state) do
    Logger.debug("LoRa: message recieved: #{pkt_length}Bytes")
    Modem.read(state.config.frequency, state.owner, state.spi)
    {:noreply, state}
  end

  def handle_info(:send_ok, state) do
    Logger.debug("LoRa: message sent")

    Modem.tx_done_flag(state.spi)

    {:noreply, state}
  end

  def handle_info(:send_error, state) do
    Logger.error("LoRa: Send Timeout")
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, _from, type}, state) do
    Logger.error("LoRa: send packet: Exit with #{type}")

    Modem.tx_done_flag(state.spi)

    {:noreply, state}
  end

  def handle_call({:begin, frequency}, _from, state) do
    version = Modem.get_version(state.spi)

    if version == 0x12 do
      Modem.begin(frequency, state.spi)
      Process.send_after(self(), :receiver_mode, 500)
      {:reply, :ok, %{state | :config => %{state[:config] | :frequency => frequency}}}
    else
      Logger.error("LoRa: Not a Valid Version")
      {:reply, {:error, :version}, state}
    end
  end

  def handle_cast(:sender_mode, state) do
    {:noreply, %{state | is_receiver?: false}}
  end

  def handle_cast({:send, data, header}, state) do
    Kernel.send(self(), :receiver_mode)
    Modem.idle(state.spi)
    GenServer.cast(__MODULE__, {:header_mode, header})
    data
    |> (fn(a) -> if is_bitstring(a), do: a, else: Kernel.inspect(a) end).()
    |> Communicator.print(state.spi)
    self() |> Modem.end_packet(state.spi)

    {:noreply, %{state | is_receiver?: true}}
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
    SPI.release(state.spi.pid)
    {:noreply, state}
  end

  def handle_cast(:awake, state) do
    operator = state.spi.operator

    {:ok, spi} = start_spi(operator.device, operator.speed_hz)
    new_spi = %{state[:spi] | pid: spi}

    Modem.idle(new_spi)
    {:noreply, %{state | :spi => new_spi}}
  end

  def handle_cast(:enable_crc, state) do
    Modem.enable_crc(state.spi)
    {:noreply, state}
  end

  def handle_cast(:disable_crc, state) do
    Modem.disable_crc(state.spi)
    {:noreply, state}
  end

  def handle_cast({:header_mode, value}, state) do
    Modem.set_header_mode(value, state.spi)
    {:noreply, %{state | :config => %{state[:config] | :header => value}}}
  end

  defp start_spi(device, speed_hz) do
    {:ok, spi} = SPI.start_link(device, speed_hz: speed_hz, mode: 0)
    {:ok, spi}
  end
end
