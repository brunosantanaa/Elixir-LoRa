defmodule LoRa.Encoding do
  @moduledoc """
  Handles the encoding of Elxir terms to binary packets, and the decoding of
  binary packet back into Elixir terms.

  ## Encodings

    - `:inspect` - encodes using `Kernel.inspect/2`, decodes to a string of that inspected value
    - `:binary` - encodes and decodes raw binaries, directly
    - `:term` - encodes and decodes terms using Erlang's [external term format](https://www.erlang.org/doc/apps/erts/erl_ext_dist.html)
  """

  @type encoding :: :inspect | :binary | :term

  @doc """
  Encodes a term as a binary for transmission of a packet.
  """
  @spec encode_data!(data :: term(), encoding :: encoding()) :: binary() | no_return()
  def encode_data!(data, :inspect), do: inspect(data)
  def encode_data!(data, :binary) when is_binary(data), do: data
  def encode_data!(data, :term), do: :erlang.term_to_binary(data)
  def encode_data!(_, _), do: raise(ArgumentError)

  @doc """
  Decodes a binary as a term upon reception of a packet.
  """
  @spec decode_data!(data :: binary(), encoding :: encoding()) :: term() | no_return()
  def decode_data!(data, :inspect) when is_binary(data), do: data
  def decode_data!(data, :binary) when is_binary(data), do: data
  def decode_data!(data, :term) when is_binary(data), do: :erlang.binary_to_term(data)
  def decode_data!(_, _), do: raise(ArgumentError)
end
