defmodule LoRa.Crypto do
  @block_size 16

  def aes128_encrypt(key, data) do
    to_add = @block_size - rem(byte_size(data), @block_size)
    data_resize = data <> to_string(:string.chars(to_add, to_add))

    :crypto.block_encrypt(
      :aes_128_ecb,
      key,
      to_string(:string.chars(0, @block_size)),
      data_resize
    )
  end

  def aes128_decrypt(key, data) do
    decrypt =
      :crypto.block_decrypt(:aes_128_ecb, key, to_string(:string.chars(0, @block_size)), data)

    to_remove = :binary.last(decrypt)
    :binary.part(decrypt, 0, byte_size(decrypt) - to_remove)
  end
end
