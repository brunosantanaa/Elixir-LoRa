defmodule LoRaTest do
  use ExUnit.Case
  doctest LoRa

  test "greets the world" do
    assert LoRa.hello() == :world
  end
end
