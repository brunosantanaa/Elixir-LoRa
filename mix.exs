defmodule LoRa.MixProject do
  use Mix.Project

  def project do
    [
      app: :lora,
      version: "1.0.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      name: "LoRa",
      source_url: "https://github.com/brunosantanaa/Elixir-LoRa",
      docs: [
        main: "LoRa",
        logo: "assets/logo.png",
        extras: ["README.md"]
      ]
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README*"],
      maintainers: ["Bruno Santana"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/brunosantanaa/Elixir-LoRa"}
    ]
  end

  defp description do
    """
    This is a module for transmitter data using LoRa Radios.
  
    Radios:
        Semtech SX1276/77/78/79 based boards.
    """
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elixir_ale, "~> 1.2"},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
    ]
  end
end
