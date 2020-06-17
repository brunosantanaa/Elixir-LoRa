defmodule LoRa.Region do
  def info(:au915) do
    %{
      sf: [up: Enum.to_list(7..10), dn: Enum.to_list(7..12)],
      bw: [
        up: [125.0e3, 500.0e3],
        dn: [500.0e3]
      ],
      freq_base: [
        up: [915.2e6, 915.9e6],
        dn: [923.3e6]
      ],
      steps: [
        up: [0.2e6, 1.6e6],
        dn: [0.6e6]
      ],
      limits: [915.0e6, 928.0e6]
    }
  end

  def info(:us915) do
    %{
      sf: [up: Enum.to_list(7..10), dn: Enum.to_list(7..12)],
      bw: [
        up: [125.0e3, 500.0e3],
        dn: [500.0e3]
      ],
      freq_base: [
        up: [902.3e6, 903.0e6],
        dn: [923.3e6]
      ],
      steps: [
        up: [0.2e6, 1.6e6],
        dn: [0.6e6]
      ],
      limits: [902.0e6, 928.0e6]
    }
  end

  def info(:eu868) do
    bw_base = List.duplicate(125.0e3, 8)
    freq_base = [868.1e6, 868.3e6, 868.5e6, 868.8e6, 869.5e6, 864.1e6, 864.3e6, 864.5e6]

    %{
      sf: [up: Enum.to_list(7..12), dn: Enum.to_list(7..12)],
      bw: [
        up: bw_base,
        dn: bw_base ++ [125.0e3]
      ],
      freq_base: [
        up: freq_base,
        dn: freq_base ++ [869.0e6]
      ],
      steps: [
        up: List.duplicate(870.0e6, 8),
        dn: List.duplicate(870.0e6, 9)
      ],
      limits: [863.0e6, 870.0e6]
    }
  end

  def plan(region, link, freqs \\ [], stp \\ [], bw_ \\ [], state \\ []) do
    inf = info(region)

    freq_max = inf.limits |> Enum.max()
    sf = inf.sf[link]

    cond do
      length(freqs) > 0 ->
        [base | base_tail] = freqs
        [step | step_tail] = stp
        [bw | bw_tail] = bw_

        n_val = ((freq_max - base) / step) |> round()
        
        if n_val > 1 do
          f_bw = Enum.map(1..n_val, fn x -> {base + step * (x - 1), bw} end)

          new =
            Enum.map(f_bw, fn {freq, bw} ->
              Enum.map(sf, fn sf_ -> %{freq: freq, bw: bw, sf: sf_} end)
            end)

          plan(region, link, base_tail, step_tail, bw_tail, state ++ new)
        else
          new_ = Enum.map(sf, fn sf_ -> %{freq: base, bw: bw, sf: sf_} end)
          plan(region, link, base_tail, step_tail, bw_tail, state ++ new_)
        end

      length(state) == 0 ->
        plan(region, link, inf.freq_base[link], inf.steps[link], inf.bw[link], [])

      length(freqs) == 0 && length(state) > 0 ->
        state
    end
  end
end
