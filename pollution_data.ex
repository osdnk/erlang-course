defmodule PollutionData do
  @moduledoc false
  def importLinesFromCSV(file) do
    fo = File.read!(file) |> String.split("\r\n")
    meas = Enum.map(fo, fn(a) -> parseLine(a) end)
    identifyStation(meas)
    |> addS
   # addM(meas)

  end

  defp addM(list) do
    Enum.map(list, fn m -> :pollution_server.add_value(m.location, m.datetime, "PM10", m.level) end)
  end

  defp addS(list) do
    Enum.map(list, fn  x -> :pollution_server.add_station(x.n, x.l) end)
  end
  defp parseLine(line) do
    [date, time, x, y, level] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse
           |> Enum.map(& Integer.parse /1 )
           |> Enum.map(fn(a) -> elem(a, 0) end )
           |> :erlang.list_to_tuple
    time = String.split(time, ":")
           |> Enum.map(& Integer.parse /1 )
           |> Enum.map(fn(a) -> elem(a, 0) end )
           |> :erlang.list_to_tuple
    x = elem(Float.parse(x), 0)
    y = elem(Float.parse(y), 0)
    level = elem(Integer.parse(level), 0)
    %{:datetime => {date, time},
      :location => {x, y},
      :level => level}
  end
  defp identifyStation(list) do
    Enum.map(list, fn  d -> d.location end)
    |> Enum.map(fn  l -> %{l: l, n: "station_#{elem(l, 0)}_#{elem(l, 1)}"} end)
    |> Enum.uniq
  end


end