%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <OSDNK>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2018 00:16
%%%-------------------------------------------------------------------
-module(pollution).
-author("osdnk").
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).

-record(measurements, {all = sets:new(),
  type_date_station_to_meas = #{},
  type_date_to_meas = dict:new(),
  type_station_to_meas = dict:new()}).
-record(stations, {all = sets:new(), coord_to_elem = #{}, name_to_elem = #{}}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).

create_monitor() ->
  #monitor{}.

parse_name_or_coords_to_station({X, Y}, Monitor) -> maps:get({X, Y}, (Monitor#monitor.stats)#stations.coord_to_elem);
parse_name_or_coords_to_station(Name, Monitor) -> maps:get(Name, (Monitor#monitor.stats)#stations.name_to_elem).

add_station(Name, {X, Y}, Monitor) ->
  S = {Name, {X, Y}},
  NewAllStations = sets:add_element(S, (Monitor#monitor.stats)#stations.all),
  NewCoordsToElem = maps:put({X, Y}, S, (Monitor#monitor.stats)#stations.coord_to_elem),
  NewNamesToElem = maps:put(Name, S, (Monitor#monitor.stats)#stations.name_to_elem),
  Monitor#monitor{stats = (Monitor#monitor.stats)#stations{
    all = NewAllStations,
    coord_to_elem = NewCoordsToElem,
    name_to_elem = NewNamesToElem
  }}.

add_value(Station, Date, Type, Value, Monitor) ->
  S = parse_name_or_coords_to_station(Station, Monitor),
  M = {S, Date, Type, Value},
  NewAllMeas = sets:add_element(M, (Monitor#monitor.meas)#measurements.all),
  NewTypeDateToMeas = dict:append({Type, element(1, Date)}, M, (Monitor#monitor.meas)#measurements.type_date_to_meas),
  NewTypeStationToMeas = dict:append({Type, S}, M, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  NewTypeDateStationToMeas = maps:put({Type, Date, S}, M, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  Monitor#monitor{meas = (Monitor#monitor.meas)#measurements{
    all = NewAllMeas,
    type_date_to_meas = NewTypeDateToMeas,
    type_station_to_meas = NewTypeStationToMeas,
    type_date_station_to_meas = NewTypeDateStationToMeas
  }}.

remove_value(Station, Date, Type, Monitor) ->
  S = parse_name_or_coords_to_station(Station, Monitor),
  M = maps:get({Type, Date, S}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  NewAllMeas = sets:del_element(M, (Monitor#monitor.meas)#measurements.all),
  NewTypeDateStationToMeas = maps:remove({Type, Date, S}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  {Prev1, TypeDateToMeas} = dict:take({Type, element(1, Date)}, (Monitor#monitor.meas)#measurements.type_date_to_meas),
  NewTypeDateToMeas = dict:append_list({Type, Date}, lists:filter(fun(X) -> X =/= M end, Prev1), TypeDateToMeas),
  {Prev2, TypeStationToMeas} = dict:take({Type, S}, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  NewTypeStationToMeas = dict:append_list({Type, S}, lists:filter(fun(X) -> X =/= M end, Prev2), TypeStationToMeas),
  Monitor#monitor{meas = (Monitor#monitor.meas)#measurements{
    all = NewAllMeas,
    type_date_to_meas = NewTypeDateToMeas,
    type_station_to_meas = NewTypeStationToMeas,
    type_date_station_to_meas = NewTypeDateStationToMeas
  }}.

get_one_value(Type, Date, Station, Monitor) ->
  S = parse_name_or_coords_to_station(Station, Monitor),
  {_, _, _, V} = maps:get({Type, Date, S}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  V.

avg(L) ->
  sum(L, 0) / length(L).
sum([H|T], Acc) ->
  sum(T, element(4, H) + Acc);
sum([], Acc) ->
  Acc.

get_station_mean(Type, Station, Monitor) ->
  S = parse_name_or_coords_to_station(Station, Monitor),
  {ok, L} = dict:find({Type, S}, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  case is_list(L) of
    true -> avg(L);
    false -> 0
  end.

get_daily_mean(Type, Day, Monitor) ->
  {ok, L} = dict:find({Type, Day}, (Monitor#monitor.meas)#measurements.type_date_to_meas),
  case is_list(L) of
    true -> avg(L);
    false -> 0
  end.




%% API
