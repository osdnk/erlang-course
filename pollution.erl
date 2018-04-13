%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2018 00:16
%%%-------------------------------------------------------------------
-module(pollution).
-author("osdnk").
-export([create_monitor/0, add_station/3, add_value/5]).

-record(measurements, {all = sets:new(), type_date_station_to_meas = #{}, type_date_to_meas = #{}, type_station_to_meas = #{}}).
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
  NewTypeDateToMeas = maps:put({Type, Date}, M, (Monitor#monitor.meas)#measurements.type_date_to_meas),
  NewTypeStationToMeas = maps:put({Type, S}, M, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  NewTypeDateStationToMeas = maps:put({Type, S}, M, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  Monitor#monitor{meas = (Monitor#monitor.meas)#measurements{
    all = NewAllMeas,
    type_date_to_meas = NewTypeDateToMeas,
    type_station_to_meas = NewTypeStationToMeas,
    type_date_station_to_meas = NewTypeDateStationToMeas
  }}.

remove_value({X, Y}, Date, Type, Monitor) -> Monitor.

inner_remove_value(Station, Date, Type, Monitor) -> Monitor. %TODO

inner_get_one_value(Type, Date, Station, Monitor) -> Monitor. %TODO
%get_one_value(Type, Date, L) -> inner_get_one_value(Type, Date, parse_date_or_coords_to_station()).

get_station_mean(Type, Date, Monitor) -> Monitor. %TODO
get_daily_meann(Type, Date, Monitor) -> Monitor. %TODO


%% API
