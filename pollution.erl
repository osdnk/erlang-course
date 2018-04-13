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
-export([create_monitor/0, add_station/3, get_all/1]).

-record(measurements, {all = sets:new(), type_date_station_to_meas = #{}, type_date_to_meas = #{}, type_station_to_meas = #{}}).
-record(stations, {all = sets:new(), coord_to_elem = #{}, name_to_elem = #{}}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).




create_monitor () ->
  #monitor{}.

%parse_date_or_coords_to_station ({X, Y}, Monitor) -> Monitor.stats.coors_to_ele()

add_station(Name, {X, Y}, Monitor) ->
  S = {Name, {X, Y}},
  NewAllStations = sets:add_element(S, (Monitor#monitor.stats)#stations.all),
  NewCoordsToElem = maps:put({X, Y}, S, (Monitor#monitor.stats)#stations.coord_to_elem),
  NewNamesToElem = maps:put(Name, S, (Monitor#monitor.stats)#stations.name_to_elem),
  Monitor#monitor{ stats = (Monitor#monitor.stats)#stations{
    all = NewAllStations,
    coord_to_elem = NewCoordsToElem,
    name_to_elem = NewNamesToElem
  } }.




get_all (M) ->
  (M#monitor.stats)#stations.all.

add_value({X, Y}, Date, Type, Value, Monitor) -> Monitor. %TODO
inner_add_value(Station, Date, Type, Value, Monitor) -> Monitor. %TODO
remove_value({X, Y}, Date, Type, Monitor) -> Monitor. %TODO
inner_remove_value(Station, Date, Type, Monitor) -> Monitor. %TODO

inner_get_one_value(Type, Date, Station, Monitor) -> Monitor. %TODO
%get_one_value(Type, Date, L) -> inner_get_one_value(Type, Date, parse_date_or_coords_to_station()).

get_station_mean(Type, Date, Monitor) -> Monitor. %TODO
get_daily_meann(Type, Date, Monitor) -> Monitor. %TODO



%% API
