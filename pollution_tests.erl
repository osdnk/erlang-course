%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2018 20:10
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("osdnk").
-include_lib("eunit/include/eunit.hrl").

-record(measurements, {all = sets:new(), type_date_station_to_meas = #{}, type_date_to_meas = #{}, type_station_to_meas = #{}}).
-record(stations, {all = sets:new(), coord_to_elem = #{}, name_to_elem = #{}}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).



reverse_nil_test() -> [] = lists:reverse([]).
reverse_one_test() -> [1] = lists:reverse([1]).
reverse_two_test() -> [2, 1] = lists:reverse([1, 2]).

length_test() -> ?assert(length([1, 2, 3]) =:= 3).

create_monitor_test() ->
  Current = pollution:create_monitor(),
  Expected = {
    monitor,
    {measurements,
      sets:new(),
      #{},
      dict:new(),
      dict:new()
    },
    {stations,
      sets:new(),
      #{},
      #{}
    }
  },
  ?assertEqual(Current, Expected).

add_station_test() ->
  Monitor = pollution:create_monitor(),
  Current = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  ?assert(sets:is_element({"Lodolamacz Moskwa", {14, 10}}, (Current#monitor.stats)#stations.all)),
  ?assert(maps:get("Lodolamacz Moskwa",
    (Current#monitor.stats)#stations.name_to_elem) =:= {"Lodolamacz Moskwa", {14, 10}}),
  ?assert(maps:get({14, 10},
    (Current#monitor.stats)#stations.coord_to_elem) =:= {"Lodolamacz Moskwa", {14, 10}}).

add_bad_station_test() ->
  Current = pollution:create_monitor(),
  ?assertNot(sets:is_element({"Lodolamacz Moskwa", {14, 10}}, (Current#monitor.stats)#stations.all)).


add_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  Current = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS),
  ?assert(sets:is_element({{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", element(1, D)},
    (Current#monitor.meas)#measurements.type_date_to_meas)) =:= [{{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}]).

add_value_bad_station_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  ?assertError(_, pollution:add_value({14, 10}, D, "X", 12, Monitor)).

remove_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  CurrentButNotRemoved = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS),
  Current = pollution:remove_value({14, 10}, D, "X", CurrentButNotRemoved),
  ?assertNot(sets:is_element({{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", D}, (Current#monitor.meas)#measurements.type_date_to_meas)) == []),
  ?assert(element(2, dict:find({"X", {"Lodolamacz Moskwa", {14, 10}}}, (Current#monitor.meas)#measurements.type_station_to_meas)) == []).

get_one_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  Current = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS),
  ?assert(pollution:get_one_value("X", D, {14, 10}, Current) =:= 12).

get_station_mean_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  Current = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS),
  Current2 = pollution:add_value({14, 10}, calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}), "X", 10, Current),
  ?assert(pollution:get_station_mean("X", "Lodolamacz Moskwa", Current2) == 11).


get_daily_mean_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  MonitorWithS2 = pollution:add_station("Lodolamacz Kijow", {19, 97}, MonitorWithS),
  Current = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS2),
  Current2 = pollution:add_value({19, 97}, D, "X", 10, Current),
  ?assert(pollution:get_daily_mean("X", element(1, D), Current2) == 11).

get_air_quality_index_test() ->
  D1 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 30, 12}}),
  D2 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  CurrentM = pollution:add_value({14, 10}, D1, "PM10", 12, MonitorWithS),
  CurrentM2 = pollution:add_value("Lodolamacz Moskwa", D2, "PM10", 10, CurrentM),
  Current = pollution:get_air_quality_index(D1, {14, 10}, CurrentM2),
  ?assert (Current == [0.24, 0]).
