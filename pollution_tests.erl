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
reverse_two_test() -> [2,1] = lists:reverse([1,2]).

length_test() -> ?assert(length([1,2,3]) =:= 3).

create_monitor_test() ->
  Current = pollution:create_monitor(),
  Expected = {
    monitor,
    {measurements,
      sets:new(),
      #{},
      #{},
      #{}
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

add_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:create_monitor(),
  MonitorWithS = pollution:add_station("Lodolamacz Moskwa", {14, 10}, Monitor),
  Current = pollution:add_value({14, 10}, D, "X", 12, MonitorWithS),
  ?assert(sets:is_element({{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)).
