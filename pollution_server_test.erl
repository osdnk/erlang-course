%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2018 10:52
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("osdnk").
-include_lib("eunit/include/eunit.hrl").


-record(measurements, {all = sets:new(), type_date_station_to_meas = #{}, type_date_to_meas = #{}, type_station_to_meas = #{}}).
-record(stations, {all = sets:new(), coord_to_elem = #{}, name_to_elem = #{}}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).



add_value_test() ->
  D = calendar:local_time(),
  pollution_server:start_link(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D, "X", 12),
  {ok, Current} = pollution_server:stop(),
  ?assert(sets:is_element({{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", element(1, D)},
    (Current#monitor.meas)#measurements.type_date_to_meas)) =:= [{{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}]).

add_station_test() ->
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  {ok, Current} = pollution_server:stop(),
  ?assert(sets:is_element({"Lodolamacz Moskwa", {14, 10}}, (Current#monitor.stats)#stations.all)),
  ?assert(maps:get("Lodolamacz Moskwa",
    (Current#monitor.stats)#stations.name_to_elem) =:= {"Lodolamacz Moskwa", {14, 10}}),
  ?assert(maps:get({14, 10},
    (Current#monitor.stats)#stations.coord_to_elem) =:= {"Lodolamacz Moskwa", {14, 10}}).

add_bad_station_test() ->
  pollution_server:start(),
  {ok, Current} = pollution_server:stop(),
  ?assertNot(sets:is_element({"Lodolamacz Moskwa", {14, 10}}, (Current#monitor.stats)#stations.all)).

remove_value_test() ->
  D = calendar:local_time(),
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D, "X", 12),
  pollution_server:remove_value({14, 10}, D, "X"),
  {ok, Current} = pollution_server:stop(),
  ?assertNot(sets:is_element({{"Lodolamacz Moskwa", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", D}, (Current#monitor.meas)#measurements.type_date_to_meas)) == []),
  ?assert(element(2, dict:find({"X", {"Lodolamacz Moskwa", {14, 10}}}, (Current#monitor.meas)#measurements.type_station_to_meas)) == []).

get_one_value_test() ->
  D = calendar:local_time(),
  pollution_server:start(),
   pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D, "X", 12),
  {ok, C} = pollution_server:get_one_value("X", D, {14, 10}),
  pollution_server:stop(),
  ?assert(C =:= 12).

get_station_mean_test() ->
  D = calendar:local_time(),
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D, "X", 12),
  pollution_server:add_value({14, 10}, calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}), "X", 10),
  {ok, C} =pollution_server:get_station_mean("X", "Lodolamacz Moskwa"),
  pollution_server:stop(),
  ?assert(C == 11).


get_daily_mean_test() ->
  D = calendar:local_time(),
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_station("Lodolamacz Kijow", {19, 97}),
  pollution_server:add_value({14, 10}, D, "X", 12),
  pollution_server:add_value({19, 97}, D, "X", 10),
  {ok, C} = pollution_server:get_daily_mean("X", element(1, D)),
  pollution_server:stop(),
  ?assert(C == 11).

get_air_quality_index_test() ->
  D1 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 30, 12}}),
  D2 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}),
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D1, "PM10", 12),
  pollution_server:add_value("Lodolamacz Moskwa", D2, "PM10", 10),
  {ok, Current} = pollution_server:get_air_quality_index(D1, {14, 10}),
  pollution_server:stop(),
  ?assertEqual (Current, [0.24, 0]).

sequence_test() ->
  D1 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 30, 12}}),
  D2 =  calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}),
  pollution_server:start(),
  pollution_server:add_station("Lodolamacz Moskwa", {14, 10}),
  pollution_server:add_value({14, 10}, D1, "PM10", 12),
  pollution_server:get_daily_mean("PM10", element(1, D1)),
  pollution_server:get_station_mean("PM10", "Lodolamacz Moskwa"),
  pollution_server:get_air_quality_index(D1, {14, 10}),
  pollution_server:stop(),
  ?assert(true).

