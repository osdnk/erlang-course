%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2018 10:23
%%%-------------------------------------------------------------------
-module(pollution_server).
-export([
  init/0,
  stop/0,
  crash/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_air_quality_index/2,
  start/0
]).

%% API start


stop() ->
  handler(stop, []).

add_station(Name, Station) ->
  handler(add_station, [Name, Station]).

add_value(Station, Date, Type, Value) ->
  handler(add_value, [Station, Date, Type, Value]).

remove_value(Station, Date, Type) ->
  handler(remove_value, [Station, Date, Type]).

get_one_value(Type, Date, Station) ->
  handler(get_one_value, [Type, Date, Station]).

get_station_mean(Type, Station) ->
  handler(get_station_mean, [Type, Station]).

get_daily_mean(Type, Day) ->
  handler(get_daily_mean, [Type, Day]).

get_air_quality_index(Date, Station) ->
  handler(get_air_quality_index, [Date, Station]).

crash() ->
  %% io:format("pollution_server:crash()~n"),
  handler(crash, []).

%% API end

start() ->
  register(server, spawn(fun () -> init() end)),
  server.

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

handler(RequestType, Args) when is_list(Args) ->
  server ! {RequestType, self(), Args},
  receive
    Message -> Message
  after
    1000 -> {error, timeeout}
  end.

response(Pid, Monitor) ->
  Pid ! {ok, Monitor}.

loop(Monitor) ->
  receive
    {add_station, Pid, [Name, Station]} ->
      NewMonitor = pollution:add_station(Name, Station, Monitor),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {add_value, Pid, [Station, Date, Type, Value]} ->
      NewMonitor = pollution:add_value(Station, Date, Type, Value, Monitor),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {remove_value, Pid, [Station, Date, Type]} ->
      NewMonitor = pollution:remove_value(Station, Date, Type, Monitor),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {get_one_value, Pid, [Type, Date, Station]} ->
      Value = pollution:get_one_value(Type, Date, Station, Monitor),
      response(Pid, Value),
      loop(Monitor);

    {get_station_mean, Pid, [Type, Station]} ->
      Value = pollution:get_station_mean(Type, Station, Monitor),
      response(Pid, Value),
      loop(Monitor);

    {get_daily_mean, Pid, [Type, Day]} ->
      Value = pollution:get_daily_mean(Type, Day, Monitor),
      response(Pid, Value),
      loop(Monitor);

    {get_air_quality_index, Pid, [Date, Station]} ->
      Value = pollution:get_air_quality_index(Date, Station, Monitor),
      response(Pid, Value),
      loop(Monitor);

    {stop, Pid, []} ->
      response(Pid, Monitor),
      terminate()

%    {crash, Pid, []} ->
    %response(Pid, 1 / 0)
  end.

terminate() ->
  ok.
