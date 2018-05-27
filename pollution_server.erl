%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2018 10:23
%%%-------------------------------------------------------------------
-module(pollution_server).
-behaviour(gen_server).
-export([
  init/1,
  stop/0,
  crash/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_air_quality_index/2,
  start_link/0,
  handle_cast/2,
  handle_call/3
]).

%% API start

handle_cast(_, Value) ->
  {noreply, Value}.

handle_call(Req, _From, State) ->
  {Value, State} = get_new_state(Req, State),
  {reply, Value, State}.

stop() ->
  gen_server:call(server,  {stop}).

add_station(Name, Station) ->
  gen_server:call(server, {add_station, Name, Station}).

add_value(Station, Date, Type, Value) ->
  gen_server:call(server, {add_value, Station, Date, Type, Value}).

remove_value(Station, Date, Type) ->
  gen_server:call(server, {remove_value, Station, Date, Type}).

get_one_value(Type, Date, Station) ->
  gen_server:call(server, {get_one_value, Type, Date, Station}).

get_station_mean(Type, Station) ->
  gen_server:call(server, {get_station_mean, Type, Station}).

get_daily_mean(Type, Day) ->
  gen_server:call(server, {get_daily_mean, Type, Day}).

get_air_quality_index(Date, Station) ->
  gen_server:call(server, {get_air_quality_index, Date, Station}).

crash() ->
  io:format("server:crash()~n"),
  gen_server:call(server, {crash}).

%% API end


start_link() ->
  gen_server:start_link({local, server}, pollution_server, [], []).

init(_) ->
  {ok, pollution:create_monitor()}.


get_new_state(Req, Monitor) ->
  case Req of
    {add_station, Name, Station} ->
      NewMonitor = pollution:add_station(Name, Station, Monitor),
      {novalue, NewMonitor};

    {add_value, Station, Date, Type, Value} ->
      NewMonitor = pollution:add_value(Station, Date, Type, Value, Monitor),
      {novalue, NewMonitor};

    {remove_value, Station, Date, Type} ->
      NewMonitor = pollution:remove_value(Station, Date, Type, Monitor),
      {novalue, NewMonitor};

    {get_one_value, Type, Date, Station} ->
      Value = pollution:get_one_value(Type, Date, Station, Monitor),
      {Value, Monitor};

    {get_station_mean, Type, Station} ->
      Value = pollution:get_station_mean(Type, Station, Monitor),
      {Value, Monitor};

    {get_daily_mean, Type, Day} ->
      Value = pollution:get_daily_mean(Type, Day, Monitor),
      {Value, Monitor};

    {get_air_quality_index, Date, Station} ->
      Value = pollution:get_air_quality_index(Date, Station, Monitor),
      {Value, Monitor};

    {stop} ->
      Monitor,
      terminate();

    {crash, []} ->
      1 / 0
  end.

terminate() ->
  ok.
