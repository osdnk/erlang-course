%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 11:38
%%%-------------------------------------------------------------------
-module(server_monitor).
-author("osdnk").

%% API
-export([init/0, start/0]).

start() ->
  PID = spawn(?MODULE, init, []),
  register(supervisor, PID).
init() ->
  process_flag(trap_exit, true),
  pollution_server:start_link(),
  receive
    {'EXIT', Pid, Reason} ->
      init()
  end.