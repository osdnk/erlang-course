%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 11:38
%%%-------------------------------------------------------------------
-module(server_monitor).
-behaviuor(supervisor).
-author("osdnk").

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, supervisor}, ?MODULE, []).

init([]) ->
  {ok, {
    {one_for_one, 10, 1000},
    [{pollution_server,
      {pollution_server, start_link, []},
      permanent, 5000, worker, [pollution_server]}
    ]
  }
  }.



