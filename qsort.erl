%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 11:33
%%%-------------------------------------------------------------------
-module(qsort).
-export([qs2/1]).
-export([split/2]).
-author("osdnk").

lessThan(List, Arg) -> lists:filter(fun(A) -> A < Arg end, List).
grtEqThan(List, Arg) -> lists:filter(fun(A) -> A >= Arg end, List).
qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

cor (X, {Prev, Next}, Pivot) when (X > Pivot) -> {Prev, [X | Next]};
cor (X, {Prev, Next}, _) -> {[X | Prev], Next}.


split(A, Pivot) -> lists:foldl(fun (X, Acc) -> cor(X, Acc, Pivot) end, {[], []}, A).


qs2([]) -> [];
qs2([Pivot|Tail]) ->
  {L, G} = split(Tail, Pivot),
  qs2(L) ++ [Pivot] ++ qs2(G).

randomElems(N,Min,Max)-> [ random:uniform(Max - Min) + Min || _<- lists:seq(1,N)].


