%%%-------------------------------------------------------------------
%%% @author osdnk
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2018 23:21
%%%-------------------------------------------------------------------
-module(onp).
-author("osdnk").
-export([onp/1]).

onp(Expr) -> innerOnp([], string:tokens(Expr, " ")).

str_to_num(N) ->
  case string:to_float(N) of
    {error,no_float} -> float(list_to_integer(N));
    {F,_Rest} -> F
  end.

is_operand(A) ->
  case re:run(A, "^[0-9].*$") of
    {match, _} -> false;
    _ -> true
  end.

operate("+", A, B) -> A + B;
operate("-", A, B) -> A - B;
operate("*", A, B) -> A * B;
operate("/", A, B) -> A / B;
operate("^", A, B) -> math:pow(A, B).
operate_single("sqrt", A) -> math:sqrt(A);
operate_single("sin", A) -> math:sin(A);
operate_single("cos", A) -> math:cos(A);
operate_single("tan", A) -> math:tan(A).

innerOnp([A | _], []) -> A;
innerOnp(Numbers, [LA | LB]) ->
  case is_operand(LA) of
    true -> parse_operand(Numbers, [LA | LB]);
    false -> innerOnp([str_to_num(LA) | Numbers], LB)
  end.
parse_operand([A | [B | C]], [LA | LB])
  when (LA =:= "*") or (LA =:= "-") or (LA =:= "+") or (LA =:= "/") or (LA =:= "^")  ->
   innerOnp([operate(LA, B, A) | C], LB);
parse_operand([A | B], [LA | LB]) ->
  innerOnp([operate_single(LA, A) | B], LB).

%% API