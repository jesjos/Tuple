-module (test).
-export ([start/0]).
-import (ts, [new/0, in/2, out/2]).

start() ->
  TS = new(),
  out(TS, {apa, bepa}),
  {_,X} = in(TS, {apa, any}),
  io:format("Letade efter: ~p, sökte efter: ~p", [bepa, X]).
  
t2(X) ->
  case X of
    {found, Result, List} ->
      io:format("Found: ~p ~p~n", [Result, List]);
    {failed, Result} ->
      io:format("Failed, ~p~n", [Result])
  end.