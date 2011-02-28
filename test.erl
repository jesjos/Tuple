-module (test).
-export ([start/0]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  TS = new(),
  out(TS, {apa, bepa}),
  io:format("Send message"),
  A = in(TS, {apa, any}),
  io:format("Found: ~p~n", [A]).