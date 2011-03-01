-module (myTest).
-export ([start/0]).
-import (ts2, [in/2, out/2, new/0]).

start() ->
  
  TS = new(),
  out(TS, test),
  A = in(TS, test),
  io:format("Received: ~p~n", [A]),
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed~n")
  end.
  
  
blocking() ->
  Self = self(),
  TS = new(),
  T1 = spawn_link(fun() -> blocked(TS, Self) end),
  T2 = spawn_link(fun() -> releaser(TS, Self) end),
  receive
    unblocked ->
      false;
    releaser ->
      receive
        unblocked ->
          true
      end
  end.
  
blocked(TS, Parent) ->
  Var = in(TS, {apa, bepa}),
  Parent! unblocked,
  io:format("Unblocked~n").
  
  
releaser(TS, Parent) ->
  io:format("Releaser starts...~n"),
  receive
    after 1000 ->
      Parent! releaser,
      out(TS, {apa, bepa}),
      io:format("Releaser sent message~n")
  end.