-module (test).
-export ([start/0]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  % TS = new(),
  % out(TS, {apa, bepa}),
  % out(TS, {schaitan, [1,2,3,4], {apor, bepor}}),
  % io:format("Send message"),
  % A = in(TS, {apa, any}),
  % {_, List,_} = in(TS, {any, any, {apor, any}}),
  % io:format("Found: ~p and ~p~n", [A, List]),
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed")
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