-module (myTest).
-export ([start/0]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  basicTest(),
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed~n")
  end.
  
  
blocking() ->
  io:format("Testing blocking..."),
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
  

basicTest() ->
  io:format("Running basic test, send and receive~n"),
  TS = new(),
  out(TS, {apa}),
  case in(TS, {apa}) of
    {apa} ->
      io:format("Correct: Sent {apa}, got {apa}~n");
    Other ->
      io:format("Send and receive failed, sent {apa}, received: ~p~n", [Other])
  end.