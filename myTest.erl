-module (myTest).
-export ([start/0]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  basicTests(),
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed~n")
  end.
  
  
blocking() ->
  io:format("Testing blocking...~n"),
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
  io:format("   Blocker is blocking~n"),
  Var = in(TS, {apa, bepa}),
  Parent! unblocked,
  io:format("   Unblocked~n").
  
  
releaser(TS, Parent) ->
  io:format("   Releaser starts...~n"),
  receive
    after 1000 ->
      Parent! releaser,
      out(TS, {apa, bepa}),
      io:format("   Releaser sent message~n")
  end.
  

basicTests() ->
  io:format("Running basic test, send and receive~n"),
  TS = new(),
  out(TS, {apa}),
  case in(TS, {apa}) of
    {apa} ->
      io:format("   Correct: Sent {apa}, got {apa}~n");
    Other ->
      io:format("Send and receive failed, sent {apa}, received: ~p~n", [Other])
  end,
  
  io:format("Running multiple outs...~n"),
  outprint(TS, {apa}),
  outprint(TS, {apa}),
  A = inprint(TS, any),
  B = inprint(TS, any),
  case A == B of
    true ->
      io:format("Correct: Tuplespace can receive multiple identical tuples.~n~n");
    false ->
      io:format("Failed: Tuplespace can't accomodate multiple identical tuples.~n~n")
  end.
  

outprint(TS, Pattern) ->
  io:format("   OUT - TS: ~p Pattern: ~p~n", [TS, Pattern]),
  out(TS,Pattern).

inprint(TS, Pattern) ->
  io:format("   IN - TS: ~p Pattern: ~p ", [TS, Pattern]),
  In = in(TS, Pattern),
  io:format("Received: ~p~n", [In]),
  In.