-module (myTest).
-export ([start/0]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  basicTests(),
  
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed~n")
  end,
  
  emptySpaceAndTupleRemoval().
  
  %% massive(300).
  
  
blocking() ->
  io:format("Testing blocking...~nCreating blocking process and receiving process.~n"),
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
        after 2000 ->
          false
      end
  end.
  
blocked(TS, Parent) ->
  io:format("   Blocker is blocking~n"),
  Var = inprint(TS, {apa, bepa}),
  Parent! unblocked,
  io:format("   Blocker process unblocked~n").
  
  
releaser(TS, Parent) ->
  io:format("   Releaser starts... Waiting~n"),
  receive
    after 1000 ->
      Parent! releaser,
      outprint(TS, {apa, bepa}),
      io:format("   Releaser sent message~n")
  end.
  

basicTests() ->
  io:format("Running basic test, send and receive~n"),
  TS = new(),
  outprint(TS, {apa}),
  case inprint(TS, {apa}) of
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


massive(N) -> 
  TS = new(),
  massive(TS, N, []).

massive(TS, 0, List) ->
  receive
    after 3000 -> massive(stop, List)
  end;
massive(TS, N, List) ->
  io:format("List: ~p~n", [List]),
  NewInner = spawn_link(fun() -> inner(TS, N) end),
  NewOutter = spawn_link(fun() -> outter(TS, N) end),
  massive (TS, N-1, [NewInner|[NewOutter|List]]).

massive(stop, []) ->
  true;
massive(stop, [Head|List]) ->
  Head! stop,
  massive(stop, List).

inner(TS, Tuple) -> inner(TS, Tuple, Tuple).

inner(_,_,0) -> true;
inner(TS, Tuple, N) ->
  receive
    stop ->
      true
  after 1 ->
    Result = in(TS, {Tuple, N}),
    io:format("       An inner received: ~p~n", [Result]),
    inner(TS, Tuple, N-1)
  end.

outter(TS, Tuple) -> outter(TS, Tuple, Tuple).

outter(_,_,0) -> true;
outter(TS, Tuple, N) ->
  receive
    stop ->
      true
    after 1 -> 
      io:format("       An outter is sending: ~p~n", [{Tuple, N}]),
      out(TS, {Tuple, N}),
      outter(TS, Tuple, N-1)
  end.
  

emptySpaceAndTupleRemoval() ->
  io:format("Beginning tuple removal test.~n"),
  TS = new(),
  Self = self(),
  Writer = spawn_link(fun() -> writer(TS, Self) end),
  spawn_link(fun() -> reader(TS, Writer) end),
  spawn_link(fun() -> reader(TS, Writer) end),
  receive
    done ->
      io:format("Correct: tuples are removed by IN-operations and tuplespace is empty.~n");
    failedRemoval ->
      io:format("Failed: tuples aren't removed.~n")
  after 3000 ->
    io:format("Failed: tupleremoval failed due to time-out.~n")
  end.

reader(TS, Writer) ->
  {_, N} = inprint(TS, {test, any}),
  io:format("     Reader no ~p received a tuple.~n", [N]),
  Writer! done.

anyReader(TS, Writer) ->
  in(TS, any),
  io:format("     The any-reader received a tuple.~n"),
  Writer! done.

writer(TS, Parent) ->
  Self = self(),
  writeout(TS, 2),
  spawn_link(fun() -> anyReader(TS, Self) end),
  writerReceive(Parent).
  
writeout(_,0) -> true;
writeout(TS,N) ->
   outprint(TS, {test,N}),
   writeout(TS, N-1).
   
writerReceive(Parent) -> r(Parent,2).

r(Parent, 0) ->
  receive
    _ ->
      Parent! failedRemoval
  after 1000 ->
      Parent! done
  end;
r(Parent, N) -> 
  receive
    _ ->
      r(Parent, N-1)
  end.
      
%% Helper functions

outprint(TS, Pattern) ->
  io:format("   OUT - TS: ~p Pattern: ~p~n", [TS, Pattern]),
  out(TS,Pattern).

inprint(TS, Pattern) ->
  io:format("   IN - TS: ~p Request: ~p~n", [TS, Pattern]),
  In = in(TS, Pattern),
  io:format("     IN: Received: ~p~n", [In]),
  In.