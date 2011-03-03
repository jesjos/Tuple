-module (myTest).
-export ([start/0, cb/0, cbIN/2, concBasicIN/3, cbOUT/2]).
-import (ts, [in/2, out/2, new/0]).

start() ->
  basicTests(),
 
  
  
  Blocking = blocking(),
  case Blocking of
    true -> io:format("Blocking succeeded~n");
    false -> io:format("Blocking failed~n")
  end,
  
  emptySpaceAndTupleRemoval(),
  cb().
  
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


emptySpaceAndTupleRemoval() ->
  io:format("Beginning tuple removal test.~n"),
  TS = new(),
  Self = self(),
  Writer = spawn_link(fun() -> writer(TS, Self) end),
  spawn_link(fun() -> reader(TS,1, Writer) end),
  spawn_link(fun() -> reader(TS,2, Writer) end),
  receive
    done ->
      io:format("Correct: tuples are removed by IN-operations and tuplespace is empty. Message queue is fair. ~n");
    {failed, removal} ->
      io:format("Failed: tuples aren't removed.~n");
    {failed, order} ->
      io:format("Failed: order a of receives is wrong, message queue isn't fair.~n")
  after 3000 ->
    io:format("Failed: tupleremoval failed due to time-out.~n")
  end.

% Reads a tuple and sends a done message to a writer
reader(TS, X, Writer) ->
  {_, N} = inprint(TS, {test, any}),
  io:format("     Reader no ~p received a tuple: ~p.~n", [X, N]),
  Writer! {done, X}.

% Reads any tuple - used to check if TS is empty
anyReader(TS, Writer) ->
  in(TS, any),
  io:format("     The any-reader received a tuple.~n"),
  Writer! done.

% Writes tuples to TS. Spawns an anyReader when write is complete. Receives done messages.
writer(TS, Parent) ->
  Self = self(),
  writeout(TS, 2),
  spawn_link(fun() -> anyReader(TS, Self) end),
  writerReceive(Parent).
  
writeout(_,0) -> true;
writeout(TS,N) ->
   outprint(TS, {test,N}),
   writeout(TS, N-1).
   
writerReceive(Parent) -> r(Parent,0,2).

% If we receive a message after receiving the two previous messages, the TS is not empty
r(Parent,N,N) ->
  receive
    _ ->
      Parent! {failed, removal}
  after 1000 ->
      Parent! done
  end;
% Receives a reader-done message. Checks that order is correct.
r(Parent, X,N) ->
  Y = X+1, 
  receive
    {done, Y} ->
      r(Parent, Y, N);
    _ ->
      Parent! {failed, order}
  end.

%% Calls in/out with different tuples to ensure that user
%% can not crasch program by inserting "wrong" tuples
cb()->
TS = new(),
io:format("~nWill test some IN/OUT calls with different tuples to assure that user"),
io:format("can not crasch program by inserting wrong tuples, also blocking will be tested~n"),

spawn_link(fun() -> cbIN(TS,a) end),
spawn_link(fun() -> cbOUT(TS,a) end),

spawn_link(fun() -> cbIN(TS,[a]) end),
spawn_link(fun() -> cbOUT(TS,[a]) end),

%% tests that the first two process blocks
%% but still lets third process receive msg
spawn_link(fun() -> cbIN(TS,{a}) end),
spawn_link(fun() -> cbIN(TS,[]) end),
spawn_link(fun() -> cbIN(TS,[a,b]) end),
spawn_link(fun() -> cbOUT(TS,{a,b}) end),



spawn_link(fun() -> cbOUT(TS,{a,b}) end),
spawn_link(fun() -> cbIN(TS,{any, a}) end),

spawn_link(fun() -> cbIN(TS,{123, any}) end),
spawn_link(fun() -> cbOUT(TS, {123,45}) end).

%% calls in and waits for respons
cbIN(TS, Tuple)->
Self = self(),
spawn_link(myTest, concBasicIN, [TS, Tuple, Self]),
io:format("IN call with tuple: ~p~n",[Tuple]),
receive 
	{ok, T} -> 
		io:format("IN with tuple: ~p was OK~n", [T])
       after 4000 -> 
		io:format("IN with tuple: ~p FAILED~n", [Tuple])
end.

concBasicIN(TS, Tuple, From) ->
  From ! {ok, in(TS, Tuple)}.

cbOUT(TS, Tuple)->
io:format("OUT call with ~p~n",[Tuple]),
out(TS, Tuple).  
  
  
%% Helper functions

% Pretty-printing
outprint(TS, Pattern) ->
  io:format("   OUT - TS: ~p Pattern: ~p~n", [TS, Pattern]),
  out(TS,Pattern).

inprint(TS, Pattern) ->
  io:format("   IN - TS: ~p Request: ~p~n", [TS, Pattern]),
  In = in(TS, Pattern),
  io:format("     IN: Received: ~p~n", [In]),
  In.