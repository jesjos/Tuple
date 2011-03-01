-module (ts2).
-export ([new/0, in/2, out/2]).
-import (match, [match/2]).

new() ->
  spawn_link(fun() -> loop([],[]) end).
    
loop(TS, Q) ->
  io:format("TS: ~p, Q: ~p~n", [TS, Q]),
  receive
    {get, Caller, Pattern} ->
      case get({Caller,Pattern}, TS) of
        {found, {_, Result}, NewList} ->
          Caller! Result,
          loop(NewList, Q);
        failed ->
          io:format("Sending to backlog, request: ~p~n", [{Caller, Pattern}]),
          loop(TS, [{Caller, Pattern}|Q])
      end;
    {put, Pattern} ->
      case get({none,Pattern}, lists:reverse(Q)) of
        {found, {Caller, _}, NewList} ->
          io:format("Found in backlog, sending Caller: ~p, Pattern: ~p~n", [Caller, Pattern]),
          Caller! Pattern,
          loop(TS, lists:reverse(NewList));
        failed ->
          loop([{none, Pattern}|TS], Q)
      end 
  end.

get(Request, List) -> get(Request, List, []).

get(_, [], _) ->
  failed; 
get(Request, [Head|List], Checked) ->
  {InCaller, InPattern} = Request,
  {ListCaller, ListPattern} = Head,
  case match(ListPattern, InPattern) of
    true -> 
      io:format("Get is returning: ~p", [{found, {ListCaller, ListPattern}, lists:reverse(Checked) ++ List}]),
      {found, {ListCaller, ListPattern}, lists:reverse(Checked) ++ List};
    false -> get(Request, List, [Head|Checked])
  end.
  


in(TS, Pattern) ->
  TS! {get, self(), Pattern},
  receive
    Result ->
      Result
  end.

out(TS, Pattern) ->
  TS! {put, Pattern}.