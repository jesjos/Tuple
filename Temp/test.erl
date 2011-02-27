-module(test).
-export([t3/0, t5/1]).

t3() ->
  T4 = spawn_link(fun() -> t4() end),
  T4! {found, {apa, bepa}, []},
  io:format("Send message~n").
  
t4() ->
  receive
    Response ->
      io:format("Received: ~p", [Response]),
      case Response of
        {found, Result, List} ->
          io:format("Found: ~p ~p~n", [Result, List]);
        {failed, Result} ->
          io:format("Failed, ~p~n", [Result])
      end
  end.

t5(X) ->
  case X of
    {found, Result, List} ->
      io:format("Found: ~p ~p~n", [Result, List]);
    {failed, Result} ->
      io:format("Failed, ~p~n", [Result])
  end.