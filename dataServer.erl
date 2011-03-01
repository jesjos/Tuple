-module (dataServer).
-export ([dataServer/0]).
-import (match, [match/2]).

dataServer() ->
  dataServer([]).
  
dataServer(List) ->
  io:format("Entered dataServer loop~n"),
  receive
    {get, Caller, Pattern} ->
      Response = findAndRemove(Pattern, List),
      io:format("Response: ~p~n", [Response]),
      case Response of
        {{found, Result}, NewList} ->
          io:format("Found!~n"),
          Caller! {found, Result},
          io:format("Message sent: ~p, Caller: ~p~n", [{found, Result}, Caller]),
          dataServer(NewList);
        {failed} ->
          io:format("Failed!~n"),
          Caller! {failed},
          dataServer(List)
      end,
      io:format("Data sent a get response~n");
    {put, Pattern} ->
      io:format("Data got a put request~n"),
      dataServer([Pattern|List])
  end.
  
  
findAndRemove(Pattern, List) ->
  findAndRemove(Pattern, List, []).

findAndRemove(_, [], _) ->
  {failed};
findAndRemove(Pattern, [Record|List], Checked) ->
  case match(Pattern, Record) of
    true ->
      {{found, Record}, lists:append(lists:reverse(Checked), List)};
    false ->
      findAndRemove(Pattern, List, [Record|Checked])
  end.