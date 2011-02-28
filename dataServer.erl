-module (dataServer).
-export ([dataServer/0]).
-import (match, [match/2]).

dataServer() ->
  dataServer([]).
  
dataServer(List) ->
  receive
    {get, Caller, Pattern} ->
      io:format("data got a get request~n"),
      {Response, NewList} = findAndRemove(Pattern, List),
      io:format("Response in data: ~p", [{Response, NewList}]),
      case Response of
        {found, Result} ->
          Caller! {found, Result},
          dataServer(NewList);
        {failed} ->
          Caller! failed,
          dataServer(List)
      end,
      io:format("Data sent a get respons");
    {put, Pattern} ->
      io:format("data got a put request~n"),
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