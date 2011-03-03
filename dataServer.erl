%%  A dataserver. Receives get and put requests.
%%  Returns results to caller. 
%%  Performs no additional actions if request fails (i.e. has no request queue).

-module (dataServer).
-export ([dataServer/0]).
-import (match, [match/2]).

dataServer() ->
  dataServer([]).
  
dataServer(List) ->
  receive
    {get, Caller, Pattern} ->
      Response = findAndRemove(Pattern, List),
      case Response of
        {{found, Result}, NewList} ->
          Caller! {found, Result},
          dataServer(NewList);
        {failed} ->
          Caller! {failed},
          dataServer(List)
      end;
    {put, Pattern} ->
      dataServer([Pattern|List])
  end.
  
  
%% Finds a matching tuple in a list and removes it.
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