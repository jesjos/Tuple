%% @author Jesper Josefsson
-module (ts).
-export ([new/0, in/2, out/2]).


new() ->
  Data = spawn_link(fun() -> data() end),
  Server = spawn_link(fun() -> server(Data, []) end).

in(TS, Pat) ->
  TS! {get,self(), Pat},
  receive
    {ok, Result} ->
      Result
  end.

out(TS, Pat) ->
  TS! {put, Pat}.
  
server(Data, Backlog) ->
  receive
    {put, Pat} ->
      CheckedBacklog = checkBacklog(Pat, Backlog),
      {Response, NewBackLog} = CheckedBacklog,
      case Response == succeeded of
        false -> Data! {put, Pat};
        true -> true
      end,
      server(Data, NewBackLog);
    {get, Caller, Pat} ->
      Data! {get, self(), Pat},
      receive 
        {failed} ->
          server(Data, [{backlog, Caller, Pat}|Backlog]);
        {found, Result} ->
          Caller!{ok,Result},
          server(Data, Backlog)
      end
  end.

checkBacklog(Pattern, Backlog) -> checkBacklog(Pattern, Backlog, []).

checkBacklog(_, [], Backlog) ->
  {failed,Backlog};
checkBacklog(Pattern, [H|T], Backlog) ->
  {_,Caller, Request} = H,
  case match(Pattern, Request) of
    true ->
      Caller! {ok, Pattern},
      {succeeded, backwardConcat(T, Backlog)};
    false ->
      checkBacklog(Pattern, T, [H|Backlog])
  end.
    
backwardConcat(Target, []) -> Target;
backwardConcat(Target, [H|T]) -> backwardConcat([H|Target], T).

data() ->
  data([]).
  
data(List) ->
  receive
    {put, Data} ->
      data([Data|List]);
    {get, Caller, Data} ->
      Response = removeIfContains(Data, List),
      io:format("Respone, ~p~n", [Response]),
      case Response of
        {found, Result, List} ->
          io:format("Found: ~p ~p~n", [Result, List]),
          Caller! Result,
          data(List);
        {failed, List} ->
          io:format("Failed, ~p~n", [List]),
          Caller! failed,
          data(List)
      end
  end.

removeIfContains(Data, List) -> removeIfContains(Data, List, []).

removeIfContains(_, [], Checked) ->
  {failed, Checked};
removeIfContains(Data, [H|T], Checked) ->
  case match(Data, H) of
    true ->
      {found, H, backwardConcat(T, Checked)};
    false ->
      removeIfContains(Data, T, [H|Checked])
  end.

match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.