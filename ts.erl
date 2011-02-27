%% @author Jesper Josefsson
-module (ts).
-export ([new/0, in/2, out/2]).


new() ->
  Data = spawn(fun() -> data() end),
  Server = spawn(fun() -> server(Data, []) end).

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
      checkBacklog(Pat, Backlog),
      Data! {put, Pat};
    {get, Pat} ->
      Data! {get, self(), Pat},
      receive 
        {false} ->
          server(Data, [Pat|Backlog]);
        {true, Result} ->
          Result
      end
  end.

data() ->
  data([]).
  
data(List) ->
  receive
    {put, Data} ->
      data([Data|List]);
    {get, Caller, Data} ->
      Result = lists:filter(fun(Elem) -> match(Match Elem, Data) end, List),
        case Result == [] of
          true ->
            Caller! {false}
          false ->
            [Head|Tail] = Result,
            Caller! {true, Head}
        end


match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.