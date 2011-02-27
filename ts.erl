%% @author Jesper Josefsson
-module (ts).
-export ([new/0, in/2, out/2]).


new() ->
  Data = spawn(fun() -> data() end),
  Server = spawn(fun() -> server(Data) end).

in(TS, Pat) ->
  TS! {get,self(), Pat},
  receive
    {ok, Result} ->
      Result
  end.

out(TS, Pat) ->
  TS! {put, Pat}.
  
server(Data) ->
  receive
    {put, Pat} ->
      Data! {put, Pat};
    {get, Pat} ->
      Data! {get, Pat}
  end.

data() ->
  data([]).
  
data(List) ->
  receive
    {put, Data} ->
      data([Data|List]);
    {get, Caller, Data} ->
      [Head|Tail] = lists:filter(fun(Elem) -> match(Match Elem, Data) end, List),
      Head


match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.