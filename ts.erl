-module (ts).
-export ([in/2, out/2, new/0]).

new() ->
  Server = spawn_link(fun() -> server() end).

server() ->
  receive
    {get, Pattern} ->
      server();
    {put, Pattern} ->
      server()
  end.

in(TS, Pattern) ->
  TS! {get, self(), Pattern},
  receive
    {ok, Result} ->
      Result
  end.

out(TS, Pattern) ->
  TS! {put, Pattern}.