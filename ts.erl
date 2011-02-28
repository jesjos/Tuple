-module (ts).
-export ([in/2, out/2, new/0]).
-import (dataServer, [dataServer/0]).
-import (match, [match/2]).

new() ->
  Dataserver = spawn_link(fun() -> dataServer() end),
  Server = spawn_link(fun() -> server(Dataserver) end),
  io:format("Server: ~p~n", [Server]),
  Server.

server(Dataserver) -> server(Dataserver, []).

server(Dataserver, Backlog) ->
  receive
    {get, Caller, Pattern} ->
      Dataserver! {get, self(), Pattern},
      receive
        {found, Result} ->
          Caller! Result,
          server(Dataserver, Backlog);
        {failed} ->
          server(Dataserver, [{Caller, Pattern}|Backlog])
      end;
    {put, Caller, Pattern} ->
      {Result, Request, NewBacklog} = serveBacklog(Pattern, Backlog),
      case Result of
        found ->
          {Caller, FoundPattern} = Request,
          Caller! FoundPattern;
        failed ->
          Dataserver! {put, Pattern}
      end,
      io:format("Put message~n"),
      server(Dataserver, NewBacklog)
  end.

serveBacklog(Pattern, Backlog) ->
  serveBacklog(Pattern, Backlog, []).

serveBacklog(Pattern, [], List) ->
  {failed, Pattern, List};
serveBacklog(Pattern, [{Caller, Request}|Backlog], Checked) ->
  case match(Pattern, Request) of
    true ->
      {found, {Caller, Pattern}, lists:append(lists:reverse(Checked), Backlog)};
    false ->
      serveBacklog(Pattern, Backlog, [{Caller, Request}|Checked])
  end.
  
in(TS, Pattern) ->
  TS! {get, self(), Pattern},
  receive
    Result ->
      Result
  end.

out(TS, Pattern) ->
  TS! {put, self(), Pattern}.
