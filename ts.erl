-module (ts).
-export ([in/2, out/2, new/0]).
-import (dataServer, [dataServer/0]).
-import (match, [match/2]).

new() ->
  Dataserver = spawn_link(fun() -> dataServer() end),
  Server = spawn_link(fun() -> server(Dataserver) end).

server(Dataserver) -> server(Dataserver, []).

server(Dataserver, Backlog) ->
  io:format("Server loop start~n"),
  receive
    {get, Caller, Pattern} ->
      io:format("Got a get request"),
      Dataserver! {get, self(), Pattern},
      receive
        {found, Result} ->
          io:format("Get: Server received found result~n"),
          Caller! Result,
          server(Dataserver, Backlog);
        {failed} ->
          io:format("Get: Server received failed result~n"),
          server(Dataserver, [{Caller, Pattern}|Backlog])
      end;
    {put, Caller, Pattern} ->
      io:format("Got a put request"),
      {Result, Request, NewBacklog} = serveBacklog(Pattern, Backlog),
      io:format("Result from backlog: ~p ~p", [Result, Request]),
      case Result of
        found ->
          {OldCaller, FoundPattern} = Request,
          OldCaller! FoundPattern,
          io:format("Sent blacklog~n");
        failed ->
          io:format("Found no backlog match"),
          Dataserver! {put, Pattern}
      end,
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
