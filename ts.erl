%%  A simple tuplespace implementation.
%%  The program is divided into a message server and a data server.
%%  A client uses the in and out functions to call the message server.
%%  The message server tries to service IN-requests by messaging a request to the data server.
%%  If the data server fails to match the request, the request is saved in the message server backlog.
%%  When the message server receives an OUT-request, it tries to service messages from the backlog first.
%&  If no backlog request can be serviced using the incoming OUT-request, that OUT-request is forwarded to the
%%  data server for storage.


-module (ts).
-export ([in/2, out/2, new/0]).
-import (dataServer, [dataServer/0]).
-import (match, [match/2]).

new() ->
  Dataserver = spawn_link(fun() -> dataServer() end),
  Server = spawn_link(fun() -> server(Dataserver) end).

server(Dataserver) -> server(Dataserver, []).

server(Dataserver, Backlog) ->
  % io:format("Server loop start, Backlog is: ~p~n", [Backlog]),
  receive
    {get, Caller, Pattern} ->
      % io:format("Got a get request~n"),
      Dataserver! {get, self(), Pattern},
      receive
        {found, Result} ->
          % io:format("Get: Server received found result~n"),
          Caller! Result,
          server(Dataserver, Backlog);
        {failed} ->
          % io:format("Get: Server received failed result~n"),
          server(Dataserver, [{Caller, Pattern}|Backlog])
      end;
    {put, Caller, Pattern} ->
      % io:format("Server: got a put request~n"),
      % The backlog needs to be reversed in order to serve older messages first
      {Result, Request, NewBacklog} = serveBacklog(Pattern, lists:reverse(Backlog)),
      % io:format("Server: result from backlog: ~p for request: ~p~n", [Result, Request]),
      case Result of
        found ->
          {OldCaller, FoundPattern} = Request,
          OldCaller! FoundPattern;
          % io:format("Sent blacklog~n");
        failed ->
          % io:format("Found no backlog match~n"),
          Dataserver! {put, Pattern}
      end,
      % The backlog needs to be reversed again in order to append new backlog messages
      server(Dataserver, lists:reverse(NewBacklog))
  end.

serveBacklog(Pattern, Backlog) ->
  serveBacklog(Pattern, Backlog, []).

serveBacklog(Pattern, [], List) ->
  {failed, Pattern, List};
serveBacklog(Pattern, [{Caller, Request}|Backlog], Checked) ->
  case match(Request, Pattern) of
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
