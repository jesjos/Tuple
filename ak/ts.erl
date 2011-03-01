-module (ts).
-export ([in/2, out/2, new/0]).

%% returns the PID of a new (empty) tuplespace

new() ->
 		spawn_link(fun() -> loop([],[]) end).


loop(TS,Q) ->
   receive
      {in, From, Ref, Pattern} ->
				case findTup(TS, Pattern) of
							{ok, Pattern} -> From ! {ok, Ref, Pattern};
							false -> loop(TS, Q ++ [{From,Ref,Pattern}])
				end;

       {out, Tuple} -> 
						case findTup(Q, Tuple) of 
							{ok, T, F, R} -> F ! {ok, R, T};
							false -> loop(TS ++ [Tuple], Q)
					end;
      stop -> true

end.

findTup([], _) -> false;
findTup([ {F,R,T} | TS ], Pattern) ->
												case match(T, Pattern) of
												true -> {ok, T, F, R};
												false -> findTup(TS, Pattern)
												end;

findTup([ T | TS ], Pattern) -> 
												case match(T, Pattern) of
												true -> {ok, Pattern};
												false -> findTup(TS, Pattern)
												end.
											

%% returns a tuple matching Pattern from tuplespace TS.
%% Note that this operation will block if there is no such tuple

in(TS, Pattern) ->
	Ref = make_ref(),
	TS ! {in, self(), Ref, Pattern},
  	receive
  	  	{ok, Ref, Result} ->
  	    	Result
 		end.


%% – puts Tuple into the tuplespace TS

out(TS, Tuple) ->
	Ref = make_ref(),
	TS ! {out, self(), Ref, Tuple}.
  


 
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.