-module (ts).
-export ([in/2, out/2, new/0]).

%% returns the PID of a new (empty) tuplespace

new() ->
 		spawn_link(fun() -> loop([],[]) end).


loop(TS,Q) ->
   receive
      {in, From, Pattern} ->
				case findTup(TS, Pattern) of
		     {Pattern} -> 
						From ! {From, Pattern},
            loop(TS -- [Pattern], Q);
		     false -> 
						loop(TS, Q ++ [{From,Pattern}])
				end;
       {out, Tuple} -> 
io:format("Q: ~p~n", [Q]),
io:format("TS: ~p~n", [TS]),
		case findTup(Q, Tuple) of 
			   {Tuple, From} -> From ! {F, Tuple},
					            loop(TS, Q -- [T]);
			   false -> loop(TS ++ [Tuple], Q),
io:format("Q: ~p~n", [Q]),
io:format("TS: ~p~n", [TS])
		end;
    stop -> true
end.
								

findTup([], _) -> false;
findTup([ {From, Pattern} | TS ], Tuple) ->
		case match(Pattern, Tuple) of
		    true -> {Tuple, From};
				false -> findTup(TS, Pattern)
		end;

findTup([ T | TS ], Pattern) -> 
		case match(Pattern, T) of
					true -> {T};
					false -> findTup(TS, Pattern)	
		end.
											

%% returns a tuple matching Pattern from tuplespace TS.
%% Note that this operation will block if there is no such tuple

in(TS, Pattern) ->
	TS ! {in, self(), Pattern},
  	receive
  	  	{From, Result} ->
  	    	Result
    end.


%% – puts Tuple into the tuplespace TS

out(TS, Tuple) ->
	TS ! {out, Tuple}.
  
match(_, any) -> true;
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS);
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.
