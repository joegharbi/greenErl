-module(pipe).

-compile(pipeline/2).

generate_input(Length) -> [generate_list(Length), [ fun fib/1 || _ <- lists:seq(1, Length)]].

generate_list(0) -> [];
generate_list(Length) -> [1| generate_list(Length-1)].

%---------------------=== COMMON FUNCTIONS	 ===--------------------------------------
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).


start(L, N) ->
	Input = lists:map(fun(X) -> X end, L),
	F = fun(X) -> X+1 end,
	pipeline(Input, [ F || _ <- lists:seq(1, N)]).
	
pipeline(L, []) ->
	L;
pipeline(L, FunList) ->
	Parent = self(),
	[First | _ ] = Pids = [spawn(fun() -> worker(Parent, Fun) end) || Fun <- FunList],
	send([Parent | Pids] ++ [Parent]),
	[First ! {Parent, Data} || Data <- L ],
	First ! stop,
	Result = [receive
				{_, Res} -> Res
						end || Data <- L],
	receive 
		stop -> Result
	end.
	 
send([Prev, Pid, Next | T]) ->
	Pid ! {self(), Prev, Next},
	send([Pid, Next|T]);
send(_) ->
	ok.
	
worker(Parent, F) ->
	receive
		{Parent, Prev, Next} -> do({Prev, Next, F})
	end.

do(Args = {Prev, Next, F}) ->
	receive
		{Prev, Data} -> 
			Next ! {self(), F(Data)},
			do(Args);
		stop ->
			Next ! stop
	end.
				
			
	
	
	
% run2(N, Data) ->
%     Main = self(), 
% 	Pids = [spawn(fun() -> worker(Main) end) || _ <- lists:seq(1, N)],
% 	New = Pids ++ [Main],
% 	[ Pid ! {Main, New} || Pid <- Pids],
% 	hd(Pids) ! Data,
% 	receive
% 		Data -> vege
% 	end.
	
% worker(Main) ->
% 	receive
% 		{Main, Pids} when is_list(Pids) -> 
% 		    Next = search(self(), Pids),
% 		    receive
% 				Data -> Next ! Data
% 			end
% 	end.

% search(Pid, [Pid | [Next | Pids]]) -> %% [Pid, Next | Pids]
% 	Next;
% search(Pid, [_ | L = [Next | Pids]]) -> %% [Pid, Next | Pids]
% 	search(Pid, L).
