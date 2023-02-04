-module(farm).
-export([generate_input/1, fib/2]).

generate_input(Length) -> [generate_list(Length), 8].

generate_list(0) -> [];
generate_list(Length) -> [15| generate_list(Length-1)].

%---------------------=== COMMON FUNCTIONS	 ===--------------------------------------
fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N-1) + fibonacci(N-2).

collector(Acc, 0, Main) ->
	Main ! {final, Acc};
collector(Acc, N, Main) ->
	receive
		{result, Res} ->
			collector([Res|Acc], N-1, Main)
	end.
		
dispatcher([], 0) ->
	stop;
dispatcher([], N) ->
	receive 
		{ready, Pid} ->
			Pid ! stop,
			dispatcher([], N-1)
	end;
dispatcher([Data | Rest], N) ->
	receive
		{ready, Worker} -> 
			Worker ! {data, Data},
			dispatcher(Rest, N)
	end.

%---------------------===	TASKFARMS	===--------------------------------------

fib(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> funWorker({Collector, Dispatcher, fun fibonacci/1}) end) 
				  end, lists:seq(1, K)),
	receive
		{final, Data} -> Data
	end.

funWorker({Collector, Dispatcher, F} = Args) ->
	Dispatcher ! {ready, self()},
	receive
		stop ->
			stop;
		{data, Data} ->
			Collector ! {result, F(Data)},
			funWorker(Args)
	end.

%----------------------------------------------------------------------------------------------------

