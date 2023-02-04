-module(farm_8).
-export([generate_input/1, named_function/2]).

generate_input(Length) -> [lists:seq(1,Length), 8].
	
%---------------------=== COMMON FUNCTIONS	 ===--------------------------------------
identity(X) -> X.

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

named_function(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> funWorker({Collector, Dispatcher, fun identity/1}) end) 
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