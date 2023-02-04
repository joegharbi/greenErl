-module(taskfarm).
-export([generate_input/1, farm_fun/2, farm/2, farm_named/2]).

generate_input(Length) -> [lists:seq(1,Length), 8].
	
%---------------------=== COMMON FUNCTIONS	 ===--------------------------------------
increase(X) -> X + 1.

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

farm_fun(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> funWorker({Collector, Dispatcher, fun(X) -> X + 1 end}) end) 
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

farm(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> worker({Collector, Dispatcher}) end) 
				  end, lists:seq(1, K)),
	receive
		{final, Data} -> Data
	end.

worker({Collector, Dispatcher} = Args) ->
	Dispatcher ! {ready, self()},
	receive
		stop ->
			stop;
		{data, Data} ->
			Collector ! {result, Data + 1},
			worker(Args)
	end.

farm_named(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> funWorker({Collector, Dispatcher, fun increase/1}) end) 
				  end, lists:seq(1, K)),
	receive
		{final, Data} -> Data
	end.

farm_worker_named(L, K) ->
	DataLen = length(L),
	FarmPid = self(),
	Dispatcher = spawn(fun() -> dispatcher(L, DataLen) end),
	Collector = spawn(fun() -> collector([], DataLen, FarmPid) end),
	lists:foreach(fun(_) ->
						spawn(fun() -> worker_named({Collector, Dispatcher}) end) 
				  end, lists:seq(1, K)),
	receive
		{final, Data} -> Data
	end.

worker_named({Collector, Dispatcher} = Args) ->
	Dispatcher ! {ready, self()},
	receive
		stop ->
			stop;
		{data, Data} ->
			Collector ! {result, increase(Data)},
			worker_named(Args)
	end.
%----------------------------------------------------------------------------------------------------

