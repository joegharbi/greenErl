-module(tr12).
-export([ordmap/3]).

ordmap(F, L, N) ->
	Main = self(),
	Index = lists:seq(1,length(L)),
	DPid = spawn(fun() -> dispatcher(lists:zip(Index, L)) end),
	CPid = spawn(fun() -> collector(0, length(L), [], Main) end),
	Workers = [spawn(fun() -> worker(F, DPid, CPid) end) || _<- lists:seq(1, N)],
	[W ! init || W <- Workers],
	receive 
		{value, CPid, Data} ->
			Data
	after 5000 -> Data = []
	end,
	[W ! stop || W <- Workers],
	Data.
	
worker_init(F, DPid, CPid) ->
	DPid ! {done, self()},
	worker_(F, DPid, CPid).

worker_(F, DPid, CPid)->
	receive 
		{data, D} -> 
			CPid ! {ready, F(D)},
			DPid ! {done, self()},
			worker_(F, DPid, CPid);
		stop ->
			ok
	end.

worker(F, DPid, CPid)->
	receive 
		{data, {I, D}} -> 
			CPid ! {ready, {I, F(D)}},
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		init ->
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		stop ->
			ok
	end.

collector(N, N, Acc, Main) ->
	Main ! {value, self(), lists:reverse(Acc)};
collector(C, N, Acc, Main) ->
	receive 
		{ready, {I, Data}} when I == C+1 ->
			collector(C+1, N, [Data | Acc], Main)
	end.

dispatcher([]) ->
	ok;
dispatcher([H|T]) ->
	receive 
		{done, W} ->
			W ! {data, H},
			dispatcher(T)
	end.
