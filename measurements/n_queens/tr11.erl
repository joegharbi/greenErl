-module(tr11).
-export([parmap/3]).

parmap(F, L, N) ->
	Main = self(),
	DPid = spawn(fun() -> dispatcher(L) end),
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
	
worker(F, DPid, CPid)->
	receive 
		{data, D} -> 
			CPid ! {ready, F(D)},
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		init ->
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		stop ->
			ok
	end.

collector(N, N, Acc, Main) ->
	Main ! {value, self(), Acc};
collector(C, N, Acc, Main) ->
	receive 
		{ready, Data} ->
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
