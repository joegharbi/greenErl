-module(parmap_fib).
-export([generate_input/1, ordparmap/1, parmap/1,ordparmap_hof/1, parmap_hof/1, parmap_list_comprehension/1, ordparmap_list_comprehension/1]).

generate_input(Length) -> [generate_list(Length)].

generate_list(0) -> [];
generate_list(Length) -> [15| generate_list(Length-1)].

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).
%---------------------=== ORDPARMAP ===--------------------------------------

ordparmap(L) ->
	MyPid = self(),
	Pids = lists:map(fun(X) -> spawn(fun() -> MyPid ! {self(), fib(X)} end) end, L),
	ord_receives(Pids).

ord_receives([]) ->  [];
ord_receives([Pid|Pids]) ->
  receive
		{Pid, Val} -> Val
	end,
	[Val | ord_receives(Pids)].

%---------------------=== PARMAP ===--------------------------------------

parmap(L) ->
	Pid = self(),
	lists:foreach(fun(X) -> spawn(fun() -> Pid ! fib(X) end) end, L),
	receives(length(L)).

receives(0) ->
	[];
receives(N) ->
    receive
		Val -> Val
	end,
	[Val | receives(N-1)].

%---------------------=== ORDPARMAP HOF ===--------------------------------------

ordparmap_hof(L) ->
	MyPid = self(),
	Pids = lists:map(fun(X) -> spawn(fun() -> MyPid ! {self(), fib(X)} end) end, L),
	lists:map(fun(Pid) -> 
						receive
							{Pid, Val} -> Val
						end
			  end, Pids).

%---------------------=== PARMAP HOF===--------------------------------------

parmap_hof(L) ->
	Pid = self(),
	lists:foreach(fun(X) -> spawn(fun() -> Pid ! fib(X) end) end, L),
	lists:map(fun(_) -> receive
							Val -> Val
						end
			  end, L).

%---------------------=== PARMAP LIST COMPREHENSION ===--------------------------------------

parmap_list_comprehension(L) ->
	Pid = self(),
	lists:foreach(fun(X) -> spawn(fun() -> Pid ! fib(X) end) end, L),
    [ receive Val -> Val end|| _ <- L].

%---------------------=== ORDPARMAP LIST COMPREHENSION ===--------------------------------------

ordparmap_list_comprehension(L) ->
	MyPid = self(),
	Pids = lists:map(fun(X) -> spawn(fun() -> MyPid ! {self(), fib(X)} end) end, L),
	[ receive {Pid, Val} -> Val end|| Pid <- Pids].