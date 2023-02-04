-module(binmap).

-export([generate_input/1, send/2]).

generate_input(ListSize) ->
	[term_to_binary(maps:from_list(generate_list(ListSize))), 1000].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

send(_, _, 0)       -> ok;
send(Pid, L, Times) ->
  Pid ! L,
  receive
    co -> send(Pid, L, Times-1)
  end.
send(L, Times) ->
  MyPid = self(),
  Pid = spawn(fun() -> rec(MyPid) end),
  send(Pid,L,Times),  
  Pid ! ok,
  receive
    ok -> ok
  end.

rec(From) ->
    receive
      ok  -> From ! ok;
      L   -> L, From ! co, rec(From)
    end.