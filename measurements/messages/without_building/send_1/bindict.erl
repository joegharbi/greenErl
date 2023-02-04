-module(bindict).

-export([generate_input/1, send/1]).

generate_input(ListSize) ->
	[term_to_binary(dict:from_list(generate_list(ListSize)))].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

send(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> rec(MyPid) end),
  Pid ! L,  
  receive
    ok -> ok
  end.

rec(From) ->
    receive
      L   -> L, From ! ok
    end.