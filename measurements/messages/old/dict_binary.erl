-module(dict_binary).

-export([generate_input/1, send/2]).

generate_input(ListSize) ->
	[term_to_binary(dict:from_list(generate_list(ListSize))), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

send(Pid,_,0) -> Pid ! {self(),ok};
send(Pid, L, Times) -> 
  Pid ! {self(),L},
  receive
    ok -> send(Pid, L, Times - 1)
  end.

send(L, Times) -> 
  Pid = spawn(fun rec/0),
  send(Pid, L, Times).

rec() ->
    receive
      {_, ok} -> ok;
      {From, L} -> From ! ok, rec()
    end.