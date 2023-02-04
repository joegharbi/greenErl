-module(map).

-export([generate_input/1, send/1, send_elements/1, send_elements_acc/1]).

generate_input(ListSize) ->
	[maps:from_list(generate_list(ListSize))].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND

send(L) ->
  Pid = spawn(fun rec/0),
  Pid ! {self(),L},
  receive
    ok -> ok
  end.

rec() ->
  receive
    {From, L} ->  L,
                  From ! ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS

send_elements(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  maps:fold(fun(K, V, _) ->
		        Pid ! {K, V} end, 
            ok,
            L),
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_elements(From) ->
  L = receivemap_elements(From),
  From ! ok.

receivemap_elements(From) ->
  receive
    {From,ok} -> #{};
    {K,V}     -> maps:put(K, V, receivemap_elements(From))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS ACC

send_elements_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements_acc(MyPid) end),
  maps:fold(fun(K, V, _) ->
		        Pid ! {K, V} end, 
            ok,
            L),
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.
  
receive_elements_acc(From) -> receive_elements_acc(From, #{}).
receive_elements_acc(From, Sum) -> 
  receive
    {From,ok}       -> Sum, From ! ok;
    {K,V}           -> receive_elements_acc(From, maps:put(K, V, Sum))
  end.