-module(dictionary).

-export([generate_input/1, send/1, send_elements/1, send_elements_acc/1]).

generate_input(ListSize) ->
	[dict:from_list(generate_list(ListSize))].

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
  dict:fold(fun(K, V, _) ->
		        Pid ! {K, V} end, 
            ok,
            L),  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_elements(From) ->
  L = receivedict_elements(From),
  From ! ok.

receivedict_elements(From) ->
  receive
    {From,ok}   -> dict:new();
    {Key,Value} -> dict:store(Key, Value, receivedict_elements(From))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS ACC

send_elements_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements_acc(MyPid) end),
  dict:fold(fun(K, V, _) ->
		        Pid ! {K, V} end, 
            ok,
            L),  Pid ! {self(),ok},
  receive
    ok -> ok
  end.
  
  
receive_elements_acc(From) -> receive_elements_acc(From, dict:new()).
receive_elements_acc(From, Sum) -> 
  receive
    {From,ok}       -> Sum, From ! ok;
    {K,V}           -> receive_elements_acc(From, dict:store(K, V, Sum))
  end.