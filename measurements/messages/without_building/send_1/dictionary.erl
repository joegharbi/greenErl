-module(dictionary).

-export([generate_input/1, send/1, send_elements/1,send_elements_map/1]).

generate_input(ListSize) ->
	[dict:from_list(generate_list(ListSize))].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS

send_elements(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  dict:fold(fun(K, V, _) ->
		        Pid ! {K, V} end, 
            ok,
            L),
  Pid ! co,
  receive
    ok -> ok
  end.

receive_elements(From) ->
    receive
      co            -> From ! ok;
      {Key,Value}   -> Key, Value, receive_elements(From)
    end.


send_elements_map(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  dict:map(fun(K, V) ->
		        Pid ! {K, V} end,
            L),
  Pid ! co,
  receive
    ok -> ok
  end.