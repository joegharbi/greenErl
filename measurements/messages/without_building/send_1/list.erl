-module(list).

-export([generate_input/1, send/1, send_elements/1]).

generate_input(ListSize) -> 
  [generate_list(ListSize)].

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
  [Pid ! Element || Element <- L],
  Pid ! co,
  receive
    ok -> ok
  end.

receive_elements(From) ->
    receive
      co            -> From ! ok;
      Element       -> Element, receive_elements(From)
    end.
