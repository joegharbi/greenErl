-module(list).

-export([generate_input/1, send/2, send_elements/2]).

generate_input(ListSize) -> 
  [generate_list(ListSize), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS

send_elements(_, _, 0) -> ok;
send_elements(Pid, L, Times) ->
  [Pid ! Element || Element <- L],
  Pid ! co,
  receive
    co -> send_elements(Pid, L, Times-1)
  end.
send_elements(L, Times) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  send_elements(Pid,L,Times),
  Pid ! ok,
  receive
    ok -> ok
  end.

receive_elements(From) ->
    receive
      ok            -> From ! ok;
      co            -> From ! co, receive_elements(From);
      Element       -> Element, receive_elements(From)
    end.
