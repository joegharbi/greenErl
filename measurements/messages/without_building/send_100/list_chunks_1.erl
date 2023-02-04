-module(list_chunks_1).

-export([generate_input/1, send/2]).

generate_input(ListSize) -> 
  [n_length_chunks(generate_list(ListSize),1),100].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS

send(_, _, 0) -> ok;
send(Pid, L, Times) ->
  [Pid ! Chunk || Chunk <- L],
  Pid ! co,
  receive
    co -> send(Pid, L, Times-1)
  end.
send(L, Times) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  send(Pid,L,Times),
  Pid ! ok,
  receive
    ok -> ok
  end.

receive_elements(From) ->
    receive
      ok            -> From ! ok;
      co            -> From ! co, receive_elements(From);
      Chunk         -> Chunk, receive_elements(From)
    end.