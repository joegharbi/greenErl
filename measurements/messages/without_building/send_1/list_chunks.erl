-module(list_chunks).

-export([generate_input/1, send/1]).

generate_input(ListSize) -> 
  [n_length_chunks(generate_list(ListSize),1000)].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS

send(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  [Pid ! Chunk || Chunk <- L],
  Pid ! co,
  receive
    ok -> ok
  end.

receive_elements(From) ->
    receive
      co            -> From ! ok;
      Chunk         -> Chunk, receive_elements(From)
    end.