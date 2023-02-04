-module(list_chunks_generated).

-export([generate_input/1, send/1, send_comp/1, send_acc/1]).

generate_input(ListSize) -> 
  [n_length_chunks(generate_list(ListSize),250)].

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
  Pid = spawn(fun() -> receive_chunks(MyPid) end),
  [Pid ! Chunk || Chunk <- L],
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_chunks(From) ->
  L = receivelist_chunks(From),
  lists:flatten(L),
  From ! ok.

receivelist_chunks(From) ->
  receive
    {From,ok} -> [];
    Chunk     -> [Chunk | receivelist_chunks(From)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS LIST COMPREHENSION

send_comp(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks_comp(MyPid) end),
  Length = length(L),
  Pid ! Length,
  [Pid ! Chunk || Chunk <- L],
  receive
    ok -> ok
  end.

receive_chunks_comp(From) ->
  receive
    Length -> Length
  end,
  L = [receive Element -> Element end || _ <- lists:seq(1,Length)],
  lists:flatten(L),
  From ! ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS ACC

send_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks_acc(MyPid) end),
  [Pid ! Chunk || Chunk <- L],
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_chunks_acc(From) -> receive_chunks_acc(From, []).
receive_chunks_acc(From, Sum) -> 
  receive
    {From,ok}     -> lists:flatten(Sum), From ! ok;
    Chunk         -> receive_chunks_acc(From, [Chunk | Sum])
  end.