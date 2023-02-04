-module(list).

-export([generate_input/1, send/1,send_elements_rec/1, send_elements_acc/1, send_elements_comp/1, send_chunks/1,send_chunks_comp/1, send_chunks_acc/1]).

generate_input(ListSize) -> 
  [generate_list(ListSize)].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].
    
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

send_elements_rec(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  [Pid ! Element || Element <- L],
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_elements(From) ->
  L = receivelist_elements(From),
  From ! ok.

receivelist_elements(From) ->
  receive
    {From,ok} -> [];
    Element   -> [Element | receivelist_elements(From)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS LIST COMPREHENSION

send_elements_comp(L) ->
  MyPid = self(),
  Length = length(L),
  Pid = spawn(fun() -> receive_elements_comp(MyPid) end),
  Pid ! Length,
  [Pid ! Element || Element <- L],
  receive
    ok -> ok
  end.

receive_elements_comp(From) ->
  receive
    Length -> Length
  end,
  L = [receive Element -> Element end || _ <- lists:seq(1,Length)],
  From ! ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS ACC

send_elements_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements_acc(MyPid) end),
  [Pid ! Element || Element <- L],
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.  
  
receive_elements_acc(From) -> receive_elements_acc(From, []).
receive_elements_acc(From, Sum) -> 
  receive
    {From,ok}       -> lists:reverse(Sum), From ! ok;
    Element         -> receive_elements_acc(From, [Element | Sum])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS

send_chunks(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks(MyPid) end),
  [Pid ! Chunk || Chunk <- n_length_chunks(L,250)],
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

send_chunks_comp(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks_comp(MyPid) end),
  Chunks = n_length_chunks(L,250),
  Length = length(Chunks),
  Pid ! Length,
  [Pid ! Chunk || Chunk <- Chunks],
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

send_chunks_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks_acc(MyPid) end),
  [Pid ! Chunk || Chunk <- n_length_chunks(L,250)],
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
