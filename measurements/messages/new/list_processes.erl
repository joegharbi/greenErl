-module(list_processes).

-export([generate_input/1, send/2,send_elements/2, send_chunks/2]).

generate_input(ListSize) -> 
  [generate_list(ListSize), 500].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND

send(L,K) ->
  J = lists:seq(1,K),
  MyPid = self(),
  [ spawn( fun() -> sender(L, MyPid) end) || _ <- J],
  [ receive ok -> ok end || _ <- J].

sender(L, PPid) ->
  Pid = spawn(fun rec/0),
  Pid ! {self(),L},
  receive
    ok -> PPid ! ok
  end.

rec() ->
  receive
    {From, L} ->  L,
                  From ! ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND ELEMENTS

send_elements(L,K) ->
  J = lists:seq(1,K),
  MyPid = self(),
  [ spawn( fun() -> sender_elements(L, MyPid) end) || _ <- J],
  [ receive ok -> ok end || _ <- J].

sender_elements(L,PPid) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_elements(MyPid) end),
  [Pid ! Element || Element <- L],
  Pid ! {self(),ok},
  receive
    ok -> PPid ! ok
  end.

receive_elements(From) ->
  L = receivelist_elements(From),
  From ! ok.

receivelist_elements(From) ->
  receive
    {From,ok} -> [];
    Element   -> [Element | receivelist_elements(From)]
  end.

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

send_chunks(L,K) ->
  J = lists:seq(1,K),
  MyPid = self(),
  [ spawn( fun() -> sender_chunks(L, MyPid) end) || _ <- J],
  [ receive ok -> ok end || _ <- J].

sender_chunks(L,PPid) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks(MyPid) end),
  [Pid ! Chunk || Chunk <- n_length_chunks(L,250)],
  Pid ! {self(),ok},
  receive
    ok -> PPid ! ok
  end.

receive_chunks(From) ->
  L = receivelist_chunks(From),
  From ! ok.

receivelist_chunks(From) ->
  receive
    {From,ok} -> [];
    Chunk     -> Chunk ++ receivelist_chunks(From)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SEND CHUNKS ACC

send_chunks_acc(L) ->
  MyPid = self(),
  Pid = spawn(fun() -> receive_chunks_acc(MyPid) end),
  [Pid ! Chunk || Chunk <- n_length_chunks(L,1000)],
  Pid ! {self(),ok},
  receive
    ok -> ok
  end.

receive_chunks_acc(From) -> receive_chunks_acc(From, []).
receive_chunks_acc(From, Sum) -> 
  receive
    {From,ok}     -> Sum, From ! ok;
    Chunk         -> receive_chunks_acc(From, Sum ++ Chunk)
  end.
