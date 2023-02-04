-module(ring).
-compile(export_all).

ring(N, L) ->
    Pid1 = lists:foldl(fun(_, Pid) -> spawn(fun() -> stage(Pid) end) end, 
		       self(),
		       lists:seq(1,N)),
    [Pid1 ! {forward, H} || H <- L],
    Pid1 ! finished,
    [receive
	{forward, D} ->
	    D
    end || _ <- L].

stage(NextPid)->
    receive
	{forward, Data} ->
	    NewData = process(Data),
	    NextPid ! {forward, NewData},
	    stage(NextPid);
	finished ->
	    NextPid ! finished
    end.

process(Data) ->
    Data + 1.
