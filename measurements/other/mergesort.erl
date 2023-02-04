-module(mergesort).
-compile(export_all).

%%SIMPLE RECURSIVE MERGESORT
%%https://gist.github.com/sdebnath/a0cfed12cab3a0e2d761

%%==============================================================================
%% Mergesort implementation in Erlang
%%
%% Author: Shawn Debnath
%%==============================================================================
 
%%----------------------------------------------------------------------
%% msort/1
%% 
%% Mergesort (recursive), implements split and merge.
%%----------------------------------------------------------------------
msort([]) -> [];
msort([H]) ->
    [H];
msort(List) ->
    {Front, Back} = split(List),
    merge(msort(Front), msort(Back)).

split(List) ->
    split(List, List, []).
split([], Back, Front) ->
    {lists:reverse(Front), Back};
split([_], Back, Front) ->
    {lists:reverse(Front), Back};
split([_,_ | Counter], [H | T], Result) ->
    split(Counter, T, [H | Result]).
 
merge([], Back) ->
    Back;
merge(Front, []) ->
    Front;
merge([L | Front], [R | Back]) when L < R ->
    [L | merge(Front, [R | Back])];
merge([L | Front], [R | Back]) ->
	[R | merge([L | Front], Back)].
	
%Generate N random numbers with fixed seed from 1 to N
generate(N) -> random:seed(1,1,1),
    [random:uniform(N) || _ <- lists:seq(1, N)].