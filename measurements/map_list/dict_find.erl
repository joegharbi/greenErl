-module(dict_find).
-export([generate_input/1, find/3, fetch/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, dict:from_list(generate_list(Size)), 1000000].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

find(_, _, 0) -> ok;
find(Key, Dict, N) ->
	dict:find(Key, Dict),
	find(Key, Dict, N - 1).

fetch(_, _, 0) -> ok;
fetch(Key, Dict, N) ->
	dict:fetch(Key, Dict),
	fetch(Key, Dict, N - 1).