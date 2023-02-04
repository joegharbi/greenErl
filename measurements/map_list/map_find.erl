-module(map_find).
-export([generate_input/1, find/3, find_pattern_match/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, maps:from_list(generate_list(Size)), 1000000].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

find(_, _, 0) -> ok;
find(Key, Map, N) -> 
	maps:find(Key, Map),
	find(Key, Map, N - 1).

find_pattern_match(_, _, 0) -> ok;
find_pattern_match(Key, Map, N) ->
	#{Key := Value} = Map,
	Value,
	find_pattern_match(Key, Map, N - 1).
