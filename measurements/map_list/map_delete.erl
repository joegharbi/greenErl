-module(map_delete).
-export([generate_input/1, remove/3, take/3, without/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, maps:from_list(generate_list(Size)), 1000000].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

remove(_, _, 0) -> ok;
remove(Key, Map, N) -> 
	maps:remove(Key, Map),
	remove(Key, Map, N - 1).

take(_, _, 0) -> ok;
take(Key, Map, N) -> 
	{_, NewMap} = maps:take(Key, Map),
	NewMap,
	take(Key, Map, N - 1).

without(_, _, 0) -> ok;
without(Key, Map, N) -> 
	maps:without([Key], Map),
	without(Key, Map, N - 1).
