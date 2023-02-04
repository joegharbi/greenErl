-module(list_build).
-export([generate_input/1, from_map/1, iterated_from_map/1]).
% -compile(export_all).

generate_input(ListSize) ->
	[maps:from_list(generate_list(ListSize))].
	
generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

from_map(Map) -> maps:to_list(Map).

iterated_from_map(Map) -> iterated_from_map2(maps:iterator(Map)).
iterated_from_map2(none) -> [];
iterated_from_map2(It) -> 
	{K, V, Next} = maps:next(It),
	[{K, V} | iterated_from_map2(Next)].
