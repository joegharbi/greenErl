-module(dict_build).
-export([generate_input/1, from_list/1, recursive_from_list/1]).
% -compile(export_all).

generate_input(ListSize) ->
	[generate_list(ListSize)].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

from_list(List) -> dict:from_list(List).

recursive_from_list([]) -> dict:new();
recursive_from_list([{K, V} | T]) -> dict:store(K, V, recursive_from_list(T)).

