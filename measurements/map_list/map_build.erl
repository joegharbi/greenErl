-module(map_build).
-export([generate_input/1, from_list/1, recursive_from_list/1, recursive_from_list_put/1]).
% -compile(export_all).

generate_input(ListSize) ->
	[generate_list(ListSize)].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

from_list(List) -> maps:from_list(List).

recursive_from_list([]) -> #{};
recursive_from_list([{K, V} | T]) -> (recursive_from_list(T))#{K => V}.

recursive_from_list_put([]) -> #{};
recursive_from_list_put([{K, V} | T]) -> maps:put(K, V, recursive_from_list_put(T)).
