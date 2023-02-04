-module(map_update_all).
-export([generate_input/1, map_with_map/1, map_with_iter/1, fold_with_map/1, fold_with_iter/1, recursive/1]).
% -compile(export_all).

generate_input(Size) ->
	[maps:from_list(generate_list(Size))].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

map_with_map(Map) -> maps:map(fun(_,V) ->  V + 1 end, Map).

map_with_iter(Map) -> maps:map(fun(_,V) ->  V + 1 end, maps:iterator(Map)).

fold_with_map(Map) -> maps:fold(fun(K, V, Acc) -> Acc#{K => V + 1} end, #{}, Map).

fold_with_iter(Map) -> maps:fold(fun(K, V, Acc) -> Acc#{K => V + 1} end, #{}, maps:iterator(Map)).

recursive(Map) -> recursive_iter(maps:iterator(Map)).
recursive_iter(none) -> #{};
recursive_iter(Iter) -> 
	{K, V, It} = maps:next(Iter),
	(recursive_iter(It))#{K => V + 1}.