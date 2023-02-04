-module(list_update_all).
-export([generate_input/1, map/1, foldl/1, foldr/1, recursive/1]).
% -compile(export_all).

generate_input(Size) ->
	[generate_list(Size)].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

map(List) -> lists:map(fun({K, V}) -> {K, V + 1} end, List).

% This reverses the order of the keys, but for dictionary it does not matter.
foldl(List) -> lists:foldl(fun({K, V}, Acc) -> [{K, V + 1} | Acc] end, [], List).

foldr(List) -> lists:foldr(fun({K, V}, Acc) -> [{K, V + 1} | Acc] end, [], List).

recursive([]) -> [];
recursive([{K, V} | T]) -> [{K, V + 1} | recursive(T)].