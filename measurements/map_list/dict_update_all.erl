-module(dict_update_all).
-export([generate_input/1, map/1, fold/1]).
% -compile(export_all).

generate_input(Size) ->
	[dict:from_list(generate_list(Size))].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

map(Dict) -> dict:map(fun(_,V) ->  V + 1 end, Dict).

fold(Dict) -> dict:fold(fun(K, V, Acc) -> dict:store(K, V + 1, Acc) end, dict:new(), Dict).
