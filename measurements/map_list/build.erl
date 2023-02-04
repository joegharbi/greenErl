-module(build).
-export([generate_input/1, map_put/1, map_pattern/1, list_recursive/1, map_from_list/1, list_from_map/1]).
% -compile(export_all).

generate_input(N) -> [N].

map_put(0) -> #{};
map_put(N) -> maps:put(N, N, map_put(N - 1)).

map_pattern(0) -> #{};
map_pattern(N) -> (map_pattern(N - 1))#{N => N}.

list_recursive(0) -> [];
list_recursive(N) -> [{N, N} | list_recursive(N - 1)].

map_from_list(N) -> maps:from_list(list_recursive(N)).

list_from_map(N) -> maps:to_list(map_put(N)).

dict_from_list(N) -> dict:from_list(list_recursive(N)).

dict_store(0) -> dict:new();
dict_store(N) -> dict:store(N, N, dict_store(N - 1)).