-module(test).

f(List) ->
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([Head | Tail]) -> [lists:last(Head) | map_recursion1(Tail)].
