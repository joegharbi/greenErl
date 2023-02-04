-module(test).

f(List) ->
    X = 2,
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([X | Tail1]) -> [X + 1 | map_recursion1(Tail1)].
