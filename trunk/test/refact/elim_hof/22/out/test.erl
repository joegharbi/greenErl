-module(test).

f(List) ->
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([Elem | Tail1]) -> [Elem + 1 | map_recursion1(Tail1)].
