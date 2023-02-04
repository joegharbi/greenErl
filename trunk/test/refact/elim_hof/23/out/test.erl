-module(test).

f(List) ->
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([{Elem, _} | Tail1]) -> [Elem + 1 | map_recursion1(Tail1)];
map_recursion1([_ | Tail1]) -> [0 | map_recursion1(Tail1)].
