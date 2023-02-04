-module(test).

f(List) ->
    X = 2,
    map_recursion1(List, X).
map_recursion1([], _) -> [];
map_recursion1([{Elem, _} | Tail1], X) -> [Elem + X | map_recursion1(Tail1, X)];
map_recursion1([X | Tail1], X1) -> [X | map_recursion1(Tail1, X1)].
