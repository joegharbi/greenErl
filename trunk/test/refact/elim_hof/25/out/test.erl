-module(test).

f(List, Z) ->
    X = 2,
    Y = 3,
    map_recursion1(List, X, Y, Z).
map_recursion1([], _, _, _) -> [];
map_recursion1([Elem | Tail1], X, Y, Z) ->
    A = X + Y + Z,
    [Elem + A | map_recursion1(Tail1, X, Y, Z)].
