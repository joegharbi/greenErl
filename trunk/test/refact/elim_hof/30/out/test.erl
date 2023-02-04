-module(test).
-export([f/2]).

f(List, Y) ->
    X = 2,
    Z = 3,
    A = 4,
    map_recursion1(List, X, Y, Z).

map_recursion1([], _, _, _) -> [];
map_recursion1([Elem | Tail1], X, Y, Z) when Elem =:= 42 ->
    Z = X + Y,
    Elem + Z,
    [Q = 2 | map_recursion1(Tail1, X, Y, Z)];
map_recursion1([X | Tail1], X2, Y, Z) ->
    [X1 = X + Y | map_recursion1(Tail1, X2, Y, Z)];
map_recursion1([{X, Y} | Tail1], X1, Y1, Z) ->
    [oh | map_recursion1(Tail1, X1, Y1, Z)].
