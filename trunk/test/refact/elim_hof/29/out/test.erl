-module(test).

f(List) ->
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([Elem | Tail1]) when Elem =:= 42 -> [ok | map_recursion1(Tail1)];
map_recursion1([{Elem1, Elem2} | Tail1]) when Elem1 =:= Elem2 ->
    [nok | map_recursion1(Tail1)];
map_recursion1([_ | Tail1]) -> [error | map_recursion1(Tail1)].
