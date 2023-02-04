-module(test).

increase(X) -> X + 1.

f(List) ->
    map_recursion1(List).
map_recursion1([]) -> [];
map_recursion1([Head | Tail]) -> [increase(Head) | map_recursion1(Tail)].
