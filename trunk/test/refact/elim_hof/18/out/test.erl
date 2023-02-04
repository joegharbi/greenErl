-module(test).

increase(X) -> X + 1.

f(List) ->
    map_recursion2(List).

map_recursion1(_) -> haha.
map_recursion2([]) -> [];
map_recursion2([Head | Tail]) -> [increase(Head) | map_recursion2(Tail)].
