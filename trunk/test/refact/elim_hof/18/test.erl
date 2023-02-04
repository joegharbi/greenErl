-module(test).

increase(X) -> X + 1.

f(List) ->
    lists:map(fun increase/1, List).

map_recursion1(_) -> haha.
