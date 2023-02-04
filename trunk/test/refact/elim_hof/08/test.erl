-module(test).

even(X) -> X rem 2 =:= 0.

f(List) ->
    lists:filter(fun even/1, List).
