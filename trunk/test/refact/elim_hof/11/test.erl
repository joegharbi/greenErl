-module(test).

f(List) ->
    lists:filter(fun(X) -> Y = X + 1, Y =:= 42 end, List).
