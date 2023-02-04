-module(test).

f(List) ->
    lists:map(fun(X) -> Y = X + 1, Y - 2 end, List).
