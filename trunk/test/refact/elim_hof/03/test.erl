-module(test).

f(List) ->
    lists:map(fun(X) -> X + 1 end, List).
