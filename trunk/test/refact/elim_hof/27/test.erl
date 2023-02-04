-module(test).

f(List) ->
    X = 2,
    lists:map(fun(X) -> X + 1 end, List).
