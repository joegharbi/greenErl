-module(test).

f(List) ->
    lists:filter(fun({X, _}) -> X end, List).
