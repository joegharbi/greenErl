-module(test).

f(List) ->
    lists:filter(fun lists:last/1, List).
