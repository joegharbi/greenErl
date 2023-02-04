-module(test).

f(List) ->
    lists:map(fun lists:last/1, List).
