-module(test).

f(List) ->
    lists:filter(fun(_) -> true end, List).
