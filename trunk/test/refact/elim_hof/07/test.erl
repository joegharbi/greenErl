-module(test).

f(List) ->
    lists:map(fun(_) -> 1 end, List).
