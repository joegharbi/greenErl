-module(test).

f(List) ->
    lists:filter(fun(_) -> Y = 41 + 1, Y =:= 42 end, List).
