-module(test).

f(List) ->
    lists:filter(fun(X) when X =:= ok -> true; (Y) -> Y =:= 2 end, List).
