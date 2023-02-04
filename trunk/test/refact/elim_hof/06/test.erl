-module(test).

f(List) ->
    lists:map(fun(X) when X =:= ok -> ok; (Y) -> Y end, List).
