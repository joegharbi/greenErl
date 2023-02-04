-module(test).

f(List) ->
    lists:filter(fun(X) -> X rem 2 =:= 0 end, List).
