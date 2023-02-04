-module(test).

f(List) ->
    lists:filter(fun(Elem) -> X = Elem + 1, X rem 2 =:= 0 end, List).
