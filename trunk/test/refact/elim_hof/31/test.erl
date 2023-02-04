-module(test).

f(List) ->
    lists:filter(fun(Elem) -> Elem rem 2 =:= 0 end, List).
