-module(test).

f(List) ->
    Elem = 5,
    lists:filter(fun(Elem) -> Elem rem 2 =:= 0 end, List).
