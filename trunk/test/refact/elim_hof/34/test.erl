-module(test).

f(List, Z) ->
    X = 2,
    Y = 3,
    lists:filter(fun(Elem) -> A = Elem + X + Y + Z, A rem 2 =:= 0 end, List).
