-module(test).

f(List, Z) ->
    X = 2,
    Y = 3,
    lists:map(fun(Elem) -> A = X + Y + Z, Elem + A end, List).
