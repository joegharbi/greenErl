-module(test).

f(List) ->
    Elem = 5,
    lists:filter(fun(X) -> Y = Elem + X, Y rem 2 =:= 0;
                 (Elem) -> Elem + 1 =:= 2 end, List).
