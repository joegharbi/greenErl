-module(test).

f(List) ->
    X = 2,
    lists:map(fun({Elem, _}) -> Elem + X;
                 (X) -> X end, List).
