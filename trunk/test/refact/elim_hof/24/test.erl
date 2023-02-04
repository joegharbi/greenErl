-module(test).

f(List) ->
    lists:map(fun(Elem) -> X = Elem + 1,
                           X + 1 end, List).
