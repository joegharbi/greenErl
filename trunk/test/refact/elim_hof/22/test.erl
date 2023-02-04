-module(test).

f(List) ->
    lists:map(fun(Elem) -> Elem + 1 end, List).
