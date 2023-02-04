-module(test).

f(List) ->
    lists:filter(fun(ok) -> true; (_) -> false end, List).
