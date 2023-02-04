-module(test).

f(List) ->
    lists:map(fun(ok) -> ok; (_) -> error end, List).
