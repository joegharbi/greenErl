-module(test).

f(List) ->
    lists:map(fun({Elem, _}) -> Elem + 1;
                  (_) -> 0 end, List).
