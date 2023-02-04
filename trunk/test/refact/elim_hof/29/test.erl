-module(test).

f(List) ->
    lists:map(fun(Elem) when Elem =:= 42 -> ok;
                 ({Elem1, Elem2}) when Elem1 =:= Elem2 -> nok;
                 (_) -> error end, List).
