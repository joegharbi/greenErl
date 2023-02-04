-module(test).

f(List) ->
    lists:filter(fun(Elem) when Elem =:= 42 -> true;
                 ({Elem1, Elem2}) when Elem1 =:= Elem2 -> false;
                 (_) -> true end, List).
