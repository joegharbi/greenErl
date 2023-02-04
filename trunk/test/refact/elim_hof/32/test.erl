-module(test).

f(List) ->
    lists:filter(fun({Elem1, Elem2}) -> Elem1 rem 2 =:= 0;
                    (X) -> X end, List).
