-module(test).

f(List) ->
    Elem = 2,
    lists:filter(fun({X, _}) -> X + Elem =:= 42;
                    ({Elem, _}) -> Elem =:= 16;
                    (_) -> Elem =:= 16 end, List).
