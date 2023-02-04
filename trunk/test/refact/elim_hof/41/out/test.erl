-module(test).

f(List) ->
    Elem = 2,
    filter_recursion1(List, Elem).
filter_recursion1([], _) -> [];
filter_recursion1([Elem1 = {X, _} | Tail1], Elem) ->
    case X + Elem =:= 42 of
        true -> [Elem1 | filter_recursion1(Tail1, Elem)];
        false -> filter_recursion1(Tail1, Elem)
    end;
filter_recursion1([Elem2 = {Elem, _} | Tail1], Elem1) ->
    case Elem =:= 16 of
        true -> [Elem2 | filter_recursion1(Tail1, Elem1)];
        false -> filter_recursion1(Tail1, Elem1)
    end;
filter_recursion1([Elem1 | Tail1], Elem) ->
    case Elem =:= 16 of
        true -> [Elem1 | filter_recursion1(Tail1, Elem)];
        false -> filter_recursion1(Tail1, Elem)
    end.
