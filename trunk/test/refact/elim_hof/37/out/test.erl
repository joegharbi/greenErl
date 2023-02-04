-module(test).

f(List) ->
    Elem = 5,
    filter_recursion1(List, Elem).
filter_recursion1([], _) -> [];
filter_recursion1([X | Tail1], Elem) ->
    Y = Elem + X,
    case Y rem 2 =:= 0 of
        true -> [X | filter_recursion1(Tail1, Elem)];
        false -> filter_recursion1(Tail1, Elem)
    end;
filter_recursion1([Elem | Tail1], Elem1) ->
    case Elem + 1 =:= 2 of
        true -> [Elem | filter_recursion1(Tail1, Elem1)];
        false -> filter_recursion1(Tail1, Elem1)
    end.
