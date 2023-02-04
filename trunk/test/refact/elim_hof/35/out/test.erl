-module(test).

f(List, Z) ->
    X = 2,
    Y = 3,
    Unused = 5,
    filter_recursion1(List, X, Y, Z).
filter_recursion1([], _, _, _) -> [];
filter_recursion1([Elem | Tail1], X, Y, Z) ->
    A = Elem + X + Y + Z,
    case A rem 2 =:= 0 of
        true -> [Elem | filter_recursion1(Tail1, X, Y, Z)];
        false -> filter_recursion1(Tail1, X, Y, Z)
    end.
