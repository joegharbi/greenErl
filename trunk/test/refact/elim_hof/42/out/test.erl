-module(test).
-export([f/2]).

f(List, Y) ->
    X = 2,
    Z = 3,
    A = 4,
    filter_recursion1(List, X, Y, Z).
filter_recursion1([], _, _, _) -> [];
filter_recursion1([Elem | Tail1], X, Y, Z) when Elem =:= 42 ->
    Z = X + Y,
    Elem + Z,
    case 5 =:= 2 of
        true -> [Elem | filter_recursion1(Tail1, X, Y, Z)];
        false -> filter_recursion1(Tail1, X, Y, Z)
    end;
filter_recursion1([X | Tail1], X2, Y, Z) ->
    case X1 = X =:= Y of
        true -> [X | filter_recursion1(Tail1, X2, Y, Z)];
        false -> filter_recursion1(Tail1, X2, Y, Z)
    end;
filter_recursion1([{X, Y} | Tail1], X1, Y1, Z) ->
    case false of
        true -> [{X, Y} | filter_recursion1(Tail1, X1, Y1, Z)];
        false -> filter_recursion1(Tail1, X1, Y1, Z)
    end;
filter_recursion1([Elem1 | Tail1], X, Y, Z) ->
    case true of
        true -> [Elem1 | filter_recursion1(Tail1, X, Y, Z)];
        false -> filter_recursion1(Tail1, X, Y, Z)
    end;
filter_recursion1([Elem1 = {X, _} | Tail1], X1, Y, Z) ->
    case false of
        true -> [Elem1 | filter_recursion1(Tail1, X1, Y, Z)];
        false -> filter_recursion1(Tail1, X1, Y, Z)
    end.
