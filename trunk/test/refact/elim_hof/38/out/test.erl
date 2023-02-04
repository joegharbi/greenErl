-module(test).

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Elem | Tail1]) when Elem =:= 42 ->
    case true of
        true -> [Elem | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end;
filter_recursion1([{Elem1, Elem2} | Tail1]) when Elem1 =:= Elem2 ->
    case false of
        true -> [{Elem1, Elem2} | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end;
filter_recursion1([Elem1 | Tail1]) ->
    case true of
        true -> [Elem1 | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end.
