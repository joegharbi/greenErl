-module(test).

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([{Elem1, Elem2} | Tail1]) ->
    case Elem1 rem 2 =:= 0 of
        true -> [{Elem1, Elem2} | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end;
filter_recursion1([X | Tail1]) ->
    case X of
        true -> [X | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end.
