-module(test).

f(List) ->
    Elem = 5,
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Elem | Tail1]) ->
    case Elem rem 2 =:= 0 of
        true -> [Elem | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end.
