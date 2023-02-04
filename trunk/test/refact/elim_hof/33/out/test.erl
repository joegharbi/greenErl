-module(test).

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Elem | Tail1]) ->
    X = Elem + 1,
    case X rem 2 =:= 0 of
        true -> [Elem | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end.
