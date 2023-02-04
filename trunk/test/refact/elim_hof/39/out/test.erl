-module(test).

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Elem1 | Tail1]) ->
    case true of
        true -> [Elem1 | filter_recursion1(Tail1)];
        false -> filter_recursion1(Tail1)
    end.
