-module(test).

even(X) -> X rem 2 =:= 0.

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Head | Tail]) ->
    case even(Head) of
        true -> [Head | filter_recursion1(Tail)];
        false -> filter_recursion1(Tail)
    end.
