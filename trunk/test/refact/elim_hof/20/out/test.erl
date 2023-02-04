-module(test).

f(List) ->
    filter_recursion1(List).
filter_recursion1([]) -> [];
filter_recursion1([Head | Tail]) ->
    case lists:last(Head) of
        true -> [Head | filter_recursion1(Tail)];
        false -> filter_recursion1(Tail)
    end.
