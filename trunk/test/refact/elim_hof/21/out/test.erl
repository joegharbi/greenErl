-module(test).

even(X) -> X rem 2 =:= 0.

f(List) ->
    filter_recursion2(List).

filter_recursion1(_) -> haha.
filter_recursion2([]) -> [];
filter_recursion2([Head | Tail]) ->
    case even(Head) of
        true -> [Head | filter_recursion2(Tail)];
        false -> filter_recursion2(Tail)
    end.
