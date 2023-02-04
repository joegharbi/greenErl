-module(hof_after_rec).
-export([generate_input/1, f/1]).

generate_input(N) -> [lists:seq(1, N)].

do_stuff(X) ->
    A = X,
    B = X*X,
    C = X*X*X,
    A + B + C.

even(X) -> X rem 2 =:= 0.

f(List) ->
    Y = 42,
    NewList = map_recursion1(List, Y),
    NewList2 = filter_recursion1(NewList, Y),
    map_recursion2(NewList2).
map_recursion1([], _) -> [];
map_recursion1([X | Tail1], Y) ->
    [case even(X) of
        true -> Y;
        false -> X
    end | map_recursion1(Tail1, Y)].
filter_recursion1([], _) -> [];
filter_recursion1([X | Tail1], Y) ->
    case X =/= Y of
        true -> [X | filter_recursion1(Tail1, Y)];
        false -> filter_recursion1(Tail1, Y)
    end.
map_recursion2([]) -> [];
map_recursion2([Head | Tail]) -> [do_stuff(Head) | map_recursion2(Tail)].
