-module(hof_test).

even(X) -> X rem 2 =:= 0.
increase(X) -> X + 1.

main(List) ->
    [Elem1 || Elem1 <- List],
    Elem1 = 2,
    map_recursion1(List),
    lists:map(fun lists:last/1, List),
    ok,
    lists:map(fun(Elem) -> Elem + 1 end, List),
    map_recursion3(List),
    lists:map(fun(X) when X =:= 1 -> 1 end, List),
    map_recursion2(List),
    BoundVar = 2,
    lists:map(fun(BoundVar) -> 1 end, List),
    lists:map(fun(_) -> ok end, List),
    lists:map(fun(ok) -> ko end, List),
    lists:filter(fun even/1, List),
    lists:filter(fun lists:last/1, List),
    lists:filter(fun({_, X}) -> X end, List).


%other_main(List) ->
%    [increase(NewElem) || NewElem<-List],
%    [lists:last(NewElem) || NewElem<-List].
map_recursion1([]) -> [];
map_recursion1([Head | Tail]) -> [increase(Head) | map_recursion1(Tail)].
map_recursion2([]) -> [];
map_recursion2([X | Tail1]) when X =:= 1 -> [1 | map_recursion2(Tail1)];
map_recursion2([Y | Tail1]) -> [Y + 1 | map_recursion2(Tail1)].
map_recursion3([]) -> [];
map_recursion3([Elem | Tail1]) ->
    X = Elem + 1,
    [X + 1 | map_recursion3(Tail1)].
