-module(rec).
-import(lists, [keydelete/3]).

increase(X) ->
    X + 1.

even(X) ->
    X rem 2 =:= 0.

dummy(List) ->
    lists:keyfind(1, 2, List),
    case 1 of
	2 -> lists:map(fun increase/1, List);
	3 -> lists:filter(fun even/1, List)
    end.

map_recursion1([]) ->
    [];
map_recursion1([Elem | Rest]) ->
    [increase(Elem) | map_recursion1(Rest)].
