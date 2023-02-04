-module(test).

recursive(Key, List) ->
    {value, _, NewList} = lists:keytake(Key, 1, List),
    {K, V} = lists:keyfind(Key, 1, NewList),
    recursive(Key, NewList).
