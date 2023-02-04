-module(test).

recursive(Key, List) ->
    {value, _, NewList} = lists:keytake(Key, 1, List),
    NewList ++ List,
    recursive(Key, NewList).
