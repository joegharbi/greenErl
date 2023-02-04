-module(test).

recursive(Key, List) ->
    NewList = List ++ List,
    lists:keyfind(Key, 1, List),
    recursive(Key, NewList).  
