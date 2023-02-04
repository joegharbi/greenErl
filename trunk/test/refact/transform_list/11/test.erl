-module(test).

recursive(Key, List) ->
    lists:keyfind(Key, 1, List),
    recursive(Key, List).  
