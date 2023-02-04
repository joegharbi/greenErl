-module(test).

recursive(List) ->
    lists:keyfind(key, 1, List),
    recursive(List).  
