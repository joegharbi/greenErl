-module(test).

recursive(List) ->
    lists:keyfind(42, 1, List),
    recursive(List).  
