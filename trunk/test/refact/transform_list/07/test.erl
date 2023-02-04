-module(test).

recursive(List) ->
    K = 1,
    lists:keyfind(K + 1, 1, List),
    recursive(List).  
