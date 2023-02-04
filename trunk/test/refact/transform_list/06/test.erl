-module(test).

recursive(List) ->
    K = key,
    lists:keyfind(K, 1, List),
    recursive(List).  
