-module(test).

recursive(List) ->
    K = key,
    Result = lists:keyfind(K, 1, List),
    recursive(List).  
