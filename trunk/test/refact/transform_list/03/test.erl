-module(test).

recursive(List) ->
    K = key,
    {K, V} = lists:keyfind(K, 1, List),
    recursive(List).  
