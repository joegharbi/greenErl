-module(test).

recursive(List) ->
    K = key,
    {NewKey, V} = lists:keyfind(K, 1, List),
    recursive(List).  
