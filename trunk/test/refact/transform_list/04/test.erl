-module(test).

recursive(List) ->
    K = 1,
    {NewKey, V} = lists:keyfind(K + 1, 1, List),
    recursive(List).  
