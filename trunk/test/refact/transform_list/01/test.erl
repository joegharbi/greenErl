-module(test).

recursive(List) ->
    K = key,
    {_, V} = lists:keyfind(K, 1, List),
    recursive(List). 
