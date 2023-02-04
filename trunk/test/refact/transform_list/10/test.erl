-module(test).

recursive() ->
    List = [{key, 1},{otherkey, 2}],
    lists:keyfind(key, 1, List),
    recursive().  
