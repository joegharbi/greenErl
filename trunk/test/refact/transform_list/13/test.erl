-module(test).

non_recursive(Key, List) ->
    lists:keyfind(Key, 1, List).  
