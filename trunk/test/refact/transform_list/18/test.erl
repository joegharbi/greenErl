-module(test).

recursive(Key, List, 0) ->
    {K, V} = lists:keyfind(Key, 1, List);
recursive(Key, List, N) ->
    recursive(Key, List, N - 1).  