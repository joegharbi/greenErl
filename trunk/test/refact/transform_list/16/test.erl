-module(test).

recursive(Key, List, 0) ->
    List;
recursive(Key, List, N) ->
    lists:keyfind(Key, 1, List),
    recursive(Key, List, N - 1).  