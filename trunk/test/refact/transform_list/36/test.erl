-module(test).

recursive(Key, []) when Key < 10 -> ok;
recursive(Key, List) ->
    lists:keydelete(Key, 1, List),
    recursive(Key + 1, List).
