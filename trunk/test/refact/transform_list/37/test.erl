-module(test).

recursive(_Key, []) -> ok;
recursive(Key, List) ->
    lists:keydelete(Key, 1, List),
    recursive(Key + 1, List).
