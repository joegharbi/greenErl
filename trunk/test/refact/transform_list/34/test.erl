-module(test).

recursive(Key, List) ->
    lists:keydelete(Key, 1, List),
    recursive(Key, List).
