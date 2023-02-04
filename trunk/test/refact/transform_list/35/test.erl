-module(test).

recursive(Key, List) ->
    recursive(Key, List),
    lists:keydelete(Key, 1, List).
