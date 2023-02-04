-module(test).

recursive(Key, List) ->
    lists:keymember(Key, 1, List),
    recursive(Key, List).
