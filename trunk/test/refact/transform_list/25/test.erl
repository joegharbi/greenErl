-module(test).

recursive(Key, List) ->
    recursive(Key, List),
    lists:keytake(Key, 1, List).
