-module(test).

recursive(Key, List) ->
    lists:keytake(Key, 1, List),
    recursive(Key, List).