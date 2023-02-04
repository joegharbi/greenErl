-module(test).

recursive(Key, List) ->
    Result = lists:keytake(Key, 1, List),
    recursive(Key, List).
