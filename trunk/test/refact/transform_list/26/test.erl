-module(test).

recursive(Key, List) ->
    recursive(Key, List),
    Result = lists:keytake(Key, 1, List).
