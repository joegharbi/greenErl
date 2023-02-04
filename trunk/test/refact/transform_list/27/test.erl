-module(test).

recursive(Key, List) ->
    lists:keystore(Key, 1, List, {Key, a}),
    recursive(Key, List).
