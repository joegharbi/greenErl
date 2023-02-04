-module(test).

recursive(Key, List) ->
    lists:keystore(Key + 1, 1, List, {Key + 1, a}),
    recursive(Key, List).
