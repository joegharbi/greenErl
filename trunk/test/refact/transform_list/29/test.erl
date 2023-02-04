-module(test).

recursive(Key1, Key2, List) ->
    lists:keystore(Key1, 1, List, {Key2, a}),
    recursive(Key1, Key2, List).
