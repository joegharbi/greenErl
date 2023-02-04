-module(test).

recursive(Key, Tuple, List) ->
    lists:keystore(Key, 1, List, Tuple),
    recursive(Key, Tuple, List).
