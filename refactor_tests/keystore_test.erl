-module(keystore_test).

main(LM, KM) ->
    List = [], Key = 2, Value = 3,
    List#{Key => Value},
    recursive(LM, KM, 100).

recursive(_Key, ListR, 0) -> ListR;
recursive(Key, List, N) ->
    lists:keystore(Key, 1, List, {Key, N}),
    lists:keystore(Key + 1, 1, List, {Key + 1, 2}),
    Tuple = {1,2},
    L = lists:keystore(Key, 1, List, Tuple),
    lists:keyfind(Key, 1, L),
    Key2 = 123,
    lists:keystore(Key, 1, L, {Key2, 1}),
    lists:keystore(Key, 1, L, Tuple + 1),
    recursive(Key, L, N - 1).
