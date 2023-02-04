-module(keytake_test).

main(LM, KM) ->
    recursive(LM, KM, 100).

recursive(_Key, ListR, 0) -> ListR;
recursive(Key, List, N) ->
    lists:keytake(Key, 1, List),
    lists:keytake(Key + 1, 1, List),
    {_, {K, V}, NewList} = lists:keytake(Key, 1, List),
    lists:keyfind(Key + 2, 1, NewList),
    recursive(Key, NewList, N - 1).
