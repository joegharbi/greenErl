-module(test).

create_tuple() -> {1,2}.

recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    lists:keystore(Key1, 1, List, {Key1, a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    lists:keystore(Key1 + 1, 1, List, {Key1 + 1, a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    lists:keystore(Key1, 1, List, {Key2, a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    lists:keystore(Key1, 1, List, Tuple);
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    lists:keystore(Key1, 1, List, create_tuple()).
