-module(test).

create_tuple() -> {1,2}.

recursive(Key, List) ->
    lists:keystore(Key, 1, List, create_tuple()),
    recursive(Key, List).
