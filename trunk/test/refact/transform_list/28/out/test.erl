-module(test).

recursive(Key, List) ->
    (maps:remove(Key + 1, List))#{Key + 1=>a},
    recursive(Key, List).
