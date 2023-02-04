-module(test).

recursive(Key1, Key2, List) ->
    (maps:remove(Key1, List))#{Key2=>a},
    recursive(Key1, Key2, List).
