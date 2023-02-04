-module(test).

recursive(Key, List) ->
    List#{Key=>a},
    recursive(Key, List).
