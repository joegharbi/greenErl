-module(test).

recursive(Key, List) ->
    maps:is_key(Key, List),
    recursive(Key, List).
