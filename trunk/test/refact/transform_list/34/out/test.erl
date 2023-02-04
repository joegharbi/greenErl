-module(test).

recursive(Key, List) ->
    maps:remove(Key, List),
    recursive(Key, List).
