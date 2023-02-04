-module(test).

recursive(Key, List) ->
    recursive(Key, List),
    maps:to_list(maps:remove(Key, List)).
