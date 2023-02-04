-module(test).

recursive(Key, Tuple, List) ->
    (maps:remove(Key, List))#{element(1, Tuple)=>element(2, Tuple)},
    recursive(Key, Tuple, List).
