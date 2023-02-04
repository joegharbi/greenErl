-module(test).

recursive(_Key, Map1) when map_size(Map1) =:= 0 -> ok;
recursive(Key, List) ->
    maps:remove(Key, List),
    recursive(Key + 1, List).
