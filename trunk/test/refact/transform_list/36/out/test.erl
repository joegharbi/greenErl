-module(test).

recursive(Key, Map1) when map_size(Map1) =:= 0, Key < 10 -> ok;
recursive(Key, List) ->
    maps:remove(Key, List),
    recursive(Key + 1, List).
