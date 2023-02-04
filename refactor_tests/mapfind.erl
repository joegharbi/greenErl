-module(mapfind).

find(Map, Key) ->
    #{Key := Value} = Map.
