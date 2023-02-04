-module(test).

recursive(Key, List, 0) ->
    ok;
recursive(Key, List, N) -> 
    List,
    recursive(Key, List, N - 1).