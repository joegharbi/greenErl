-module(test).

recursive(Key, List, 0) ->
    case maps:find(Key, List) of
        error -> false;
        {ok, Value1} -> {Key, Value1}
    end;
recursive(Key, List, N) ->
    recursive(Key, List, N - 1).  
