-module(test).

recursive(Key, List, 0) ->
    maps:to_list(List);
recursive(Key, List, N) ->
    case maps:find(Key, List) of
        error -> false;
        {ok, Value1} -> {Key, Value1}
    end,
    recursive(Key, List, N - 1).  
