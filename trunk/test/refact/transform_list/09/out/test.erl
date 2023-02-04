-module(test).

recursive(List) ->
    case maps:find(key, List) of
        error -> false;
        {ok, Value1} -> {key, Value1}
    end,
    recursive(List).  
