-module(test).

recursive(List) ->
    case maps:find(42, List) of
        error -> false;
        {ok, Value1} -> {42, Value1}
    end,
    recursive(List).  
