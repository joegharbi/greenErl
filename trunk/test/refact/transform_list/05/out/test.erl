-module(test).

recursive(List) ->
    K = key,
    Result = case maps:find(K, List) of
            error -> false;
            {ok, Value1} -> {K, Value1}
        end,
    recursive(List).  
