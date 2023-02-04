-module(test).

recursive(List) ->
    K = 1,
    begin
        Key1 = K + 1, case maps:find(Key1, List) of
            error -> false;
            {ok, Value1} -> {Key1, Value1}
        end
    end,
    recursive(List).  
