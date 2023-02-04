-module(test).

recursive(Key, List, 0) ->
    begin
        Key1 = Key + 1, case maps:find(Key1, List) of
            error -> false;
            {ok, Value1} -> {Key1, Value1}
        end
    end;
recursive(Key, List, N) ->
    recursive(Key, List, N - 1).  
