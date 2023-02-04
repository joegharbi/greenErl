-module(test).

recursive(Key, List) ->
    begin
        Key1 = Key + 1, case maps:take(Key1, List) of
            error -> false;
            {Value1, List1} -> {value, {Key1, Value1}, List1}
        end
    end,
    recursive(Key, List).
