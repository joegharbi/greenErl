-module(test).

recursive(Key, List) ->
    {value, _, NewList} = case maps:take(Key, List) of
            error -> false;
            {Value1, List1} -> {value, {Key, Value1}, List1}
        end,
    begin
        K = Key, #{K:=V} = NewList
    end,
    recursive(Key, NewList).
