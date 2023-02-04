-module(test).

recursive(Key, List) ->
    recursive(Key, List),
    case maps:take(Key, List) of
        error -> false;
        {Value1, List1} -> {value, {Key, Value1}, maps:to_list(List1)}
    end.
