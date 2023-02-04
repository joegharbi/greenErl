-module(test).

recursive(Key, List) ->
    case maps:take(Key, List) of
        error -> false;
        {Value1, List1} -> {value, {Key, Value1}, List1}
    end,
    recursive(Key, List).
