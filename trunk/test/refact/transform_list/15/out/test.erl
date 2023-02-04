-module(test).

caller() ->
    recursive(key, maps:from_list([{key, 1}, {otherkey, 2}])).

recursive(Key, List) ->
    case maps:find(Key, List) of
        error -> false;
        {ok, Value1} -> {Key, Value1}
    end,
    recursive(Key, List).  

othercaller(List) ->
    recursive(true, maps:from_list(List)).
