-module(test).

caller() -> 
    recursive(key, [{key, 1}, {otherkey, 2}]).

recursive(Key, List) ->
    lists:keyfind(Key, 1, List),
    recursive(Key, List).  

othercaller(List) ->
    recursive(true, List).
