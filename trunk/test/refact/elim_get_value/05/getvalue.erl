-module(getvalue).

-import(proplists, [get_value/2, get_value/3]).

f() ->
    get_value(2, [{1,2},{2,3}]).
