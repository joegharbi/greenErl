-module(getvalue).

f() ->
    proplists:get_value(2, [{1,2},{2,3}], 42).
