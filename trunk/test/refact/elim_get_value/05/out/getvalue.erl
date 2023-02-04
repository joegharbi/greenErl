-module(getvalue).

-import(proplists, [get_value/2, get_value/3]).

f() ->
    case lists:keyfind(2, 1, [{1, 2}, {2, 3}]) of
        false -> undefined;
        {_, Var1} -> Var1
    end.
