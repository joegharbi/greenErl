-module(getvalue).

f() ->
    case lists:keyfind(2, 1, [{1, 2}, {2, 3}]) of
        false -> 42;
        {_, Var1} -> Var1
    end.
