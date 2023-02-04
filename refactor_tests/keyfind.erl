-module(keyfind).
-import(proplists, [get_value/2, get_value/3]).

alma(Var) -> 
    Var1 = Var,
    Var2 = Var1,
    Var3 = alma,
    true andalso get_value(Var3, Var2),
    proplists:get_value(korte, Var),
    proplists:get_value(kulcs, Var, false),
    proplists:get_value(kaposzta, Var2, lists:map(fun(X) -> X + 1 end, [1,2,3])),
    get_value(alma, Var, ok).
