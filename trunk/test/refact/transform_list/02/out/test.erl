-module(test).

recursive(List) ->
    K = key,
    begin
        NewKey = K, #{NewKey:=V} = List
    end,
    recursive(List).  
