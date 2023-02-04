-module(test).

recursive(List) ->
    K = 1,
    begin
        NewKey = K + 1, #{NewKey:=V} = List
    end,
    recursive(List).  
