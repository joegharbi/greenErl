-module(test).

recursive(List) ->
    K = key,
    #{K:=V} = List,
    recursive(List).  
