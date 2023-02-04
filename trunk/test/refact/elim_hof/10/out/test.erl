-module(test).

f(List) ->
    [X || X<-List, X rem 2 =:= 0].
