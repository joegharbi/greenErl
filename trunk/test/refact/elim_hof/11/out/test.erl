-module(test).

f(List) ->
    [X || X<-List, begin
        Y = X + 1, Y =:= 42
    end].
