-module(test).

f(List) ->
    [Elem1 || Elem1<-List, begin
        Y = 41 + 1, Y =:= 42
    end].
