-module(test).

f(List) ->
    [lists:last(Elem1) || Elem1<-List].
