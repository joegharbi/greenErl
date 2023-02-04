-module(test).

f(List) ->
    [Elem1 || Elem1<-List, lists:last(Elem1)].
