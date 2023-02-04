-module(test).

f(List) ->
    [Elem1 || Elem1<-List, true].
