-module(test).

even(X) -> X rem 2 =:= 0.

f(List) ->
    [Elem1 || Elem1<-List, even(Elem1)].
