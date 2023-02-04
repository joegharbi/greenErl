-module(test).

increase(X) -> X + 1.

f(List) ->
    [increase(Elem1) || Elem1<-List].
