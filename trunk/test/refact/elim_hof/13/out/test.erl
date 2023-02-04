-module(test).

f(List) ->
    [Elem1 || Elem1<-List, case Elem1 of
        X when X =:= ok -> true;
        Y -> Y =:= 2
    end].
