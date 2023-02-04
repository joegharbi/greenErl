-module(test).

f(List) ->
    [case Elem1 of
        X when X =:= ok -> ok;
        Y -> Y
    end || Elem1<-List].
