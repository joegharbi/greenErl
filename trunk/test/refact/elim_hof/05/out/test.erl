-module(test).

f(List) ->
    [case Elem1 of
        ok -> ok;
        _ -> error
    end || Elem1<-List].
