-module(test).

f(List) ->
    [Elem1 || Elem1<-List, case Elem1 of
        ok -> true;
        _ -> false
    end].
