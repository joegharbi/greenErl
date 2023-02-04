-module(test).

f(List) ->
    [X + 1 || X<-List].
