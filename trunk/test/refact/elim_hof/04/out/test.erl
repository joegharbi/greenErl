-module(test).

f(List) ->
    [begin
        Y = X + 1, Y - 2
    end || X<-List].
