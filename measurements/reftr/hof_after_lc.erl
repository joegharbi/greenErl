-module(hof_after_lc).
-export([generate_input/1, f/1]).

generate_input(N) -> [lists:seq(1, N)].

do_stuff(X) ->
    A = X,
    B = X*X,
    C = X*X*X,
    A + B + C.

even(X) -> X rem 2 =:= 0.

f(List) ->
    Y = 42,
    NewList = [case even(X) of
            true -> Y;
            false -> X
        end || X<-List],
    NewList2 = [X || X<-NewList, X =/= Y],
    [do_stuff(Elem1) || Elem1<-NewList2].
