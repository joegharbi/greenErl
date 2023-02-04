-module(lc).

even(X) -> X rem 2 =:= 0.

main(List) ->
    [Elem || Elem <- List, even(Elem)].
