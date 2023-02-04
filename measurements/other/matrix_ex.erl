-module(matrix_ex).

-compile(export_all).

-spec mult_seq(R::[[integer()]], C::[[integer()]]) -> [[integer()]].
mult_seq(Rows, Cols) -> 
    lists:map(fun(R) -> 
                      lists:map(fun(C) -> mult_sum(R, C) end, Cols)
              end, Rows).

-spec mult_sum(R::[integer()], C::[integer()]) -> integer().
mult_sum(R, C) -> 
    lists:sum([mul(A,B) || {A,B} <- zip(R,C)]). 

-spec mul(A::integer(), B::integer()) -> integer().
mul(A, B)->
    A*B.

zip([X|R],[Y|C])->
    [{X,Y} | zip(R,C)];
zip([], _) ->
    [];
zip(_, []) ->
    [].
