-module(matrix_ex_paper).

-compile(export_all).

%% -spec mult_matrix([[integer()]], [[integer()]]) -> [[integer()]].
%% mult_matrix(Rows, Cols) ->
%%   [ scalar_product(R,C) || 
%%     R <- Rows, C <- Cols ].

-spec mult_matrix2([[integer()]], [[integer()]]) -> [[integer()]].
mult_matrix2([], _) ->
  [];
mult_matrix2([R | Rows], Cols) ->
  [ lists:map (
          fun(C) -> scalar_product(R,C) end,
          Cols )
  | mult_matrix2(Rows,Cols) ].

-spec scalar_product([integer()], [integer()]) -> integer().
scalar_product(R, C) when is_list(R),is_list(C)-> 
  lists:sum([ mult_scalar(A,B) || 
              {A,B} <- zip(R,C) ]);

scalar_product(_,_) ->
    scalar_product(lists:seq(1,700),lists:seq(1,700)).

% scalar_product(R, C) when is_list(R),is_list(C)-> 
%   lists:sum([ mult_scalar(A,B) || 
%               {A,B} <- lists:zip(R,C) ]). 

% scalar_product(_,_) ->
%     scalar_product(lists:seq(1,100),lists:seq(1,100)).
    
-spec mult_scalar(integer(), integer()) -> integer().
mult_scalar(A, B) ->
  A*B.

zip([X|R],[Y|C])->
    [{X,Y} | zip(R,C)];
zip([], _) ->
    [];
zip(_, []) ->
    [].


%% mult_shaped(Rows, Cols)->
%%     NV1 = [{R,C}  || R <- Rows, C <- Cols], 
%%     NV2 = fun({R,C})-> mult_sum(R,C) end,
%%     [NV2(NV3) || NV3 <- NV1].


%mult_new(Rows, Cols)->
%    lists:append([[mult_sum(R,C) || C <- Cols] || R <- Rows]).



%% mult(Rows, Cols)->
%%     [mult_sum({R,C}) || R <- Rows, C <- Cols].

%% mult_seq(Rows, Cols) -> 
%%     lists:map(fun(R) -> 
%%                       lists:map(fun(C) -> mult_sum(R, C) end, Cols)
%%               end, Rows).

%% mult_sum({R, C}) -> 
%%     lists:sum([mul(A,B) || {A,B} <- lists:zip(R,C)]). 


%% mult([], _)->
%%     [];
%% mult([R | Rows], Cols) ->
%%     [mult_r(R, Cols) | mult(Rows, Cols)].

%% mult_r(_, [])->
%%     [];
%% mult_r(R, [C | Cols]) ->
%%     [mult_sum(R, C) | mult_r(R, Cols)].

