%%% -*- coding: latin-1 -*-

%%% This file is part of RefactorErl.
%%%
%%% RefactorErl is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% RefactorErl is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc

%%% @author Drienyovszky Dániel <monogram@inf.elte.hu>
%%% @author Dániel Horpácsi <daniel-h@elte.hu>

-module(refqc_generators).
-vsn("$Rev:  $ ").

-export([gen_args/1, gen_args/2,
         generate_varnames/1 %% var_anal_test
        ]).

-include_lib("../include/prop_based_testing.hrl").
-include_lib("referl_lib/include/lib_export.hrl").
-include_lib("referl_core/include/core_export.hrl"). %% ?Syn

-define(Underscore, [$_]).

%% -----------------------------------------------------------------------------

gen_args(X) -> gen_args(X, {files, ?Query:exec([file])}).

gen_args(X, {files, Files}) ->
    ?LET(File, oneof(Files), gen_args(X, File));
gen_args(X, {modules, Mods}) ->
    Files = [F || F <- lists:flatten(?Query:exec(Mods, ?Mod:file()))],
    ?LET(File, oneof(Files), gen_args(X, File));
gen_args(X, {filename, Filename}) ->
    [File] = ?Query:exec(?File:find(Filename)),
    gen_args(X, File);

%% -- rename function ----------------------------------------------------------

gen_args(renfun, {'$gn',file,_} = File) ->
    Mod = ?Query:exec1(File, ?File:module(), file_mod),
    ?LET({NName, Fun}, {gen_funname(), gen_local_fun(Mod)},
         begin
             MName  = ?Mod:name(Mod),
             FName  = ?Fun:name(Fun),
             FArity = ?Fun:arity(Fun),
             Trans  = fun(M, _F, As) -> apply(M, list_to_atom(NName), As)
                      end,
             Args   = [{module, MName}, {function, FName}, {arity, FArity},
                       {name, NName}, {ask_missing, false}],
             
             io:format("Args generated to rename fun: ~p:~p/~p to ~s~n",
                       [MName, FName, FArity, NName]),
             
             {FName, FArity, Trans, Args}
         end);

%% -- eliminate variable -------------------------------------------------------

gen_args(elimvar, File) ->
    Mod = ?Query:exec1(File, ?File:module(), no_module_node),
    Filename = ?File:path(File),
    ?LET(Fun, gen_local_fun(Mod),
         ?LET({Tok, {B, _E}}, gen_var(File, Fun),
              begin
                  io:format("Args generated to elim var: ~s@~p (~s)~n",
                            [Filename, B, ?Token:get_value(Tok)]),

                  {?Fun:name(Fun), ?Fun:arity(Fun),
                   [{file, Filename}, {position, B}]}
              end));

%% -- extract function ---------------------------------------------------------

gen_args(extfun, File) ->
    Mod = ?Query:exec1(File, ?File:module(), no_module_node),
    Filename = ?File:path(File),
    ?LET({NName, Fun}, {gen_funname(), gen_local_fun(Mod)},
         ?LET({{Expr1, Range1}, {_Expr2, Range2}},
              {gen_expr(Fun), gen_expr(Fun)},
              ?LET(Range, frequency([{3, Range1}, {1,crt_region(Range1, Range2)}]),
                   begin
                       FName  = ?Fun:name(Fun),
                       FArity = ?Fun:arity(Fun),
                       Args   = [{file, Filename},
                                 {posrange, Range},
                                 {name, NName}],

                       io:format("Args generated to extract fun: ~s@~p into ~s~n",
                                 [Filename, Range, NName]),

                       {FName, FArity, [Expr1|Args]}
                   end)));

%% -- rename variable ----------------------------------------------------------

gen_args(renvar, File) ->
    Mod = ?Query:exec1(File, ?File:module(), no_module_node),
    Filename = ?File:path(File),
    ?LET({NName, Fun}, {gen_varname(), gen_local_fun(Mod)},
         ?LET({Tok, {B, _E}}, gen_var(File, Fun),
              begin
                  Args = [{file, Filename},
                          {position, B}, % ?LAZY(oneof(get_var_positions(File)))
                          {varname, NName}],
                  
                  io:format("Args generated to rename var: ~s@~p (~s) to ~s~n",
                            [Filename, B, ?Token:get_value(Tok), NName]),
                  
                  {?Fun:name(Fun), ?Fun:arity(Fun), Args}
              end));

%% -- rename module ------------------------------------------------------------

gen_args(renmod, File) ->
    ModNames = optional_modnames() ++
        [atom_to_list(?Mod:name(Mod)) || Mod <- lists:usort(?Query:exec([module]))],
    [{file, File}, {name, oneof(ModNames)}];

%% -- tuple function args ------------------------------------------------------

gen_args(tuple, File) ->
    Mod = ?Query:exec1(File, ?File:module(), no_module_node),
    Filename = ?File:path(File),
    ?LET(Fun, gen_local_fun(Mod),
         ?LET({{Is, S}, Es}, oneof(param_data(Fun)),
              ?LET({Ie, E}, oneof(Es),
                   begin
                       FName  = ?Fun:name(Fun),
                       FArity = ?Fun:arity(Fun),
                       Trans  = fun(M, F, As) ->
                                        apply(M, F, tuple_from_to(As, Is, Ie))
                                end,
                       Args = [{file, Filename},
                               {posrange, {S, E}}],

                       io:format("Args generated to tuple funpar: params ~p-~p of ~s:~p/~p~n",
                                 [Is,Ie, filename:basename(Filename, ".erl"), FName, FArity]),

                       {FName, FArity, Trans, Args}
                   end))).

param_data(Fun) ->
    lists:append(
      [begin
           Ps = ?Query:exec(C, ?Clause:patterns()),
           {R, _} = lists:mapfoldr(fun helper/2, {length(Ps), []}, Ps),
           R
       end || C <- ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                ?Form:clauses()]))]).

helper(P, {I, Es}) ->
    {Start, End} = range(P),
    Es1 = [{I, End} | Es],
    {{{I, Start}, Es1}, {I-1, Es1}}.

tuple_from_to(L, S, E)
  when is_list(L), is_integer(S), is_integer(E), S =< E ->
    {Pre, Rest} = lists:split(S - 1, L),
    {Tuple, Post} = lists:split(E - S + 1, Rest),
    Pre ++ [list_to_tuple(Tuple)] ++ Post.

%% -----------------------------------------------------------------------------

%% optional names
optional_modnames() ->
    Abc = lists:seq($a,$z),
    N = random:uniform(100),
    [refqc_common:get_n(random:uniform(10), Abc) || _ <- lists:seq(0,N)].

%% merge two regions
crt_region({St1, End1}, {St2, End2}) when St1 < St2 andalso End2 < End1 ->
    {St1,End1};
crt_region({St1, End1}, {St2, End2}) when End1 < St2 ->
    {St1, End2};
crt_region({St1, End1}, {St2, End2}) when End2 < St1 ->
    {St2, End1};
crt_region(Rg,_) ->
    Rg.

%% -----------------------------------------------------------------------------

%% collects recursively all the variables (also from the inner scopes) in the
%% given clauses
get_vars([])-> [];
get_vars(Clauses) ->
    Variables = [?Query:exec(Clause, ?Clause:variables()) || Clause <- Clauses],
    InnerScopes =
        lists:flatten([?Query:exec(Clause, [{functx, back}])
                        || Clause <- Clauses]),
    Variables ++ get_vars(InnerScopes -- Clauses).

%% collects all the variables used in the given file (File = node())
get_vars_from_file(File) ->
    TopClauses = ?Query:exec(File, ?Query:seq([?File:forms(), ?Form:clauses()])),
    lists:usort(lists:flatten(get_vars(TopClauses))).

%% get_var_positions(Path) ->
%%     [File] = ?Query:exec(?File:find(Path)),
%%     Vars = get_vars_from_file(File),
%%     VarOccurrences = ?Query:exec(Vars, ?Var:occurrences()),
%%     Fun =
%%         fun(Var) ->
%%                 FirstToken = hd(?Query:exec(Var, [elex])),
%%                 refqc_common:get_token_pos(File, FirstToken)
%%         end,
%%     case VarOccurrences of
%%         [] ->
%%             [-1];
%%         _ ->
%%             [Fun(Var) || Var <- VarOccurrences]
%%     end.

%% -----------------------------------------------------------------------------

%% @doc Generates `NumVars' number of random variable names (correct
%% variable names)
%% @spec generate_varnames(NumNames :: integer()) -> [string()]
generate_varnames(NumVars) ->
    Abc = lists:seq($a,$z),
    ABC = lists:seq($A,$Z) ++ ?Underscore,
    [[lists:nth(random:uniform(length(ABC)), ABC)] ++
     refqc_common:get_n(random:uniform(2), Abc)
     || _ <- lists:seq(0,NumVars)] ++ [?Underscore].

gen_funname() ->
    ExistingNames =
        [?Fun:name(F) || F <- ?Query:exec([module,func]),
                         not erl_internal:bif(?Fun:name(F), ?Fun:arity(F))],
    frequency([{1, oneof(ExistingNames)}, {20, gen_atom()}]).

%% @doc Quickcheck generator for erlang variable names as strings
gen_varname() ->
    Variables = lists:flatten([get_vars_from_file(File) ||
                                  File <- ?Query:exec([file])]),
    ExistingNames = [?Var:name(Var) || Var <- Variables],
    frequency([{1, oneof(ExistingNames)},
               {2, [choose($A, $Z) | list(choose($a, $z))]}]).

%% @doc Quickcheck generator for erlang atoms as strings
gen_atom() ->
    [choose($a, $z),choose($a, $z),choose($a, $z) | list(choose($a, $z))].

%% @doc Quickcheck generator for functions.
%% @spec gen_local_fun(node()) -> node()
gen_local_fun(Mod) ->
    L = ?Query:exec(Mod, ?Mod:locals()),
    oneof(L).

%% @doc Quickcheck generator for variables. Instead of a node, it
%% returns information about the variable.
gen_var(File, Fun) ->
    L = ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                     ?Form:clauses(),
                                     ?Clause:body(),
                                     ?Expr:variables(),
                                     ?Var:occurrences(),
                                     [elex]])),
    [Mod] = ?Query:exec(File, ?File:module()),
    %% Makes the test fail with a "?SUCHTHAT failed to find a value
    %% within 100 attempts." message rather than with a badmatch
    %% error...
    ?SUCHTHAT(X,
              case ?Token:map_pos(File, L) of
                  [] ->
                      io:format("Should generate a random variable of ~p:~p/~p, but it hasn't any one...~n",
                                [?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]),
                      none;
                  R  -> oneof(R)
              end,
              X /= none).

%% @doc Quickcheck generator for expressions.
gen_expr(Fun) ->
    Es = ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                      ?Form:clauses(),
                                      ?Clause:body(),
                                      ?Expr:deep_sub()])),
    oneof([{E, range(E)} || E <- Es]).

%% @doc Given an expression node, returns the beginning and ending
%% position.
%% @spec range(node()) -> {integer(), integer()}
range(Expr) ->
    FL = ?Syn:first_leaf(),
    LL = ?Syn:last_leaf(),
    [FT] = FL(Expr),
    [LT] = LL(Expr),
    {B, _E} = ?Token:pos(FT),
    {_B, E} = ?Token:pos(LT),
    {B, E}.
