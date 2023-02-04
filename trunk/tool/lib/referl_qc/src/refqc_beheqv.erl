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

%%% @doc Main module implementing beh. eqv. testing.
%%% Currently may only be used by random module testing.

%%% @author Drienyovszky Dániel <monogram@inf.elte.hu>
%%% @author Dániel Horpácsi <daniel_h@inf.elte.hu>

-module(refqc_beheqv).
-vsn("$Rev$ ").

-export([check_simple/2, check_simple/4, check_simple/5]).
-export([prop_beh_eqv/2, prop_beh_eqv/3]).

%% Randmod callbacks
-export([prepare/1, perform_and_check/3]).

-include_lib("../include/prop_based_testing.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-define(MYTIMEOUT, 250). %% milliseconds
-define(NUMTESTS, 10).

-define(Types, refqc_dd_types).
-define(Iodev, refqc_dd_iodev).

-define(RandMod, refqc_randmod).

-define(L, lists).

-define(TestUtils, reftest_utils).

%%-----------------------------------------------------------------------------

%% @doc Equivalent to `prop_beh_eqv(1, Ref, Gen)'
prop_beh_eqv(Ref, Gen) -> prop_beh_eqv(1, Ref, Gen).

%% @spec prop_beh_eqv(N::integer(), Ref::atom(), Gen::callback()) -> property()
%%           callback() = (string()) -> ret_type()
%%           ret_type() = proplist() | {atom(), integer(), proplist()}
%%
%% @doc Property defining behavioural equivalency after a refactoring.
%% `N' is the number of interdependent modules to generate. `Ref' is
%% an atom specifying the callback module that implements the
%% refactoring. `Gen' is a function that applied to a filepath returns
%% a proplist of arguments for the refactoring, optionally with a
%% function name and arity.
prop_beh_eqv(N, Ref, Gen)  -> ?RandMod:prop({dd, Ref, Gen, N}).

%% -----------------------------------------------------------------------------
%% Random module based testing callbacks

prepare(Gen) -> noshrink(Gen({filename, filename:join(?TestDir, "after1.erl")})).

perform_and_check(N, Ref, X) ->
    Mods = [list_to_atom(M ++ integer_to_list(I)) ||
               I <- ?L:seq(1, N), M <- ["before", "after"]],
    ?IMPLIES(X =/= none,
             case X of
                 {F, A, Args} ->
                     %%?Transform:do(Ref, Args),
                     ?TestUtils:exec_transform(Ref, Args),
                     ?L:all(fun compile/1, Mods) andalso
                         check_simple(before1, after1, F, A);
                 {F, A, T, Args} ->
                     %%?Transform:do(Ref, Args),
                     ?TestUtils:exec_transform(Ref, Args),
                     ?L:all(fun compile/1, Mods) andalso
                         check_simple(before1, after1, T, F, A);
                 %% TODO testing renfun should check not only the renamed fun
                 Args ->
                     %%?Transform:do(Ref, Args),
                     ?TestUtils:exec_transform(Ref, Args),
                     ?L:all(fun compile/1, Mods) andalso
                         check_simple(before1, after1)
             end).

%%-----------------------------------------------------------------------------

%-ifndef(ALWAYS).
-undef(ALWAYS).
-define(ALWAYS(N,P),P).
%-endif.

%% @doc Property testing the beh. eqv. of the two modules
check_simple(Mod1, Mod2) when is_atom(Mod1), is_atom(Mod2) ->
    Init = compile(Mod1) andalso compile(Mod2),
    %% ?L:foldl(fun({F, A}, B) -> B andalso
    %%                  ?QuickCheck:quickcheck(check_simple1(Mod1, Mod2, F, A)) end,
    %%         Init, Mod1:module_info(exports)).

    %% there doesn't seem to be an easy way to exhaustively test all
    %% functions so we try to test 5 functions choosen randomly
    
    ?ALWAYS(5,
            ?FORALL({F, A}, oneof(Mod1:module_info(exports)
                                  -- [{module_info, 0}, {module_info, 1}]),
                    Init andalso check_simple(Mod1, Mod2, F, A)
                   )).


%% @doc Property testing beh. eqv. of a given funtion in two separate modules
check_simple(Mod1, Mod2, Fun, Arity)
  when is_atom(Mod1), is_atom(Mod2), is_atom(Fun), is_integer(Arity) ->

    io:format("Checking ~p:~p/~p against ~p:~p/~p by behavioural equivalency~n",
              [Mod1, Fun, Arity, Mod2, Fun, Arity]),
    
    check(?Types:infer(Mod1, Fun, Arity),
          fun(As) -> apply(Mod1, Fun, As) end,
          fun(As) -> apply(Mod2, Fun, As) end).


%% @doc Property testing beh. eqv. of given function when its
%% interface changed, `Trans' is a function which translates from the
%% old interface to the new one.
check_simple(Mod1, Mod2, Trans, Fun, Arity)
  when is_atom(Mod1), is_atom(Mod2), is_function(Trans),
       is_atom(Fun), is_integer(Arity) ->

    io:format("Checking ~p:~p/~p against ~p:~p/~p by behavioural equivalency",
              [Mod1, Fun, Arity, Mod2, Fun, Arity]),

    check(?Types:infer(Mod1, Fun, Arity),
          fun(As) -> apply(Mod1, Fun, As) end,
          fun(As) -> Trans(Mod2, Fun, As) end).


%% @private
%% @doc Does the actual testing N times, using `ArgType' for
%% generating arguments
check(ArgType, Fun1, Fun2)
  when is_function(Fun1), is_function(Fun2) ->
    ?ALWAYS(?NUMTESTS,
            ?FORALL(Args, ?Types:t_to_gen(ArgType),
                    begin
                        spawn(runP(self(), Fun1, Args)),
                        spawn(runP(self(), Fun2, Args)),
                        receive {Pid1, R1} -> ok end,
                        receive {Pid2, R2} -> ok end,
                        
                        case R1 of
                            {error, timeout} ->
                                B = ?Iodev:compare_prefix(Pid1, Pid2);
                            _ ->
                                B = ?Iodev:compare(Pid1, Pid2)
                        end,
                        ?WHENFAIL(io:format("R1: ~p~nR2: ~p~nB: ~p~n",
                                            [R1, R2, B]),
                                  collect(stat(R1), R1 =:= R2 andalso B))
                    end)).


%% @private
%% @doc Compiles and loads a module
compile(Module) when is_atom(Module) ->
    case compile:file(filename:join(?TestDir, atom_to_list(Module) ++ ".erl"), [{outdir, ?TestDir}]) of
        {ok, _} ->
	    code:add_path(?TestDir),
            code:purge(Module),
            code:delete(Module),
            code:load_file(Module),
            true;
        error ->
            false
    end.

%%-----------------------------------------------------------------------------

%% @private
try_apply(Fun, Args)->
    try Fun(Args) of
        Result -> {ok, Result}
    catch
        C:E    ->
%%             io:format("Cought exception: ~p:~p ~p~n",
%%                       [C, E, erlang:get_stacktrace()]),
            {error, C, E}
    end.

runP(P, F, As) ->
    fun() ->
            Self = self(),
            Pid = spawn(fun() -> Self ! {self(), try_apply(F, As)} end),
            receive
                {Pid, R} -> P ! {Pid, R}
            after
                ?MYTIMEOUT ->
                    exit(Pid, kill),
                    P ! {Pid, {error, timeout}}
            end
    end.

%% @private
stat({ok, _})          -> ok;
stat({error, timeout}) -> timeout;
stat({error, C, E})    -> {C, E}.
