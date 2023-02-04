%% -*- coding: latin-1 -*-

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

%%% @doc The module implements process analysis that extends the SPG
%%% with process semantic information. It creates new process nodes
%%% and communication links between them. Tha analysis also invokes
%%% `ets' analysis that adds ets manipulation information to the
%%% graphs.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(refanal_proc).
-vsn("$Rev$ ").
-behaviour(refcore_anal).

%% callback functions
-export([schema/0, externs/1, insert/4, remove/4, update/2]).

%% interface functions
-export([anal_proc/0, clean/0, create_dot/0]).

%% visualising functions
-export([create_gen_server_view/0, create_processes_view/0,
         create_full_view/0]).

-include("core.hrl").

-define(CallAnal, refcore_callanal).
-define(ProcLib, refanal_proc_lib).
-define(PT, processes).

%%% @private
schema() ->
    [{pid, record_info(fields, pid), [{spawn, pid}, {spawn_link, pid}, 
                                      {link, pid}, {register, pid}, 
                                      {send, pid}, {create, ets_tab}, 
                                      {read, ets_tab}, {write, ets_tab},
                                      {start, pid}, {start_link, pid},
                                      {sync_call, pid}, {async_call, pid}]},
     {expr, [{spawn_def, pid}, {reg_def, pid}, {eval_in, pid},
             {start_def, pid}]},
     {root, [{pid, pid}]}
    ].

%%% @private
externs(_) -> [].

%%% @private
insert(_Parent, _Pre, {_Tag, _Child}, _Post) -> ok.

%%% @private
remove(_Parent, _Pre, {_Tag, _Child}, _Post) -> ok.

%%% @private
update(_Node, _Data) -> ok.

-spec anal_proc() -> ok.
%% @doc The function analyses the process creation, communication
%% between them and creates a process communication graph from this
%% information. It creates an ets table `processes' where the process
%% nodes (executed function in the process) and the connections are
%% stored. The hidden communication through `ets' tables are also
%% considered, and included in the final graph.
anal_proc()->
    analyse(),
    create_additional_pids(),
    analyse_gen_servers(),
    create_graph(),
    refanal_ets:add_ets_read_write().

-spec clean() -> ok.
%% @doc The function removes semantic information related to the
%% process analysis. Removes `pid' and `ets_tab' semantic nodes and
%% links from the SPG and deletes the `processes' table.
clean() ->
    %% cleaning up process information from the SPG
    refanal_ets:clean(),
    Pids = ?Graph:path(?Graph:root(), [pid]),
    [?Graph:rmlink(Pid, Link, To) || Pid <- Pids, 
                                     {Link, To} <- ?Graph:links(Pid)],
    [?Graph:rmlink(From, Link, Pid) || Pid <- Pids, 
                                       {Link, From} <- ?Graph:links(Pid)],
    lists:foreach(fun ?Graph:delete/1, Pids),

    %% cleaning up the communication information, removing the
    %% processes `ets' table
    case ets:info(?PT) of
        undefined -> ok;
        _         -> ets:delete(?PT)
    end,
    ok.

create_gen_server_view() ->
    clean(),
    analyse_gen_servers(),
    create_graph(),
    create_dot(),
    ok.
create_processes_view() ->
    clean(),
    analyse(),
    create_additional_pids(),
    create_graph(),
    refanal_ets:add_ets_read_write(),
    create_dot(),
    ok.
create_full_view() ->
    clean(),
    analyse(),
    create_additional_pids(),
    analyse_gen_servers(),
    create_graph(),
    refanal_ets:add_ets_read_write(),
    create_dot(),
    ok.

-spec create_graph() -> ok.
%% @doc Extracts the pid information from the SPG and creates the
%% process model in a named `ets' table (`processes') (it also
%% calculates the sent messages).
create_graph()->
    case ets:info(?PT) of
        undefined -> ets:new(?PT, [named_table,bag]);
        _         ->
            ets:delete(?PT),
            ets:new(?PT, [named_table,bag])
    end,
%    DG = digraph:new(),
    Spawns = find_spawns(),
    SLinks = [{?ProcLib:get_proc(E), spawn_link, ?Graph:path(E, [spawn_def])} 
              || E <- Spawns],
    Regs   = find_registers(),
    RLinks = [{?ProcLib:get_proc(E), register, ?Graph:path(E, [reg_def])} 
              || E <- Regs],
%% Spawned/registered processes are linked to the spawning/registering process
    [[ets:insert(?PT, {{A, B},
                       ?ProcLib:label(?Graph:data(A)),
                       LinkType, 
                       ?ProcLib:label(?Graph:data(B))}) 
      || A <- List1,B <- List2] || {List1,LinkType,List2} <- SLinks ++ RLinks],
    %?d(SLinks++RLinks),
    AllPids = ?Graph:path(?Graph:root(), [{pid, {reg_names, '/=', [sp]}}]),
    SpawnedPids = lists:append(
                    ets:match(?PT, {{'_','$1'}, '_', spawn_link, '_'})),
%% Free pids are linked to SP
    [ets:insert(?PT, {{?ProcLib:ensure_sp(), P},
                      ?ProcLib:label(?Graph:data(?ProcLib:ensure_sp())),
                      spawn_sp, 
                      ?ProcLib:label(?Graph:data(P))})
     || P <- (AllPids -- SpawnedPids)],
    Sends = ?ProcLib:find_nodes_by_type(send_expr),
    [begin
         Message = ?Query:exec1(E, ?Expr:child(2), send),
         Flows   = ?ProcLib:run(fun()-> ?Dataflow:reach([Message],
                                                        [{back, false}]) end),
         Recs    = [ R || E1<-Flows, R <- ?Query:exec(E1, [top, 
                                                           {pattern, back}, 
                                                           {exprcl, back}]), 
                          (?Graph:data(R))#expr.type == receive_expr],
         [ets:insert(?PT, {{S, R}, ?ProcLib:label(?Graph:data(S)),
                           {send, ?Syn:flat_text(Message)}, 
                           ?ProcLib:label(?Graph:data(R))}) || 
             S <- ?ProcLib:get_proc(E),
             R <- lists:flatmap(fun ?ProcLib:get_proc/1, Recs)]      
     end || E <- Sends],
    %% gen_server information
    GenSrvPids = ?Graph:path(?Graph:root(), [{pid, {type, '==', gen_server}}]),
    Info =
        [{GP,
          ?Graph:path(GP, [{sync_call, back}]),
          ?Graph:path(GP, [{async_call, back}])} || GP <- GenSrvPids],
    lists:map(fun gen_srv_call_info/1, Info),
    ok.

-spec analyse() -> ok.
%% @doc The function performs process analysis with heuristic
%% strategy.
analyse() -> analyse([{strict, false}]).

-spec analyse([proplists:property()]) -> ok.
%% @doc Possible options: `strict' (true by default).
analyse(Options) -> %% {Mod, Name, Arity}
    Mode =
        case proplists:get_value(strict, Options, true) of
            true  -> strict;
            false -> heuristic
        end,
    AllFun = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals()])),
    SFun =  [F || F <- AllFun,  
                  ?Query:exec(F, [{funcall, {{name, '==', spawn_link}, 'and', 
                                             {arity, '==', 3}}}]) =/= []],
%% todo: spawn*
    SpawnL = find_spawn_expr(SFun), % :: [{Fun, ListOfSpawnedExprs}]
    SFuns  = lists:append(find_spawned_funs(SpawnL, Mode)),
    MatchList = match_send_and_rec(SFuns),
    Result = lists:foreach(fun({S,R}) -> 
                    [?ProcLib:ensure_link(From, flow, To) || From <- S, To <- R]
                  end, MatchList),
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.

-spec create_additional_pids() -> [Pid :: refcore_graph:gnode()].
%% @doc The function adds additional pids for receive, send, spawn,
%% register and ets function applications.
create_additional_pids() ->
    ReceiveExprs = ?ProcLib:find_nodes_by_type(receive_expr),
    %% erlang:send/2,3 should also be considered 
    SendExprs    = ?ProcLib:find_nodes_by_type(send_expr),
    SpawnExprs   = find_spawns(),
    RegExprs     = find_registers(),
    EtsApps      = ?Query:exec(?Query:seq([?Mod:find(ets), ?Mod:locals(),
                                           ?Fun:applications()])),
                               
    Exprs        = SendExprs ++ ReceiveExprs ++ SpawnExprs
                               ++ RegExprs ++ EtsApps,
    calculate_and_ensure_evaluating_pids(Exprs).

-spec calculate_and_ensure_evaluating_pids(Exprs :: [refcore_graph:gnode()]) ->
                                                  Pids::[refcore_graph:gnode()].
%% @doc The function gets a list of expressions and a link name. First
%% calculates the evaluating function for every expression. Calculates
%% the evaluating process using the call graph if there is an already
%% defined process in the call chain. Otherwise creates a process
%% identifier for the evaluating process. The later case means that
%% the function is evaluated by the so called super process.
calculate_and_ensure_evaluating_pids(Exprs) ->
    ContFuns     = lists:append([?Graph:path(Expr, [top, {visib, back}, functx,
                                                    {funcl, back}, fundef])
                                 || Expr <- Exprs]),
    ContFunData  = lists:map(fun(F) -> function_data(F) end, ContFuns),

    Pids         = ?Graph:path(?Graph:root(), [pid]),
    FunPidData   = fun(P) -> {P#pid.mod, P#pid.func, P#pid.ary} end,
    PidData      = [ {Pid, FunPidData(?Graph:data(Pid))} || Pid <- Pids],
    PidCands = lists:flatten([find_pid_candidates(F, PidData, _Trace = [])
                              || F <- lists:zip3(Exprs,ContFuns,ContFunData)]),
    [ case PC of
          {newpid, Expr, Fun} ->
              {M, F, A} = function_data(Fun),
              PidMFA    =
                  ?Graph:path(?Graph:root(),
                              [{pid, 
                                {{{mod, '==', M}, 'and', {func, '==', F}},
                                 'and', {ary, '==', A}}}]),
              case PidMFA of
                  [] ->
                      Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
                      ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
                      ?ProcLib:ensure_link(Expr, eval_in, Pid),
                      Pid;
                  [Pid]  ->
                      ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
                      ?ProcLib:ensure_link(Expr, eval_in, Pid),
                      Pid
              end;
          {existingpid, Expr, Pid} ->
              ?ProcLib:ensure_link(Expr, eval_in, Pid),
              Pid
      end || PC <- PidCands].

-spec analyse_gen_servers() -> ok.
%% @doc The function analyses the `gen_server' usage in the source
%% code. It extends only the SPG with pid and corresponding
%% information.
analyse_gen_servers() ->
    AppInfo =
        lists:append([ gen_server_start_apps_info(StartType, Arity)
                       || StartType <- [start, start_link], Arity <- [3,4]]),
    create_gen_server_pids(AppInfo),
    calculate_and_ensure_gen_server_refs().
    %%TODO:non standard communication!! through message passing

-spec find_spawns() -> [refcore_graph:gnode()].
%% @doc Returns the applications of the spawn* functions.
% TODO: other spawns
find_spawns() ->
    ?Query:exec(?Query:seq([?Mod:find(erlang), 
                            ?Fun:find(spawn_link, 3),
                            ?Fun:applications()])).
-spec find_registers() -> [refcore_graph:gnode()].
%% @doc Returns the applications of the register/2 function.
find_registers() ->
    ?Query:exec(?Query:seq([?Mod:find(erlang), 
                            ?Fun:find(register,2),
                            ?Fun:applications()])).

-spec gen_srv_call_info({GSrvPid :: refcore_graph:gnode(),
                         SyncCalls :: [refcore_graph:gnode()],
                         AsyncCalls :: [refcore_graph:gnode()]}) -> ok.
%% @doc The function extends the `processes' ets table with call and
%% cast information. It extracts information from the SPG adds
%% `sync_call' and return messages and `async_call' edges.
gen_srv_call_info({GSrvPid, SyncCalls, AsyncCalls}) ->
    % Adding communication edges to the graph
    Fun =
        fun(LabelTag, Pids) ->
            [begin
                 App = ?Graph:path(C, [{eval_in, back}]),
                 Msg = nth_arg(App, 2),
                 Msgs = ?ProcLib:run(fun()-> ?Dataflow:reach(Msg,
                                                             [{back, false}],
                                                             true) end),
                 [ets:insert(?PT, {{C, GSrvPid}, ?ProcLib:label(?Graph:data(C)),
                                   {LabelTag, ?Syn:flat_text(M)}, 
                                   ?ProcLib:label(?Graph:data(GSrvPid))}) ||
                     M <- Msgs]
             end || C <- Pids]
        end,
    Fun(sync_call, SyncCalls),
    Fun(async_call, AsyncCalls),
    % Linking the start/start_link evaluating process to the gen_server
    Fun2 =
        fun(Link) ->
                StartedBy = ?Graph:path(GSrvPid, [{Link, back}]),
                [ets:insert(?PT, {{S, GSrvPid}, ?ProcLib:label(?Graph:data(S)),
                                  Link, ?ProcLib:label(?Graph:data(GSrvPid))})
                 || S <- StartedBy]
        end,
    Fun2(start),
    Fun2(start_link),
    
    %replies from calls
    call_replies(GSrvPid),
    ok.

-spec analyse_reply_apps() -> ok.
% TODO
analyse_reply_apps() ->
    _ReplyApps =
        ?Query:exec(
           ?Query:seq([?Mod:find(gen_server), ?Fun:find(reply, 2),
                       ?Fun:applications()])).

-spec call_replies(refcore_graph:gnode()) -> ok.
%% @doc The function extends the `processes' ets table with replies
%% from the gen_server. It uses the pid information from the SPG and
%% statically calculates the possible responds to the synchronous
%% calls. It adds reply edges only if the first element of the return
%% tuple is the `reply' atom.
call_replies(GenSrvPid) ->
    CallBackMod = (?Graph:data(GenSrvPid))#pid.mod,
    HandleCall  =
        ?Query:exec(
           ?Query:seq([?Mod:find(CallBackMod), ?Fun:find(handle_call,3)])),
    case HandleCall of
        []   -> ok;
        [HC] ->
            RetExprs  =
                ?Query:exec(HC, ?Fun:return_points(HC)),
            ReturnVals =
                ?ProcLib:run(fun()->
                                  ?Dataflow:reach(RetExprs, [back], true)
                              end),
            Replies = lists:filter(fun is_reply/1, ReturnVals),
            Msgs = lists:flatten(lists:map(fun get_reply_msg/1, Replies)),
            ReplyTo = ?Graph:path(GenSrvPid, [{sync_call, back}]),
            [ets:insert(?PT, {{GenSrvPid, R}, 
                              ?ProcLib:label(?Graph:data(GenSrvPid)),
                              {sync_reply, ?Syn:flat_text(Msg)}, 
                              ?ProcLib:label(?Graph:data(R))})
             || R <- ReplyTo, Msg <- Msgs]
    end,
    ok.
-spec is_reply(refcore_graph:gnode()) -> boolean().
%% @doc The function decides whether the given expression is a three-
%% or four-tuple and the first element of the tuple is the `reply'
%% atom (data-flow is used). When the above conditions hold it returns
%% `true'.
is_reply(Expr) ->
    (?ProcLib:is_tuple_expr(Expr, 3) orelse
     ?ProcLib:is_tuple_expr(Expr, 4)) andalso
        begin
            Vals =
                get_atom_values_with_back_reach(?Graph:path(Expr, [{esub, 1}])),
            lists:any(fun(V) ->
                              V == reply
                      end, Vals)
        end.

-spec get_reply_msg(recore_graph:gnode()) -> [refcore_graph:gnode()].
%% @doc The function gets a tuple expression. First it extracts its
%% second element and uses data-flow to calculate the possible values.
get_reply_msg(Expr) ->
    Msg = ?Graph:path(Expr, [{esub, 2}]),
    ?ProcLib:run(fun()->
                         ?Dataflow:reach(Msg, [{back, false}], true)
                 end).

-spec create_gen_server_pids([{AppNode :: refcore_graph:gnode(),
                               ModName :: atom(),
                               StartType :: start | start_link,
                               SrvNames :: [{local, atom()} |
                                            {global, atom()}]}]) -> ok.
%% @doc The function calculates the gen server starting processes, if
%% necessary it creates semantic pid nodes for the calling functions
%% and link them.
create_gen_server_pids([{AppNode, ModName, StartType, SrvNames} | Tl]) ->
    {RegApps, SepRegNames} = analyse_separate_registering(AppNode),

    Pid = #pid{reg_names = SrvNames ++ SepRegNames,
               mod = ModName, type = gen_server},
    PidNode = ?ProcLib:ensure_node(Pid),
    ?ProcLib:ensure_link(?Graph:root(), pid, PidNode),
    [ExecInPid] = calculate_and_ensure_evaluating_pids([AppNode]),
    ?ProcLib:ensure_link(ExecInPid, StartType, PidNode),

    RegExecInPids = calculate_and_ensure_evaluating_pids(RegApps),

    [?ProcLib:ensure_link(RegApp, reg_def, PidNode)
     || RegApp <- RegApps],
    
    [?ProcLib:ensure_link(RegExecInPid, register, PidNode)
     || RegExecInPid <- RegExecInPids],

    create_gen_server_pids(Tl);
create_gen_server_pids([]) ->
    ok.

-spec calculate_and_ensure_gen_server_refs() -> ok.
%% @doc The function links the gen_server:call/2,3 and
%% gen_server:cast/2 applications to the corresponding pid nodes.
calculate_and_ensure_gen_server_refs() ->
    %TODO: do not forget replies
    CallApps = get_gen_server_calls(),
    CastApps = get_gen_server_casts(),

    CallPids = calculate_and_ensure_evaluating_pids(CallApps),
    CastPids = calculate_and_ensure_evaluating_pids(CastApps),

    ensure_gen_server_refs(lists:zip(CallApps, CallPids), sync_call),
    ensure_gen_server_refs(lists:zip(CastApps, CastPids), async_call).

-spec ensure_gen_server_refs(AppsAndEvaluatedInPids, LinkType) -> ok when
      AppsAndEvaluatedInPids :: {Application :: refcore_graph:gnode(),
                                 StartedPid :: refcore_graph:gnode()},
      LinkType :: start | start_link.
%% @doc Connects the `gen_server' starting (`start', `start_link')
%% expressions/applications to its evaluating process with `eval_in'
%% link. Creates edges between the evaluating process and the newly
%% created gen_server processes (the label of the link is created
%% based on the starting `start' or `start_link').
ensure_gen_server_refs(AppsAndEvaluatedInPids, LinkType) ->
    [begin
         ?ProcLib:ensure_link(App, eval_in, Pid),
         [?ProcLib:ensure_link(Pid, LinkType, GenSrvPid)
          || GenSrvPid <- get_gen_server_ref(hd(nth_arg(App,1)))]
     end || {App, Pid} <- AppsAndEvaluatedInPids],
    ok.


-spec get_gen_server_ref(ArgNode :: recore_graph:gnode()) -> [atom()].
%% @doc The function gets a node (first element of the arglist of the
%% call or cast application) and based on this node it tries to
%% calculate the referenced gen_server. If it finds a candidate,
%% returns it pid node.
get_gen_server_ref(ArgNode) ->
    TupleNames = get_server_name_candidates_with_back_reach([ArgNode]),

    AtomNames = get_atom_values_with_back_reach([ArgNode]),

    %Scope information is neglected
    Names = lists:usort([ N || {_, N} <- TupleNames] ++ AtomNames),

    Candidates =
        case Names of
            [] ->
                [];
            _  ->
                Pids = ?Graph:path(?Graph:root(), [pid]),
                PidData =
                    [{Pid, [N || {_, N} <- (?Graph:data(Pid))#pid.reg_names]}
                     || Pid <- Pids,
                        (?Graph:data(Pid))#pid.type == gen_server],
                
                [Pid || {Pid, RegNames} <- PidData,
                        ?MISC:intersect(Names, RegNames) /= []]
        end,
    P = get_gen_server_ref_based_on_pid(ArgNode),
    Candidates ++ P.

-spec get_gen_server_ref_based_on_pid(refcore_graph:gnode()) ->
                                             [refcore_core:graph()].
%% @doc The function gets the node of the first argument of call or
%% cast application and calculates which `gen_server' is referenced
%% based on the pid.
get_gen_server_ref_based_on_pid(ArgNode) ->
    NodeReach  = ?ProcLib:run(fun() ->?Dataflow:reach([ArgNode], [back]) end),
    BackSel   = lists:flatten([?Graph:path(N,[{sel, back}]) || N <- NodeReach]),
    NodeReach2 = ?ProcLib:run(fun() ->?Dataflow:reach(BackSel, [back]) end),
    Fun = fun(N) ->
                  Data = ?Graph:data(N),
                  Data#expr.type == application
          end,
    Apps = lists:filter(Fun, NodeReach2),
    lists:flatten([ ?Graph:path(A, [eval_in, S])
                    || A <- Apps, S <- [start, start_link]]).
-spec gen_server_start_apps_info(StartType, Arity) -> [{AppNode, 
                                                        ModName,
                                                        StartType,
                                                        SrvNames}] when
      Arity :: 3 | 4,
      StartType :: start | start_link,
      AppNode :: refcore_graph:gnode(),
      ModName :: atom(),
      SrvNames :: {local, atom()} | {global, atom()}.
%% @doc The function gets a function name `start' or `start_link' and
%% an arity (`3' or `4') and calculates information about the
%% applications of these functions.
gen_server_start_apps_info(StartType, Arity) ->
    Apps = ?Query:exec(
              ?Query:seq([?Mod:find(gen_server), ?Fun:find(StartType,Arity),
                          ?Fun:applications()])),
    Fun = fun(Node) -> 
                  get_gen_server_start_info(Arity, Node, StartType)
          end,
    lists:flatmap(Fun, Apps).

-spec get_atom_values_with_back_reach(Nodes::[refcore_graph:gnode()])->[atom()].
%% @doc The function calculates the possible atom values for the given
%% node with back data-flow reaching.
get_atom_values_with_back_reach(Nodes) ->
    NodeReach   = ?ProcLib:run(fun() ->?Dataflow:reach(Nodes, [back]) end),
    AtomNodes   = lists:filter(fun ?ProcLib:is_atom_expr/1, NodeReach),
    _AtomValues = lists:map(fun ?ProcLib:atom_value/1, AtomNodes).

-spec get_server_name_candidates_with_back_reach(Nodes :: refcore_graph:gnode())
                                                -> ReturnVal when
      ReturnVal :: [{local, atom()} | {global, atom()}] | [].
%% @doc The function calculates the possible tuples for the given node
%% with back data-flow reaching.
get_server_name_candidates_with_back_reach(Nodes) ->
    NodeReach  = ?ProcLib:run(fun() ->?Dataflow:reach(Nodes, [back]) end),

    TwoTupleNodes =
        lists:filter(fun(N) -> ?ProcLib:is_tuple_expr(N, 2) end, NodeReach),

    lists:flatmap(fun get_gen_server_name/1, TwoTupleNodes).
    
-spec get_gen_server_name(TupleNode) -> ReturnVal when
      TupleNode :: refcore_graph:gnode(),
      ReturnVal::[{Scope,atom()}] | [],
      Scope :: local | global | undefined.
%% @doc The function gets a graph node, especially a tuple node that
%% can be the first argument of a `start*' application and calculates
%% the possible name candidates using the data-flow analysis. The
%% return value is a list of tuples with `{Scope, SrvName}'. If the
%% scope (local, global) information is not available the `undefined'
%% atom is used.
get_gen_server_name(TupleNode) ->
    case ?Graph:path(TupleNode, [esub]) of
        [TagNode, NameNode] ->
            TagAtoms  =
                case get_atom_values_with_back_reach([TagNode]) of
                    [] -> [undefined];
                    List -> List
                end,
            NameAtoms = get_atom_values_with_back_reach([NameNode]),
            [{Tag, Name} || Tag <- TagAtoms,
                            lists:member(Tag, [local, global]),
                            Name <- NameAtoms];
        %% [_Via, _Module, _ViaName] ->
        %%     %TODO
        %%     undefined_functionality;
        _ ->
            []
    end.

-spec get_gen_server_start_info(Arity, AppNode, StartType) ->
                                       [{AppNode, ModName,
                                         StartType, SrvNames}] when
      Arity :: 3 | 4,
      AppNode :: refcore_graph:gnode(),
      StartType :: start | start_link,
      ModName :: atom(),
      SrvNames :: [{Scope, atom()}],
      Scope :: local | global | undefined.
%% @doc The function gets the arity of the examined application, the
%% node of the application and the name of the function. Returns a
%% list of tuples, where the first element is the node of the given
%% application, the second element is the callback module of the
%% gen_server, the third element is the start type (start or
%% start_link) and the fourth element is the list of possible names.
get_gen_server_start_info(3, AppNode, StartType) ->
    ModNode = nth_arg(AppNode, 1),
    ModNms  = get_atom_values_with_back_reach(ModNode),
    [ {N, MN, StartType, []} || {N, MN} <- node_info(AppNode, ModNms)];
get_gen_server_start_info(4, AppNode, StartType) ->
    SrvNameArg = nth_arg(AppNode, 1),
    ModNameArg = nth_arg(AppNode, 2),

    ModNms  = get_atom_values_with_back_reach(ModNameArg),

    SrvNms = get_server_name_candidates_with_back_reach(SrvNameArg),

    [{N, MN, StartType, SrvNms} || {N, MN} <- node_info(AppNode, ModNms)].


-spec analyse_separate_registering(StartApp :: refcore_graph:gnode()) ->
                                          {[refcore_graph:gnode()],
                                           [{local, atom()}]}.
%% @doc The function analyses registering applications. It uses
%% data-flow to determine registering applications where pid of the
%% started `gen_server' can flow.
analyse_separate_registering(StartApp) ->
    Reach = ?ProcLib:run(fun()-> ?Dataflow:reach([StartApp],
                                                 [{back, false}]) end),
    Fun = fun(Node) ->
                  ?ProcLib:is_tuple_expr(Node, 2) andalso
                      begin
                          Fst = ?Graph:path(Node, [{esub, 1}]),
                          Atoms = get_atom_values_with_back_reach(Fst),
                          lists:member(ok, Atoms)
                      end
          end,
    Tuples = lists:filter(Fun, Reach),
    Pids = lists:flatten([?Graph:path(Tuple, [{esub,2}]) || Tuple <- Tuples]),
    Reach2 = ?ProcLib:run(fun()-> ?Dataflow:reach(Pids,
                                             [{back, false}]) end),
    RegApps = find_reg_apps(Reach2),
    {RegNames, _} = find_reg_names(RegApps),
    {RegApps, [{local, Name} || Name <- RegNames]}.

-spec get_gen_server_calls()-> [refcore_graph:gnode()].
%% @doc The function returns the application nodes of the
%% gen_server:call/2 and gen_server:call/3 functions.
get_gen_server_calls() ->
    Apps2 = ?Query:exec(
               ?Query:seq([?Mod:find(gen_server), ?Fun:find(call,2),
                           ?Fun:applications()])),
    Apps3 = ?Query:exec(
               ?Query:seq([?Mod:find(gen_server), ?Fun:find(call,3),
                           ?Fun:applications()])),
    Apps2 ++ Apps3.

-spec get_gen_server_casts() -> [refcore_graph:gnode()].
%% @doc The function returns the application nodes of the
%% gen_server:cast/2 functions.
get_gen_server_casts() ->
    ?Query:exec(?Query:seq([?Mod:find(gen_server), ?Fun:find(cast,2),
                            ?Fun:applications()])).


-spec nth_arg(App :: refcore_graph:gnode(), N :: integer()) ->
                     [refcore_graph:gnode()].
%% @doc The function gets an application node `App' and a number `N',
%% it queries the nth argument of the application. It assumes that the
%% application has at least `N' arguments.
nth_arg(App, N) ->
    ?Query:exec(App, ?Query:seq([?Expr:child(2), ?Expr:child(N)])).

-spec node_info(Node :: refcore_graph:gnode(), Names :: [atom()]) ->
                       [{Node :: refcore_graph:gnode(), Name :: atom()}].
%% @doc The function creates a list of {Node, Atom} pairs from a
%% {Node, [Atom]} pair.
node_info(Node, []) ->
    [{Node, []}];
node_info(Node, Names) ->
    [ {Node, Name} || Name <- Names].

-spec match_send_and_rec([{SApp :: refcore_graph:gnode(),
                           FunList :: [refcore_graph:gnode()]}]) ->
                                [{SentMessages :: [refcore_graph:gnode()],
                                  ReceivePats ::  [refcore_graph:gnode()]}].
%% @doc The function analyses the registering applications and links
%% the registering application with `reg_def' link to pid
%% node. Updates the pid nodes if a name is found. The function
%% returns pairs in tuples: what messages are received at what
%% receive.
match_send_and_rec([{SApp, FunList} | Tail]) ->
%%    Reach = ?Dataflow:reach([SApp], [{back, false}]),
    Reach = ?ProcLib:run(fun()-> ?Dataflow:reach([SApp], [{back, false}]) end),
    RegApps = find_reg_apps(Reach),
    {RegANames, RegNames} = find_reg_names(RegApps),
    Pids = ?Graph:path(SApp, [spawn_def]),
    [?ProcLib:ensure_link(App, reg_def, Pid) || App <- RegApps, Pid <- Pids],
%?d(RegNames),
%    RegANames = lists:map(fun ?ProcLib:atom_value/1, RegNames),
    [?Graph:update(Pid, 
                   (?Graph:data(Pid))#pid{reg_names = lists:usort(RegANames)}) 
     || Pid <- Pids],
    SentMessages = find_sent_message(Reach ++ RegNames),
    ExtFunList = ?Query:exec(FunList, [funcall]) ++ FunList,
%% todo: add the full CG
    ExprL = ?Query:exec(ExtFunList, ?Query:seq([?Fun:definition(), 
                                                ?Form:clauses(), 
                                                ?Clause:body(), 
                                                ?Expr:deep_sub()])),
    ReceivePats = lists:append([?Graph:path(E, [exprcl, pattern]) || 
                                   E <- ExprL,?Expr:type(E) =:= receive_expr]),
    [{SentMessages, ReceivePats} | match_send_and_rec(Tail)];
match_send_and_rec([]) ->
    [].

-spec find_reg_names(Apps :: [refcore_graph:gnode()]) ->
                            {Atoms :: [atom()],
                             [refcore_graph:gnode()]}.
find_reg_names(Apps) ->
    Names = ?Query:exec(Apps, ?Query:seq(?Expr:child(2), ?Expr:child(1))),
    Atoms = get_atom_values_with_back_reach(Names),
    AtomNodes = ?ProcLib:atomnodes(Atoms),
    {Atoms, ?ProcLib:run(fun() -> ?Dataflow:reach(AtomNodes, 
                                                  [{back, false}]) 
                         end)}.
-spec find_reg_apps([Node::refcore_graph:gnode()]) ->
                           [AppNode :: refcore_graph:gnode()].
%% @doc The function gets a list of nodes that are arguments of an
%% application, it filters and returns application that are references
%% of the register/2 function.
find_reg_apps([E | Tail]) ->
    lists:filter(fun(A) -> 
                     ?Expr:type(A) =:= application andalso 
                     ?Graph:path(A, [{funlref, {{name, '==', register}, 'and',
                                                {arity,'==',2}}},
                                     {{func,back},{name,'==',erlang}}]) /= []
                 end, ?Query:exec(E, ?Query:seq(?Expr:parent(),
                                                ?Expr:parent()))) 
                      ++ find_reg_apps(Tail);
find_reg_apps([]) ->
    [].

-spec find_sent_message(Node :: refcore_graph:gnode()) ->
                               [MsgNode :: refcore_graph:gnode()].
%% @doc The function selects expressions from the list that are
%% arguments of a send expression and returns the second argument of
%% the send expression.
find_sent_message([R | Reach]) ->
    case ?Query:exec(R, ?Expr:parent()) of
        [P] -> case ?Expr:type(P) of 
                   send_expr -> [?Query:exec1(P, ?Expr:child(2), bad_node) | 
                                 find_sent_message(Reach)];
                    _        -> find_sent_message(Reach)
               end;
        _   -> find_sent_message(Reach)
    end;
find_sent_message([]) ->
    [].

-spec find_spawned_funs([{Fun              :: refcore_graph:gnode(),
                          SpawnAndFuncData :: [{S :: refcore_graph:gnode(),
                                                [func_data()]}]}],
                        Mode :: strict | heuristic) -> Return when
      Return :: [[{SApp :: refcore_graph:gnode(),
                   [Fun :: refcore_graph:gnode()]}]].
%% @doc The function returns a list of lists of tuples. The first
%% element of the tuple is a spawn application, the second element is a
%% list of function nodes. 
%% TODO: improve
find_spawned_funs([{_Fun, SpawnAndFuncData} | Tail], Mode) ->
    case find_funs_in_sapp(SpawnAndFuncData, Mode) of
        [] -> find_spawned_funs(Tail, Mode);
        S  -> [S | find_spawned_funs(Tail, Mode)]
              %%[{Fun, S} | find_spawned_funs(Tail)]
    end;
find_spawned_funs([], _) ->
    [].

-spec find_funs_in_sapp([{SApp        :: refcore_graph:gnode(), 
                          FunDataList :: [func_data()]}],
                        Mode :: strict | heuristic) ->
                               [{SApp :: refcore_graph:gnode(),
                                 Funs :: [refcore_graph:gnode()]}].
%% @doc The function calculates function nodes based on the given
%% function data information. Creates pid nodes and arranges the links
%% between the pids, root and spawn expressions.
find_funs_in_sapp([{_SApp, todo_more_heuristic} | Tail], Mode) ->
    find_funs_in_sapp(Tail, Mode);
find_funs_in_sapp([{SApp, FunDataList} | Tail], Mode) ->
    [{SApp, functions(SApp,FunDataList,Mode)} | find_funs_in_sapp(Tail, Mode)];
find_funs_in_sapp([], _) ->
    [].

-spec functions(SApp, [func_data()], Mode :: strict | heuristic) ->
                       Return when
      SApp :: refcore_graph:gnode(),
      Return :: [Fun],
      Fun  :: refcore_graph:gnode().
%% @doc The function gets an application a list of function
%% descriptions. It creates `pid' node in the graph and links it to
%% the root and the spawn application. The return value of the function
%% is a list of function nodes.
functions(SApp, [{M, F, undefined} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:find(M), [{func, {name, '==', F}}])),
    Pid = ?ProcLib:ensure_node(#pid{mod=M, func=F}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(Pid, spawn_def, SApp),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{M, undefined, A} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:find(M), [{func, {arity, '==', A}}])),
    Pid = ?ProcLib:ensure_node(#pid{mod=M, ary=A}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(Pid, spawn_def, SApp),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{undefined, F, A} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:all(), ?Fun:find(F,A))),
    Pid = ?ProcLib:ensure_node(#pid{func=F, ary=A}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(SApp, spawn_def, Pid),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{M, F, A} | FunList], Mode) ->
    case ?Query:exec(?Query:seq(?Mod:find(M), ?Fun:find(F,A))) of
        [Fun] -> 
            Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
            ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
            ?ProcLib:ensure_link(SApp, spawn_def, Pid),
            [Fun | functions(SApp, FunList, Mode)];
        _     -> 
            Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
            ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
            ?ProcLib:ensure_link(SApp, spawn_def, Pid),
            functions(SApp, FunList, Mode)
    end;
functions(_, [], _) ->
    [].

-spec find_spawn_expr(Arg1 | Arg2) -> ReturnVal1 | ReturnVal2 when
      Fun :: refcore_graph:gnode(),
      Arg1 :: [Fun], % Arg1 -> ReturnVal1
      ReturnVal1 :: [{Fun, ReturnVal2}],
      Arg2 :: Fun, % Arg2 -> ReturnVal2
      ReturnVal2 :: [{SApp , [func_data()]}],
      SApp :: refcore_graph:gnode().
%% @doc The function behaves differently depending on the type of its
%% argument. If it gets a function node first filters spawn
%% applications in its body, and calculates possible functions spawned
%% within this application. In this case it returns a list of tuples
%% {SpawnAppNode, ListOfFunctionData}. If it gets a list of function
%% nodes it returns a list of tuples {Fun, [{SpawnAppNode,
%% ListOfFunctionData}]} using the previous functionality.
find_spawn_expr([Fun | List]) ->
    [{Fun, find_spawn_expr(Fun)} | find_spawn_expr(List)];
find_spawn_expr([]) ->
    [];
find_spawn_expr(Fun) ->
    ExprL = ?Query:exec(Fun, ?Query:seq([?Fun:definition(), ?Form:clauses(), 
                                         ?Clause:body(),    ?Expr:deep_sub()])),
    SApps = [E || E <- ExprL, F <- ?Query:exec(E, ?Expr:function()), 
                  ?Expr:type(E) =:= application andalso 
                  ?Fun:name(F) =:= spawn_link andalso ?Fun:arity(F) =:= 3],
    [get_func_data(SApp) || SApp <- SApps].

-type func_data() :: {ModName  :: atom() | undefined,
                      FuncName :: atom() | undefined,
                      Arity    :: integer() | undefined}.
-spec get_func_data(SApp) -> [{SApp,
                               [FunData :: func_data()] |
                               todo_more_heristic}] when
      SApp :: refcore_graph:gnode().
%% @doc The function returns information about the spawn expression.
get_func_data(SApp) ->
    [_, ArgList] = ?Query:exec(SApp, ?Expr:children()),
    [MN, FN, AN] = ?Query:exec(ArgList, ?Expr:children()),
    case  {wrap(?CallAnal:lookup_ID(MN, undefined)),
           wrap(?CallAnal:lookup_ID(FN, undefined)),
           ?CallAnal:listcons_length(AN)} of
        {List1, List2, ArityL} when is_list(ArityL) andalso is_list(List1)
                                    andalso is_list(List2) ->
            {SApp, [{MName, FName, Arity} || {_, MName} <- List1, 
                                             {_, FName} <- List2,
                                             Arity <- ArityL]};
        {List1, List2, incalculable} when is_list(List1) andalso 
                                          is_list(List2) ->
            {SApp, [{MName, FName, undefined} || {_, MName} <- List1, 
                                                 {_, FName} <- List2]};
        {List1, undefined, ArityL} when is_list(ArityL) andalso 
                                        is_list(List1) ->
            {SApp, [{MName, undefined, Arity} || {_, MName} <- List1, 
                                                 Arity <- ArityL]};
        {undefined, List2, ArityL} when is_list(ArityL) andalso 
                                        is_list(List2) ->
            {SApp, [{undefined, FName, Arity} || {_, FName} <- List2, 
                                                 Arity <- ArityL]};
%% todo: List1, List2 -> [{}|...], ??? ambflow...
        _ -> {SApp, todo_more_heuristic}
    end.

-spec wrap({term(), term()} | term()) -> [{term(), term()}] | term().
%% @doc The function wraps a tuple to a list, for other type of
%% expression it leaves intact.
wrap(X = {_, _}) -> [X];
wrap(X) -> X.

-spec function_data(Fun :: refcore_graph:gnode()) -> func_data().
function_data(Fun) ->
    [Module] = ?Graph:path(Fun, [{func, back}]),
    ModName  = (?Graph:data(Module))#module.name,
    FData    = ?Graph:data(Fun),
    FunName  = FData#func.name,
    FunArity = FData#func.arity,
    {ModName, FunName, FunArity}.

find_pid_candidates({Expr, FunId, FunData}, PidData, Trace) ->
    case {lists:member(FunId, Trace), matches(FunData, PidData)} of 
        {true, _}       -> [];
        {_,  {true, P}} -> {existingpid, Expr, P};
        _ ->
            NewCallers = [C || C <- ?Graph:path(FunId, [{funcall,back}]),
                               not lists:member(C, Trace)],
            case NewCallers of
                [] -> {newpid, Expr, lists:last([FunId|Trace])};
                _  ->
                    PidCandL = [find_pid_candidates({Expr, C, function_data(C)},
                                                    PidData, [FunId | Trace])
                                || C <- NewCallers],
                    lists:usort(lists:flatten(PidCandL))
            end
    end.

-spec matches(func_data(), PidDataList) -> false | {true, Pid2} when
      PidDataList :: [{Pid1, func_data()}],
      Pid1 :: refcore_graph:gnode(),
      Pid2 :: refcore_graph:gnode().
%% @doc The function gets a description of a function and a list of
%% pid descriptions. It selects the first matching data and returns
%% the corresponding pid identifier.
matches({_FM, _FF, _FA}, []) -> false;
matches({FM, FF, FA}, [{P, {FM, FF, FA}} | _ ]) -> {true, P};
matches({FM, FF, _},  [{P, {FM, FF, undefined}} | _ ]) -> {true, P};
matches({FM, _, FA},  [{P, {FM, undefined, FA}} | _ ]) -> {true, P};
matches({_, FF, FA},  [{P, {undefined, FF, FA}} | _ ]) -> {true, P};
matches(F, [ _ | Tl]) -> matches(F, Tl).

-spec create_dot() -> ok | {error, Reason} when
      Reason :: file:posix() | badarg | system_limit.
%% @doc Creates a `dot' description of the process model. It uses the
%% results of the analysis.
create_dot() ->
    case ets:info(?PT) of
        undefined ->
            io:format("Process information is unavailable, " 
                      "please first run the analysis!");
        _ ->
            Data = [{[{Sn, SnL}, {En, EnL}], {Sn, En, L}}
                    || {{Sn, En}, SnL, L, EnL} <- qlc:eval(ets:table(?PT))],
            {Nodes, Edges} = lists:unzip(Data),
            UniqueNodes    = lists:usort(lists:concat(Nodes)),
            
            Path = filename:join([?MISC:data_dir(), "processes.dot"]),
            
            FileId =
                case file:open(Path, [write]) of
                    {ok, FI} -> FI;
                    {error, Reason1} ->
                        throw(Reason1)
                end,
            io:fwrite(FileId, "digraph{~n",[]),
            
            [io:fwrite(FileId, node_text(Node), []) || Node <- UniqueNodes],
            
            [io:fwrite(FileId, edge_text(Edge), []) || Edge <- Edges],
            io:fwrite(FileId, "}~n",[]),
            case file:close(FileId) of
                ok -> ok;
                {error, Reason2} ->
                    throw(Reason2)
            end
    end.

-spec node_text({NodeId :: refcore_graph:gnode(),
                 Label :: string()}) -> string().
%% @doc The function creates formatting for dot file creation for a
%% node.
node_text({NodeId, Label}) ->
    io_lib:format("\"~p\" [label=\"~s\"];~n", [NodeId, Label]).

-spec edge_text({SNodeId :: refcore_graph:gnode(),
                 ENodeId :: refcore_graph:gnode(),
                 Label :: string()}) -> string().
%% @doc The function creates formatting for dot file creation for an
%% edge.
edge_text({SNodeId, ENodeId, Label}) when is_list(Label) ->
    io_lib:format("\"~p\" -> \"~p\" [label=\"~s\"];~n", [SNodeId, ENodeId,
                                                         Label]);
edge_text({SNodeId, ENodeId, Label}) when is_tuple(Label) ->
    {Tag, String} = Label,
    io_lib:format("\"~p\" -> \"~p\" [label=\"{~p, ~s}\"];~n", [SNodeId, ENodeId,
                                                               Tag, String]);
edge_text({SNodeId, ENodeId, Label}) when is_atom(Label) ->
    io_lib:format("\"~p\" -> \"~p\" [label=\"~p\"];~n", [SNodeId, ENodeId,
                                                         Label]).
    



