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

%%% @doc Utilities of the CDG server.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(refsc_cdg_utils).

-vsn("$Rev$ ").

%% Interface function for CDG server.
-export([get_involved_funs/1, build_cdg/2, build_compound_cdg/1, add_edge/4,
         slice/2, slice/0]).

%% Visualization of the graph.
-export([draw_basic_graph/1, draw_compound_graph/1, draw_every_graph/2]).

%% Exported types.
-export_type([cdg_node/0, cdg_edge/0]).

%% callbacks
-export([closure/2]).

-include("slicer.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface functions provided to the CDG server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Represents nodes of the graphs, mainly nodes from the RefErl
%% SPG, but it also represent special nodes used for the CFG and CDG
%% graphs.
-type cdg_node() :: tuple().

%% Represents the edges of the CDG graph. A treetuple of `Start',
%% `End' and label elements.
-type cdg_edge() :: tuple().


-spec build_cdg(CFGInfo :: {cdg_node(), [cdg_edge()]},
                PostDomInfo :: {[cdg_edge()], atom()}) ->
                       {CDGEdges :: [cdg_edge()]}.
%% @doc The function gets the root vertex, CFG edges, immediate
%% postdom structure and returns a list of dependence edges that
%% formulates FCDG graph.
%% @TODO: The function should be improved...
build_cdg({Root, CFGEdges}, {ImmPostdomEdges, _Type}) ->
    Graph = digraph:new(),
    [add_edge(Graph, SNode, ENode, L) || {SNode, ENode, L} <- ImmPostdomEdges],

    BCDG = build_basic_cdg(Root, CFGEdges, ImmPostdomEdges, Graph),

    digraph:delete(Graph),
    BCDG.


build_compound_cdg(Funs) ->
    CallG      = digraph:new([cyclic, protected]),
    CDGEdgeTab = ets:new(cdggraph_tab, [bag, private]),
    PDEdgeTab  = ets:new(pdgraph_tab, [bag, private]),
    
    [case ?Query:exec(Fun, ?Fun:definition()) of
         [FunForm] ->
             digraph:add_vertex(CallG, FunForm),
             [case ?Query:exec(Called, ?Fun:definition()) of
                  [] -> ok;
                  [CalledForm] ->
                      digraph:add_vertex(CallG, CalledForm),
                      digraph:add_edge(CallG, FunForm, CalledForm, [])
              end || Called <- ?Query:exec(Fun, ?Fun:funcalls())];
         [] ->
             ok
     end || Fun <- Funs],

    FunForms = digraph_utils:postorder(CallG),

    refsc_cdg_server:build(FunForms),
    Components = digraph_utils:strong_components(CallG),

    Graphs = refsc_cdg_server:get(FunForms),

    LoadingFun =
        fun({F, {PD, Type}, CDG, CallSrcs}, {Dict, CallSrcAcc}) ->
                ets:insert(CDGEdgeTab, CDG),
                ets:insert(PDEdgeTab, PD),
                {orddict:store(F, Type, Dict),
                 CallSrcs ++ CallSrcAcc}
        end,
    {PDDict, CallSrcs} =
        lists:foldl(LoadingFun, {orddict:new(), []}, Graphs),

    UpdPDDict = update_pd_states(FunForms, CallG, Components, PDDict),

    add_inh_and_res_edges(CallSrcs, UpdPDDict, CDGEdgeTab, PDEdgeTab),


    resolve_inh_and_res_edges(CDGEdgeTab),
    %% TODO: handle loops

    Edges = ets:select(CDGEdgeTab, [{{'$1', '$2', '$3'}, [], ['$_']}]),

    digraph:delete(CallG),
    ets:delete(CDGEdgeTab),
    ets:delete(PDEdgeTab),

    Edges.

%% Constructs the CFG for the function with the given name and the
%% related functions. It includes only the called functions, the
%% functions calling the current function will be included later.
get_involved_funs(Forms) ->
    Fun      = ?Query:exec(Forms, ?Form:func()),
    % computing of call chains are spawned on localnode
    FwKey    = rpc:async_call(node(), ?MODULE, closure, [Fun, forward]),
    BwKey    = rpc:async_call(node(), ?MODULE, closure, [Fun, backward]),
    % reading the results of the closures
    BwCallCh   = rpc:yield(BwKey),
    BwFwKey    = rpc:async_call(node(), ?MODULE, closure, [BwCallCh, forward]),
    FwCallCh   = rpc:yield(FwKey),
    BwFwCallCh = rpc:yield(BwFwKey),

    ordsets:union(FwCallCh, BwFwCallCh).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The function returns the closure of the function forward call
%% chain. It gets a list of functions and returns the list of called
%% functions, including the starting functions.
%% @private
closure(Functions, Direction) ->
    OrdSet = ordsets:from_list(Functions),
    case Direction of
        forward  ->
            Fun =
                fun(N) ->
                        ?Query:exec(N, ?Fun:funcalls())
                end,
            iteration(OrdSet, OrdSet, Fun);
        backward ->
            Fun =
                fun(N) ->
                        ?Query:exec(N, ?Fun:called())
                end,
            iteration(OrdSet, OrdSet, Fun)
    end.

iteration(OrdSet, [], _Fun) -> OrdSet;
iteration(OrdSet, [Head | Tail], Fun) ->
    CalledFuns = Fun(Head),
    {NewOrdSet, NewWorkSet} =
        lists:foldl(fun(E, {OS, WS})->
                            case ordsets:is_element(E, OS) of
                                false ->
                                    {ordsets:add_element(E, OS),
                                     ordsets:add_element(E, WS)};
                                true ->
                                    {OS,WS}
                            end
                    end, {OrdSet, Tail}, CalledFuns),
    iteration(NewOrdSet, NewWorkSet, Fun).

slice() ->
    %% Retrieve changed nodes
    DTab       = ?RefChangeTab,
    {ok, Ref}  = dets:open_file(DTab, [{type, set}]), 
    ChNodes    = dets:select(Ref, [{{'$1', changed}, [], ['$1']}]), 
    dets:close(DTab),

    %% Determine the functions
    Forms =
        lists:usort(
          lists:append([?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                      ?Clause:form()]))
                        || Expr <- ChNodes])),
    CDGEdges = refsc_cdg_server:get_compound_cdg(Forms),

    %% Init ETS tab for storing edges, loading CDG edges

    Tab =  ets:new(slice_tab, [bag, private]),
    ets:insert(Tab, CDGEdges),
    
    %% Data dependence edges

    Nodes =
        ets:select(Tab, [{{'$1', '$2', '_'}, [], ['$1','$2']}]),

    Exprs =
        [ N || N = {'$gn', _, _} <- Nodes,refcore_syntax:node_type(N) =:= expr],
    
    Data = refsc_data_dep:calc_dep_safe(Exprs),
    ets:insert(Tab, Data),

    Fun =
        fun(N) ->
                lists:flatten(ets:match(Tab, {N, '$1', '_'}))
        end,
    OrdSet       = ordsets:from_list(ChNodes),
    Slice = iteration(OrdSet, OrdSet, Fun),
    ets:delete(Tab),
    Props = lists:usort(refsc_qc:qc_props_form_exprs(Slice)),
    [ {?Mod:name(?Query:exec1(Fun, ?Fun:module(), bad_fun)), 
       ?Fun:name(Fun), 
       ?Fun:arity(Fun)} || Fun <- Props].


slice(Node, CDGEdges) ->
    Tab =  ets:new(slice_tab, [bag, private]),
    ets:insert(Tab, CDGEdges),

    Nodes =
        ets:select(Tab, [{{'$1', '$2', '_'}, [], ['$1','$2']}]),

    Exprs =
        [ N || N = {'$gn', _, _} <- Nodes,refcore_syntax:node_type(N) =:= expr],

    Data = refsc_data_dep:calc_dep_safe(Exprs),

    ets:insert(Tab, Data),

    Fun =
        fun(N) ->
                lists:flatten(ets:match(Tab, {N, '$1', '_'}))
        end,
    OrdSet       = ordsets:add_element(Node, ordsets:new()),
    Slice = iteration(OrdSet, OrdSet, Fun),
    ets:delete(Tab),
    Slice.


add_inh_and_res_edges(SRCs, PDDict, CDGEdgeTab, PDEdgeTab) ->
    [begin
         CalledFunForm = called_fun(Src),
         CalledFunType = orddict:find(CalledFunForm, PDDict),
         case {CalledFunForm, CalledFunType} of
             {error, _} ->
                 FollNodes =
                     lists:delete(Src, 
                                  get_post_nodes(Src,SRCs,PDEdgeTab,PDDict,[])),
                 [[Parent]] =
                     ets:match(CDGEdgeTab, {'$1', Src, dd}),
                 Siblings =
                     lists:append(
                       ets:match(CDGEdgeTab, {Parent, '$1', dd})),
                 FollSib =
                     ?MISC:intersect(Siblings, FollNodes),
                 [ets:match_delete(CDGEdgeTab, {Parent, FS, dd}) ||
                     FS <- FollSib],
                 [ets:insert(CDGEdgeTab, {Src, FollN, dd}) ||
                     FollN <- FollNodes];

             {CalledFunForm, {ok, forest}} ->
                 FollNodes =
                     lists:delete(Src, 
                                  get_post_nodes(Src,SRCs,PDEdgeTab,PDDict,[])),
                 [[Parent]] =
                     ets:match(CDGEdgeTab, {'$1', Src, dd}),
                 Siblings =
                     lists:append(
                       ets:match(CDGEdgeTab, {Parent, '$1', dd})),
                 FollSib =
                     ?MISC:intersect(Siblings, FollNodes),
                 [ets:match_delete(CDGEdgeTab, {Parent, FS, dd})
                  || FS <- FollSib],
                 ets:insert(CDGEdgeTab,
                            {Src, CalledFunForm, inhdep}),
                 [ets:insert(CDGEdgeTab,
                             {{ret,CalledFunForm}, FollN, resdep})
                  || FollN <- FollNodes];
             
             {CalledFunForm, {ok, tree}} ->
                 ets:insert(CDGEdgeTab, {Src, CalledFunForm, inhdep})
         end
     end ||
         Src <- SRCs],
    ok.

resolve_inh_and_res_edges(CDGEdgeTab) ->
    InhEdges = ets:select(CDGEdgeTab, [{{'$1', '$2', inhdep}, [], ['$_']}]),
    ResEdges = ets:select(CDGEdgeTab, [{{'$1', '$2', resdep}, [], ['$_']}]),
    
    [begin
         Ps  = ets:select(CDGEdgeTab, [{{'$1', S, '_'}, [], ['$1']}]),
         Chs = ets:select(CDGEdgeTab, [{{E, '$1', '_'}, [], ['$1']}]),
         [ ets:insert(CDGEdgeTab, {P, Ch, dd}) || P <- Ps, Ch <- [E| Chs]],
         [ ets:match_delete(CDGEdgeTab, {E, Ch, dd}) || Ch <- Chs],
         ets:match_delete(CDGEdgeTab, Edge)
     end || {S, E, inhdep} = Edge <- InhEdges],
    
    [begin
         Ps  = ets:select(CDGEdgeTab, [{{'$1', S, '_'}, [], ['$1']}]),
         [ ets:insert(CDGEdgeTab, {P, E, dd}) || P <- Ps],
         ets:match_delete(CDGEdgeTab, Edge)
     end || {S, E, resdep} = Edge <- ResEdges].

get_post_nodes({exit_node, _}, _SRCs, _PDEdgeTab, _PDDict, Acc) ->
    Acc;
get_post_nodes(Node, SRCs, PDEdgeTab, PDDict, Acc) ->
    [{Node, Targ, _Label}] = ets:lookup(PDEdgeTab, Node),
    case lists:member(Targ, SRCs)
        andalso (
          %% the application is funcall for a function that may fail
          orddict:find(called_fun(Targ), PDDict) =:= {ok, forest} orelse
          %% or the definition of the function is not available
          orddict:find(called_fun(Targ), PDDict) =:= error) of
        true ->
            [Node | Acc];
        false ->
            get_post_nodes(Targ, SRCs, PDEdgeTab, PDDict, [Node | Acc])
    end.

called_fun(Src) ->
    case ?Query:exec(Src, ?Query:seq(?Expr:function(), ?Fun:definition())) of
        [] ->
            error;
        [N] ->
            N
    end.

%% add_inherited_and_resuption_edges(EtsTab, R, IF) ->
%%     [begin
%%          ets:delete_object(EtsTab, Edge),
%%          ets:insert(EtsTab, {{ret, CalledFunForm}, E, resdep})
%%      end || {{_S, E , potdep} = Edge, CalledFunForm} <- R],
%%     [begin
%%          ets:delete_object(EtsTab, Edge),
%%          ets:insert(EtsTab, {S, E, inhdep})
%%      end || {S,E, potdep} = Edge <- IF].

%% resolve_ppdom_edges([], _PdDict, R, IF) ->
%%     {R, IF};
%% resolve_ppdom_edges([{S, _, potdep} = P | PotDepEdges], PdDict, R, IF) ->
%%     % assumes that the function is available in SPG, must be improved
%%     % to handle such cases
%%     [CalledFunForm] =
%%         ?Query:exec(S, ?Query:seq(?Expr:function(),?Fun:definition())),
%%     case orddict:fetch(CalledFunForm, PdDict) of
%%         tree   -> resolve_ppdom_edges(PotDepEdges, PdDict, R, [P | IF]);
%%         forest -> resolve_ppdom_edges(PotDepEdges, PdDict,
%%                                       [{P, CalledFunForm} | R], IF)
%%     end.


update_pd_states([], _CallG, _Components, PdDict) ->
    PdDict;
update_pd_states([Form | Forms], CallG, Components, PdDict) ->
    case orddict:fetch(Form, PdDict) of
        tree   -> update_pd_states(Forms, CallG, Components, PdDict);
        forest ->
            {Group, NewComponents} =
                get_group_and_update_comps(Form, Components, []),
            Callers = digraph:in_neighbours(CallG, Form),
            Fun =
                fun(Elem, DictAcc) ->
                        orddict:store(Elem, forest, DictAcc)
                end,
            NewPdDict = lists:foldl(Fun, PdDict, lists:usort(Group ++ Callers)),
            update_pd_states(Forms, CallG, NewComponents, NewPdDict)
    end.

get_group_and_update_comps(_Form, [], VisitedGroups) ->
    {[], VisitedGroups};
get_group_and_update_comps(Form, [Group | Groups], VisitedGroups) ->
    case lists:member(Form, Group) of
        true  -> {Group, Groups ++ VisitedGroups};
        false ->
            get_group_and_update_comps(Form, Groups, [Group | VisitedGroups])
    end.

build_basic_cdg(Root, CFGEdges, ImmPostdomEdges, Graph) ->
    Ret = ?EXIT(Root),
    NotPostDomEdges  = select_edges_for_dep(CFGEdges, ImmPostdomEdges),
    Dependencies =
        [ begin
              FromBegToRoot =
                  digraph:get_path(Graph, FromVert, Ret),
              FromEndToRoot =
                  case ToVert of
                      Ret ->
                          [Ret];
                      _ ->
                          digraph:get_path(Graph, ToVert, Ret)
                  end,
              LowCommAnc =
                  get_lowest_common_ancestor(FromBegToRoot, FromEndToRoot),
              PathToCommAnc = lists:takewhile(fun(Vert) ->
                                                      Vert /= LowCommAnc
                                              end, FromEndToRoot),
              {FromVert, PathToCommAnc}
          end || {FromVert, ToVert} <- NotPostDomEdges],

    DependenceEdges =
        [{Vertex, Dep, dd} || {Vertex, Deps} <- Dependencies, Dep <- Deps],
    DependenceEdges.


add_edge(Graph, SNode, ENode, L)->
    digraph:add_vertex(Graph, SNode),
    digraph:add_vertex(Graph, ENode),
    digraph:add_edge(Graph, SNode, ENode, L).

%% Selecting edges which are present in the CFG but are not present in
%% the immediate postdominator relation. In the given list the first
%% elements of tuples are the vertices of the CFG. It is exploited
%% that the entire CFG is covered in the given list. The given list is
%% unique respect to the first element of the tuples. That means every
%% vertex has a unique postominator vertex (see the definition of the
%% postominator relation).
select_edges_for_dep(CFGEdges, ImmPostDoms) ->
    lists:append(
      [ begin
            OutEdges      = proplists:lookup_all(SVertex, CFGEdges),
            OutNeighbours = [ ENode || {_SNode, ENode, _L} <- OutEdges],
            case OutNeighbours of
                []        -> [];
                [PVertex] -> [];
                _         ->
                    [{SVertex, Vertex}
                     || Vertex <- lists:delete(PVertex, OutNeighbours)]
            end
        end || {SVertex, PVertex,_} <- ImmPostDoms]).

%% The given paths must have at least one common vertex. The function
%% is used to determine the lowest common ancestor in two (not
%% necessarily different) paths starting from the same source.  The
%% function will fail in case the given paths does not have any vertex
%% in common. The first is processed element by element and checked
%% whether the current vertex is included in the second path.
get_lowest_common_ancestor([Vertex | Path1], Path2) ->
    IsMember = lists:member(Vertex, Path2),
    case IsMember of
        true ->
            Vertex;
        _ ->
            get_lowest_common_ancestor(Path1, Path2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Drawing utility
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw_basic_graph(Form) ->
    [{_Form, _PD, BCDG, _CallSrcs}] = refsc_cdg_server:get([Form]),
    refsc_utils:draw_graph(cdg, text, BCDG).

draw_compound_graph(Form) ->
    CCDG = refsc_cdg_server:get_compound_cdg(Form),
    refsc_utils:draw_graph(ccdg, text, CCDG).

draw_every_graph(Form, Type) ->
    Funs  = get_involved_funs([Form]),
    Forms = ?Query:exec(Funs, ?Fun:definition()),

    CCDG  = refsc_cdg_server:get_compound_cdg(Form),
    refsc_utils:draw_graph(ccdg, Type, CCDG),

    CFGs  =
        lists:append([begin
                          {F, CFG, _} = refsc_cfg_server:get_cfg(F),
                          CFG
                      end || F <- Forms]),
    refsc_utils:draw_graph(cfgs, Type, CFGs),

    {Postdoms, BCDGs} =
        lists:foldl(
          fun({_, {PD, _}, BCDG, _}, {PDs, CDGs}) ->
                  {PD ++ PDs, BCDG ++ CDGs}
          end,
          {[], []}, refsc_cdg_server:get(Forms)),

    refsc_utils:draw_graph(postdoms, Type, Postdoms),
    refsc_utils:draw_graph(bcdgs, Type, BCDGs).
