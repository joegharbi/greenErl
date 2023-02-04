-module(refusr_dupcode_graph).
-export([
get_root/0,
hashNode/1,
%hashEdge/2
get_all_vertices/0,
get_vertices/1,
sort_all_vertices/0,
get_edges/1,
get_all_edges/0,
get_all_edge_types/0,
get_syntax_edges/0,
combine_edges/2
]).
-include("user.hrl").
%-include_lib("referl_core/src/core.hrl").
-include_lib("referl_core/src/refcore_schema.hrl").

get_root() ->
    {'$gn',root,0}.

hashNode({Gn,Type,Id}) ->
    Links = ?Graph:links({Gn,Type,Id}),
    %?d(Links),
    Data = lists:concat(lists:sort([Type | [Link || {Link,_} <- Links]])),
    %?d(Data),
    crypto:hash(md5, Data).
    
hash_vertex({Gn,Type,Id}) ->
    crypto:hash(md5, Type).
    
hash_edge(V1,V2) ->
    Hv1 = hash_vertex(V1),
    Hv2 = hash_vertex(V2),
    Hv1+Hv2.

get_all_vertices() ->
    get_vertices([get_root()]).

get_vertices([]) ->
        [];
get_vertices([X|XS]) ->
    ChildrenNoNametag = [Child || {_ , Child} <- ?Syn:children(X)],
    NewQueue = XS ++ ChildrenNoNametag,
    [X | get_vertices(NewQueue)].


%Sort vertices by type into a map    
sort_all_vertices() ->
    M = sort_vertices([get_root()],maps:new()),
    M.
    
sort_vertices([], SortedByType) ->
    SortedByType;
sort_vertices([X|XS],SortedByType) ->
    ChildrenNoNametag = [Child || {_ , Child} <- ?Syn:children(X)],
    NewQueue = XS ++ ChildrenNoNametag,
    {_,VertType,_} = X,
    Entry = maps:find(VertType, SortedByType),
    case Entry of
        {ok, VertList} -> 
            sort_vertices(NewQueue,maps:update(VertType, [X | VertList], SortedByType));
        error ->
            sort_vertices(NewQueue,maps:put(VertType, [X], SortedByType))
    end.
    
get_edges(Vertex) ->
    Neighbours = refcore_esg:links(Vertex),
    [{Vertex, V, Link} || {Link, V} <- Neighbours].
    
get_all_edges() ->
    AllVertices = get_all_vertices(),
    AllEdges = lists:flatten([ get_edges(V) ||  V <- AllVertices]),
    AllEdges.
    
get_all_edge_types() ->
    AllVertices = get_all_vertices(),
    sets:to_list(sets:from_list(lists:flatten([ [Type || {_,_,Type} <- get_edges(V)] ||  V <- AllVertices]))).
    
syntax_edge_labels_set() ->
     sets:from_list(lists:flatten([[ Link || {Link, _} <- L] || {_,_,L} <- ?SYNTAX_SCHEMA])).
     
get_syntax_edges() ->
    AllVertices = get_all_vertices(),
    SyntaxEdgeLabelsSet = syntax_edge_labels_set(),
    lists:flatten([ [{V1, V2, T} || {V1, V2, T} <- get_edges(V), sets:is_element(T,SyntaxEdgeLabelsSet)] ||  V <- AllVertices]).
    
%isomorphism vertex    
combine_edges({[X11|XS11], [X12|XS12], T1},{[X21|XS21], [X22|XS22], T2}) ->
    {[X11|XS11] ++ [X21|XS21], [X12|XS12] ++ [X22|XS22], T1};
combine_edges({V11, V12, T1},{V21, V22, T2}) ->
    {[V11, V21], [V12, V22], T1}.


sort_into_buckets() ->
    SyntaxEdgeLabelsSet = syntax_edge_labels_set(),
    ok.
    %buckets = sets:size(SyntaxEdgeLabelsSet).

%---------------------------------------  
 %Tipusok (a pelda kodban)   
%[top,dep,eattr,visib,esub,varvis,guard,varbind,file,module,
 %name,scope,sel_e,clause,cons_e,env,moddef,fundef,vardef,
 %modref,funlref,varref,funeref,flow,flex,elex,clex,functx,
 %modctx,body,cons_back,incl,funcl,sel,exprcl,headcl,call,
 %form,pattern]
 
%syntax_schema: refcore_erl_schema.hrl(referl_core)
%lexical_schema: refcore_schema.hrl (referl_core)

%refcore_esg.erl

%reflib_draw_graph.erl
%record_info

%Teljes lista megjelenitese terminalban: rp(Term)

%--------------------------------------

%hashEdge(Node1, Node2) ->
%    crypto:hash(md5, Data).
