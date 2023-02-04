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

%%% @doc Map analyser.
%%%
%%% @author Sandor Varga <vasa@caesar.elte.hu>

-module(refanal_map).
-vsn("$Rev:  $"). %% for emacs
-behaviour(refcore_anal).

%% Callback functions
-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([anal_map/0, is_equal_in_maps/2]).

%-define(mnesia, true).

-include("core.hrl").

%%% ============================================================================
%%% Schema, externs

%%% @private
schema() ->
    [{mapkey, record_info(fields, mapkey), []},
     {expr,   [{keyref, mapkey}, {keydef, mapkey}]},
     {map,    record_info(fields, map), [{key, mapkey}]},
     {form,   [{map, map}]},
     {expr,   [{mapdef, map}, {mapref, map}]}    
    ].

%%% @private
externs(_) -> [].

%%% ============================================================================
%%% Insert

%%% @private
insert(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of       
	#form{} ->
	    walk(fun add_del/4, [{Child, Child}], add_ref);
	#clause{} ->
	    Func = parent_form(Child),
  	    walk(fun add_del/4, [{Child, Func}], add_ref);
	#expr{type = T} when T == assoc_map_expr orelse
    			     T == empty_map_expr orelse
			     T == map_update ->
	    Func = parent_form(Child),
    	    walk(fun add_del/4, [{Child, Func}], add_ref),
	    on_update(Parent);
	#expr{type = T} when T == assoc_list orelse
			     T == key_value_list -> 
	    Func = parent_form(Child),
	    case get_src_maps(Parent, false) of 
		[{_,_,Id}] -> MapSpec = {Func, Id};	    
		[]	   -> MapSpec = {Func, unknown}
	    end,
	    walk(fun add_del/4, [{Child, MapSpec}], add_ref),
	    on_update(Parent); 
	#expr{type = infix_expr, value = V} when V == '=>' orelse
						 V == ':=' ->
	    Func = parent_form(Child),
	    case get_src_maps(Parent, false) of
		[{_,_,Id}] -> MapSpec = {Func, Id};
		[]	   -> MapSpec = {Func, unknown}
	    end,
	    walk(fun add_del/4, [{Child, MapSpec}], add_ref),
	    on_update(Parent);
	#expr{} ->
	    Func = parent_form(Child),
	    walk(fun add_del/4, [{Child, Func}], add_ref),
	    on_update(Parent);
	_ -> 
	    ok
    end.

%%% ============================================================================
%%% Remove

%%% @private
remove(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of       
	#form{} ->
	    walk(fun add_del/4, [{Child, Child}], del_ref);
	#clause{} ->
  	    walk(fun add_del/4, [{Child, Parent}], del_ref);
	#expr{type = T} when T == assoc_map_expr orelse
    			     T == empty_map_expr orelse
			     T == map_update ->	    	   
	    on_remove_map_expr(Parent, Child); 
	#expr{type = T} when T == assoc_list orelse
			     T == key_value_list ->
	    IEs = ?Anal:children(Child),
	    [on_remove_infix_expr(Parent, IE) || {esub, IE} <- IEs],
	    on_update(Parent);
	#expr{type = infix_expr, value = V} when V == '=>' orelse
						 V == ':=' ->	    
	    on_remove_infix_expr(Parent, Child),
	    on_update(Parent);
	#expr{} ->	    
	    on_remove_infix_expr_subexpr(Parent, Child);
	_ -> 
	    ok
    end.

%%% ============================================================================
%%% Update

%%% @private
update(Expr, #expr{}) -> on_update(Expr);
update(_,_)	      -> ok.

%%% ============================================================================
%%% Event handler functions for key expressions changes

on_remove_map_expr(Parent, MExpr) ->
    ie_desc_check_n_process(Parent, MExpr),
    IEs = ?Graph:path(MExpr, [esub, esub]),
    [on_remove_infix_expr(Parent, IE) || IE <- IEs].

on_remove_infix_expr(Parent, IE) ->
    case get_src_maps(Parent, false) of
	[] -> ok;
	[_] ->
	    [KeyExp] = ?Graph:path(IE, [{esub, 1}]),
	    safe_clean_key_expr(KeyExp)   
    end,
    Func = parent_form(Parent),
    Args = [{Ch, Func} || {esub, Ch} <- ?Anal:children(IE)],
    walk(fun add_del/4, Args, del_ref).

on_remove_infix_expr_subexpr(Parent, Expr) ->
    case ie_desc_check_n_process(Parent, Expr) of
	ok ->
	    Func = parent_form(Parent),
	    walk(fun add_del/4, [{Expr, Func}], del_ref);
	skip ->
	    ok
    end.


on_update(Expr) ->
    case parent_pair(Expr) of
	[]   -> ok;
	[IE] ->
	    case get_src_maps(IE, false) of
		[] -> ok;
		[MapExp] -> 
		    [KeyExp] = ?Graph:path(IE, [{esub, 1}]),
		    {_,_,KeyId} = KeyExp,
		    safe_clean_key_expr(KeyExp),
		    PrevKeyExps = get_prev_key_exps(IE),
		    CFun = fun is_equal_subgraph/2,
		    case search_equal(KeyExp, PrevKeyExps, CFun) of
			[PKExp] ->
			    ?NodeSync:add_ref(key, {ref, KeyExp}, PKExp);
			[] ->
			    NextKeyExps = get_next_key_exps(IE),
			    case search_equal(KeyExp, NextKeyExps, CFun) of
				[] ->
				    Spec = {MapExp, KeyId},
				    ?NodeSync:add_ref(key, {def, KeyExp}, Spec);
				[NKExp] ->
				    [NKey] = ?Graph:path(NKExp, [keydef]),
				    Data = ?Graph:data(NKey),
				    ?NodeSync:add_ref(key, {def, KeyExp}, NKey),
				    ?NodeSync:del_ref(key, {def, NKExp}, NKey),
				    ?NodeSync:add_ref(key, {ref, NKExp}, NKey),
				    ?Graph:update(NKey, Data#mapkey{id=KeyId})
			    end
		    end,
		    on_update(MapExp)
 	    end 
    end.

%% @doc Check that the given Expr node is a descendant of infix_expr from 
%% constant map, and if it's right then process this properly.
%% NOTE: Parent is a node from the subgraph has been cut off.
ie_desc_check_n_process(Parent, Expr) ->
    case parent_pair(Parent) of
	[] -> skip;
	[IE] ->
	    case get_src_maps(IE, false) of
		[] -> skip;
		[_] -> 
		    case Parent =:= IE of
			true  -> safe_clean_key_expr(Expr);
			false -> on_update(IE)				 
		    end
	    end
    end.

safe_clean_key_expr(KeyExp) ->
    case ?Graph:path(KeyExp, [keydef]) of
	[Key] ->
	    ?NodeSync:del_ref(key, {def, KeyExp}, Key),
	    case ?Graph:path(Key, [{keyref, back}]) of
		[H|_] ->
		    D = ?Graph:data(Key),
		    {_,_,NewKeyId} = H,
		    ?NodeSync:del_ref(key, {ref, H}, Key),
		    ?NodeSync:add_ref(key, {def, H}, Key),
		    ?Graph:update(Key, D#mapkey{id=NewKeyId});
		[] ->
		    ok
	    end;
	[] ->
	    case ?Graph:path(KeyExp, [keyref]) of
		[Key] -> 
		    ?NodeSync:del_ref(key, {ref, KeyExp}, Key);
		[] ->
		    ok
	    end
    end.

%%% ============================================================================
%%% Node processor functions

%% Level: 1

add_del(_Cmd, Form, #form{}, Func) ->
    [{Cl, Func} || {_, Cl} <- ?Anal:children(Form)];
%add_del(_Cmd, _Form, #form{}, _Func) -> 
%    [];

%% Level: 2

add_del(_Cmd, Clause, #clause{}, Func) ->
    [{Ch, Func} || {_, Ch} <- ?Anal:children(Clause)];

%% MinLevel: 3

add_del(Cmd, Expr, #expr{type = T}, Func) when T == assoc_map_expr orelse
					       T == empty_map_expr ->
    {_,_,MapId} = Expr,
    MapSpec = {Func, MapId},
    ?NodeSync:Cmd(map, {def, Expr}, MapSpec),
    [{KvpList, MapSpec} || {_, KvpList} <- ?Anal:children(Expr)];

add_del(Cmd, Expr, #expr{type = map_update}, Func) ->       
    case get_src_maps(Expr, false) of
	[{_,_,MapId}] -> ?NodeSync:Cmd(map, {ref, Expr}, {Func, MapId});
	[]            -> MapId = unknown
    end,
    %% LeftSide  IN [empty_map_expr, assoc_map_expr, map_update, variable]
    %% RightSide IN [key_value_list]
	[{esub, LS}, {esub, RS}] = ?Anal:children(Expr),
    LST = case ?Anal:data(LS) of
	      #expr{type = T} when T == assoc_map_expr orelse
				   T == empty_map_expr orelse
				   T == map_update ->
		  [{LS, Func}];
	      #expr{type = variable} ->
		  [];
		  #expr{type = parenthesis} ->
		  []
	  end,
    LST ++ [{RS, {Func, MapId}}];

%% MinLevel: 4

add_del(_Cmd, Expr, #expr{type = T}, MapSpec) when T == assoc_list orelse
						   T == key_value_list -> 
    Children = ?Anal:children(Expr),
    [{KeyVal, MapSpec} || {esub, KeyVal} <- Children];

%% MinLevel: 5

add_del(Cmd, Expr, #expr{type = infix_expr}, MapSpec = {Func, MapId}) ->
    case MapId of
	unknown -> skip;
	_       ->
	    [KExp] = ?Graph:path(Expr, [{esub, 1}]),	    
	    PrevKeyExps = get_prev_key_exps(Expr),
	    CFun = fun is_equal_subgraph/2,
	    case search_equal(KExp, PrevKeyExps, CFun) of
		[] -> 
		    do_with_forward_check(Cmd, Expr, KExp, MapSpec);
		[PKExp] ->
		    {_,_,PKId} = PKExp,
		    ?NodeSync:Cmd(key, {ref, KExp}, {MapSpec, PKId})
	    end
    end,
    [{Ch, Func} || {esub, Ch} <- ?Anal:children(Expr)]; %% For MapInMap

%% Otherwise

add_del(_Cmd, Expr, #expr{}, Func)->
    [{Ch, Func} || {_, Ch} <- ?Anal:children(Expr)];

add_del(_Cmd, Expr, #typexp{}, Func)->
    [{Ch, Func} || {_, Ch} <- ?Anal:children(Expr)];
add_del(_, _, _, _) ->
    [].

%%% ============================================================================
%%% Tree traversal

walk(Fun, [{Node, Spec} | Tail], Cmd) ->
    walk(Fun, Fun(Cmd, Node, ?Anal:data(Node), Spec) ++ Tail, Cmd);
walk(_, [], _) ->
    ok.

%%% ============================================================================
%%% Assistant functions for analyzing

%% @doc Given a node of expression, return its parent function form.
%parent_fun(Node) ->
%    case ?Anal:data(Node) of
%	#expr{}   -> Path = ?Query:seq(?Expr:clause(), ?Clause:form());
%	#clause{} -> Path = ?Query:seq(?Clause:form())
%    end,
%    [Form] = ?Graph:path(Node, Path),
%    Form.

parent_form(Node) ->
	case ?Anal:data(Node) of
		#form{} -> Node;
		_ -> parent_form(?Anal:parent(Node))
	end.

%% @doc Given a part of subgraph of map key expression, returns its parent 
%% infix_expr node.
parent_pair(Node) ->
    case ?Anal:data(Node) of
	#expr{type = infix_expr, value = V} when V == '=>' orelse V == ':=' ->
	    [Node];
	#file{} ->
	    [];
	_ -> 
	    parent_pair(?Anal:parent(Node))
    end.

%% @doc Performs the 'Cmd' command with forward check.
do_with_forward_check(Cmd, IE, KExp, MapSpec) ->
    NextKeyExps = get_next_key_exps(IE),
    case Cmd of
	add_ref -> RevCmd = del_ref, Tag = keydef;
	del_ref -> RevCmd = add_ref, Tag = keyref
    end,
    Cond = fun(N) -> ?Graph:path(N, [Tag]) =/= [] end,
    CFun = fun is_equal_subgraph/2,
    NextEqual = search_equal_by_cond(KExp, NextKeyExps, CFun, Cond),
    case NextEqual of
	[] -> 
	    {_,_,KeyId} = KExp,
	    ?NodeSync:Cmd(key, {def, KExp}, {MapSpec, KeyId});
	[NKExp] ->
	    [Key] = ?Graph:path(NKExp, [Tag]),
	    Data = ?Graph:data(Key),
	    case Cmd of
		add_ref -> {_,_,NewKeyId} = KExp;
		del_ref -> {_,_,NewKeyId} = NKExp
	    end,
	    ?NodeSync:Cmd(key, {def, KExp}, Key),
	    ?NodeSync:RevCmd(key, {def, NKExp}, Key),
	    ?NodeSync:Cmd(key, {ref, NKExp}, Key),
	    ?Graph:update(Key, Data#mapkey{id=NewKeyId})
    end.

%% @doc Given a node of infix_expr with value '=>', returns all syntactic key 
%% expressions which is prevents the key of specified infix_expr.
get_prev_key_exps(IE) ->
    IEP = ?Anal:parent(IE),
    KNth = ?Graph:index(IEP, esub, IE), 
    MapExp = ?Anal:parent(IEP),
    Path = [{esub, {1, KNth}}, {esub, 1}],
    collect_sub_keys(MapExp) ++ ?Graph:path(IEP, Path).

collect_sub_keys(MapExp) ->
    [LS] = ?Graph:path(MapExp, [{esub, 1}]),
    case ?Graph:data(LS) of
	#expr{type = map_update} ->
	    collect_sub_keys(LS) ++ ?Graph:path(LS, [{esub, 2}, esub, {esub, 1}]);
	#expr{type = T} when T == assoc_map_expr orelse
			     T == empty_map_expr ->
	    ?Graph:path(LS, [esub, esub, {esub, 1}]);
	_ -> []
    end.

%% @doc Given a node of infix_expr with value '=>', returns all syntactic key 
%% expressions which is follows the key of specified infix_expr.
get_next_key_exps(IE) ->
    IEP = ?Anal:parent(IE),
    KNth = ?Graph:index(IEP, esub, IE),
    MapExp = ?Anal:parent(IEP),
    Path = [{esub, {KNth+1, last}}, {esub, 1}],
    ?Graph:path(IEP, Path) ++ collect_sup_keys(MapExp).

collect_sup_keys(MapExp) ->
    P = ?Anal:parent(MapExp),
    case ?Graph:data(P) of
	#expr{type = map_update} ->
	    ?Graph:path(P, [{esub, 2}, esub, {esub, 1}]) ++ collect_sup_keys(P);
	_ -> []
    end.

%% @doc Given a node of expression and the analysis status, return the original 
%% map expressions.
get_src_maps(Node, IsPostAnal) ->
    Data = ?Graph:data(Node),
    Typ = Data#expr.type,
    Val = Data#expr.value,
    case Typ of	
        T when T == assoc_map_expr orelse
	       T == empty_map_expr ->			     
	    pick(IsPostAnal, ?Graph:path(Node, [mapdef]), [Node]);
	variable when IsPostAnal ->	  
	    Reach = ?Dataflow:reach_1st([Node], [{back, true}]),
            lists:flatten([?Graph:path(N, [mapdef]) || N <- Reach]);
	map_update -> 
	    [LS] = ?Graph:path(Node, [{esub, 1}]),
	    get_src_maps(LS, IsPostAnal);
        T when T == assoc_list orelse
	       T == key_value_list -> 
	    [MapExp] = ?Graph:path(Node, [{esub, back}]),
	    get_src_maps(MapExp, IsPostAnal);
	infix_expr when Val == '=>' orelse Val == ':=' ->
	    Path = [{esub, back}, {esub, back}],
	    [MapExp] = ?Graph:path(Node, Path),
	    get_src_maps(MapExp, IsPostAnal);
	_ ->
	    []
    end.

%% @doc Selector function instead of 'case' statement.
pick(Cond, CaseOfTrue, CaseOfFalse) ->
    case Cond of
	true  -> CaseOfTrue;
        false -> CaseOfFalse
    end.    

%%% ============================================================================
%%% Analyser interface functions

%% Cmd = insert | remove

anal_map() ->
    clean(),
    add_missed_map_refs().

%%% ============================================================================
%%% Functions for post analyzing
    
%% @doc Returns the list of useful functions of maps module.
get_maps_mod_funs() ->
    [{find, 1, 2}, {get, 1, 2}, {is_key, 1, 2},
     {put, 1, 3}, {remove, 1, 2}, {update, 1, 3},
     {with, 1, 2}, {without, 1, 2}].

%% @doc Detect the functions of maps module, which have key in its argument list
%% and afther then add/del references from these. 
manage_maps_mod_key_refs(Cmd, FunName, KeyNr, MapNr) ->
    Fun = ?Query:exec(?Query:seq([?Mod:find(maps), ?Mod:local(FunName, 2)])),
    Apps = ?Query:exec(Fun, ?Fun:applications()),
    Drw = fun(KExp, KExps) ->
	      CFun = fun is_equal_subgraph/2,
	      case search_equal(KExp, KExps, CFun) of
		  [SKeyExp] ->
		      [Key] = ?Graph:path(SKeyExp, [keydef]),
		      ?Graph:Cmd(KExp, keyref, Key);
		  [] -> ok
	      end
	  end,
    lists:foreach(fun(App) ->
	[MapExp] = ?Graph:path(App, [{esub, 2}, {esub, MapNr}]),
	Maps = ?Graph:path(MapExp, [mapref]),
	[KeyExp] = ?Graph:path(App, [{esub, 2}, {esub, KeyNr}]),	
	lists:foreach(fun(Map) ->
	    KeyExps = ?Graph:path(Map, [key, {keydef, back}]),	    
	    case ?Graph:data(KeyExp) of
		#expr{type = variable} ->
		    Drw(KeyExp, KeyExps); 
		#expr{} when FunName == with orelse
			     FunName == without -> 
		    KExps = ?Graph:path(KeyExp, [esub, esub]),
		    [Drw(KExp, KeyExps) || KExp <- KExps];
		#expr{} -> 
		    Drw(KeyExp, KeyExps)
	    end
	end, Maps)
    end, Apps).

%% @doc Add the missed map references.
add_missed_map_refs() ->
    MapExps = all_expr_node([empty_map_expr, assoc_map_expr]),
    lists:foreach(fun(MExpr) ->	
	[Map] = ?Graph:path(MExpr, [mapdef]),
	Reach = ?Dataflow:reach_1st([MExpr], [{back, false}]),
	Types = [variable, exact_map_expr],
        UsefulNs = [ N || N <- Reach, lists:member(?Expr:type(N), Types)],
	lists:foreach(fun(Node) ->
	    ?Graph:mklink(Node, mapref, Map),
	    case ?Expr:type(Node) of
		variable ->
		    MapUpds = map_upd_in_var(Node),
		    lists:foreach(fun(MapUpd) ->
			?Graph:mklink(MapUpd, mapref, Map),
			Path = [{esub, 2}, esub, {esub, 1}],
			KExps = ?Graph:path(MapUpd, Path),
			[add_missed_key_refs(KExpr, Map) || KExpr <- KExps]
		    end, MapUpds);
		exact_map_expr ->
		    KExps = ?Graph:path(Node, [esub, esub, {esub, 1}]),
		    [add_missed_key_refs(KExpr, Map) || KExpr <- KExps]
	    end
	end, UsefulNs)
    end, MapExps),
    Funs = get_maps_mod_funs(),
    [manage_maps_mod_key_refs(mklink,FName,KNr,MNr) || {FName,KNr,MNr} <- Funs].

%% @doc Drop the missed map references.
clean() ->
    MapExps = all_expr_node([empty_map_expr, assoc_map_expr]),
    lists:foreach(fun(MExpr) ->	
	Reach = ?Dataflow:reach_1st([MExpr], [{back, false}]),
	Types = [variable, exact_map_expr],
        UsefulNs = [ N || N <- Reach, lists:member(?Expr:type(N), Types)],
	lists:foreach(fun(Node) ->
	    case ?Graph:path(Node, [mapref]) of
		[] ->
		    skip;
		[Map] ->
		    ?Graph:rmlink(Node, mapref, Map),
		    case ?Expr:type(Node) of
			variable ->
			    MapUpds = map_upd_in_var(Node),
			    lists:foreach(fun(MapUpd) ->
				?Graph:rmlink(MapUpd, mapref, Map),
				Path = [{esub, 2}, esub, {esub, 1}],
				KExps = ?Graph:path(MapUpd, Path),
				[drop_missed_key_refs(KExpr) || KExpr <- KExps]
			    end, MapUpds);
			exact_map_expr -> 
			    KExps = ?Graph:path(Node, [esub, esub, {esub, 1}]),
			    [drop_missed_key_refs(KExpr) || KExpr <- KExps]
		    end
	    end
	end, UsefulNs)
    end, MapExps),
    Funs = get_maps_mod_funs(),
    [manage_maps_mod_key_refs(rmlink,FName,KNr,MNr) || {FName,KNr,MNr} <- Funs],
    drop_missed_nodes(). %% remaining from remove

%% @doc Given a node of variable expression, returns all map_update node which 
%% try to update this map variable directly.
map_upd_in_var(Node) ->
    PExpr = ?Graph:path(Node, [{esub, back}]),
    case PExpr of
	[N] ->
	    case ?Expr:type(N) of
		map_update -> [N] ++ map_upd_in_var(N);
		_	   -> []
	    end;
	_   -> 
	    []
    end.

%% @doc Add the missed map key references.
add_missed_key_refs(KExpr, Map) -> 
    KExps = ?Graph:path(Map, [key, {keydef, back}]),
    CFun = fun is_equal_subgraph/2,
    case search_equal(KExpr, KExps, CFun) of
	[SKExp] ->
	    [SKey] = ?Graph:path(SKExp, [keydef]),
	    ?Graph:mklink(KExpr, keyref, SKey);
	[] -> 
	    Key = make_key(KExpr),
	    ?Graph:mklink(Map, key, Key)	
    end.

make_key(KExpr) ->
    {_,_,KeyId} = KExpr,
    case ?Graph:path(KExpr, [keydef]) of
	[Key] -> Key;
	[]    ->
	    Key = ?Graph:create(#mapkey{id=KeyId}),
	    ?Graph:mklink(KExpr, keydef, Key), Key
    end.

%% @doc Drop missed map key references.
drop_missed_key_refs(KExpr) ->
    case ?Graph:path(KExpr, [keydef]) of
	[] -> skip;
	[Key] ->
 	    ?Graph:rmlink(KExpr, keydef, Key),
	    KExps = ?Graph:path(Key, [{keyref, back}]),
	    [?Graph:rmlink(KE, keyref, Key) || KE <- KExps],
	    ?Graph:delete(Key)
    end,
    [?Graph:rmlink(KExpr, keyref, Key) || Key <- ?Graph:path(KExpr, [keyref])].

%% @doc Drop missed semantic map and mapkey nodes. (Remaining from remove.)
drop_missed_nodes() ->
    Path=?Query:seq([?Mod:all(),?Mod:locals(),?Fun:definition(),?Form:maps()]),
    AllMap = ?Graph:path(?Graph:root(), Path),
    [begin
	Keys = ?Graph:path(Map, [key]),
	[begin
	    ?Graph:rmlink(Map, key, Key),
	    ?Graph:delete(Key)
	 end || Key <- Keys, ?Graph:path(Key, [{keydef, back}]) == []],
	case ?Graph:path(Map, [{mapdef, back}]) of
	    [] ->
		[Form] = ?Graph:path(Map, [{map, back}]),
		?Graph:rmlink(Form, map, Map),
		?Graph:delete(Map);
	    _ ->
		ok
	end
     end || Map <- AllMap].

%% @doc Given a node of expression, a list of expression nodes, and a compare 
%% function, returns the first node from the list which is equal the specified
%% node according to the compare function.
search_equal(KeyA, [KeyB|T], CompFun) ->
    case CompFun(KeyA, KeyB) of
	true  -> [KeyB];
	false -> search_equal(KeyA, T, CompFun)
    end;
search_equal(_, [], _) -> [].

%% @doc Given a node of expression, a list of expression nodes, and a compare 
%% function, returns the first node from the list which is equal the specified
%% node according to the compare function and fulfills the condition.
search_equal_by_cond(KeyA, Lst, CompFun, Cond) ->
    NewCompFun = fun(A, B) ->  
		     case Cond(B) of
			 true  -> CompFun(A, B);
			 false -> false   
		     end
		 end,
    search_equal(KeyA, Lst, NewCompFun).

%% @doc Given two node of expression as root A and root B, the function compare
%% the subgraphs and if these are equals in expressions return true else false.
is_equal_subgraph(NodeA, NodeB) ->
    walk_with_compare([{NodeA, NodeB}]).

walk_with_compare([{A, B} | Tail]) ->        
    walk_with_compare(compare_expr_nodes(A, B) ++ Tail);
walk_with_compare([{diff}|_])->
    false;
walk_with_compare([]) ->
    true.

compare_expr_nodes(NodeA, NodeB) ->
    V1 = (?Graph:data(NodeA))#expr.value,
    V2 = (?Graph:data(NodeB))#expr.value,
    case V1 =:= V2 of
	true ->
	    CEA = ?Graph:path(NodeA, [esub]),
	    CEB = ?Graph:path(NodeB, [esub]),
	    case length(CEA) == length(CEB) of
		true  -> lists:zip(CEA, CEB);
		false -> [{diff}]
	    end;
	false ->
	    [{diff}]
    end.

%% @doc Note that the former version returns all the specified type expressions,
%% while the latter one only gives those inside modules. (Skeleton copied from:
%% refanal_dynfun.erl)
-ifdef(mnesia). %................................................. ifdef(mnesia)

all_expr_node(Types) -> 
    all_exprs(fun mnesia_get_nodes/1, [Types]).

all_exprs(Selector, SArgs) ->
    {atomic, Res} = mnesia:transaction(Selector, SArgs),
    lists:map(fun(N) -> {'$gn', expr, N} end, Res).

mnesia_get_nodes(Types) ->
    MatchHeads = [{expr, '$1', #expr{type=T, _='_'}, '_'} || T <- Types],
    MatchSpec = [{MH, [], ['$1']} || MH <- MatchHeads],
    mnesia:select(expr, MatchSpec).

-else. %................................................................... else

all_expr_node(Types) ->
    all_exprs(fun(Expr) -> lists:member(?Expr:type(Expr), Types) end).

all_exprs(Filter) ->
    AllExprs = ?Query:exec(?Query:seq([?Mod:all(),
                                       ?Mod:locals(),
                                       ?Fun:definition(),
                                       ?Form:clauses(),
                                       ?Clause:body(),
                                       ?Expr:deep_sub()])),
    lists:filter(Filter, AllExprs).

-endif. %................................................................. endif

%%% ============================================================================
%%% Functions for testing support

%% @spec is_equal_in_maps(node(), node()) -> ok
%% @doc Given two nodes of module, the function compare them with maps 
%% investigation and retuns 'ok' if these equal or else throw exception. 
is_equal_in_maps(ModA, ModB) ->    
    MapsA = ?Query:exec(ModA, ?Query:seq([?Mod:locals(), 
					  ?Fun:definition(),
					  ?Form:maps()])),
    MapsB = ?Query:exec(ModB, ?Query:seq([?Mod:locals(), 
					  ?Fun:definition(),
					  ?Form:maps()])),
    case length(MapsA) == length(MapsB) of
	true  -> 
	    IMapDef = fun(Map) -> ?Query:exec(Map, ?Map:mapdef()) end,
	    MapDefsA = lists:flatmap(IMapDef, MapsA),
	    MapDefsB = lists:flatmap(IMapDef, MapsB),
	    case is_equal_node_list(MapDefsA, MapDefsB) of
		true  -> ok;
		false -> throw(diff_in_maps_def)
	    end,
	    IMapRef = fun(Map) -> ?Query:exec(Map, ?Map:references()) end,
	    MapRefsA = lists:flatmap(IMapRef, MapsA),
	    MapRefsB = lists:flatmap(IMapRef, MapsB),
	    case is_equal_node_list(MapRefsA, MapRefsB) of
		true  -> ok;
		false -> throw(diff_in_maps_ref)
	    end,
	    IKeys = fun(Map) -> ?Query:exec(Map, ?Map:keys()) end,
	    KeysA = lists:flatmap(IKeys, MapsA),
	    KeysB = lists:flatmap(IKeys, MapsB),
	    case length(KeysA) == length(KeysB) of
		true  -> 
		    IKeyDef = fun(Key) -> ?Query:exec(Key,?MapKey:keydef()) end,
		    KeyDefsA = lists:flatmap(IKeyDef, KeysA),
		    KeyDefsB = lists:flatmap(IKeyDef, KeysB),
		    case is_equal_node_list(KeyDefsA, KeyDefsB) of
			true  -> ok;
			false -> throw(diff_in_maps_keys_def)
		    end,
		    IKeyRef=fun(Key)->?Query:exec(Key,?MapKey:references()) end,
		    KeyRefsA = lists:flatmap(IKeyRef, KeysA),
		    KeyRefsB = lists:flatmap(IKeyRef, KeysB),
		    case is_equal_node_list(KeyRefsA, KeyRefsB) of
			true  -> ok;
			false -> throw(diff_in_maps_keys_ref)
		    end;
		false -> 
		    throw(diff_in_maps_keys_num)
	    end;
	false -> 
	    throw(diff_in_maps_num)
    end.    

is_equal_node_list([H|T], L = [_|_]) ->
    CFun = fun is_equal_node/2,
    case search_equal(H,L,CFun) of
	[N] -> is_equal_node_list(T, lists:delete(N, L));
	[ ] -> false
    end;
is_equal_node_list([_|_], []) -> false;
is_equal_node_list([], [_|_]) -> false;
is_equal_node_list([], []) -> true;
is_equal_node_list(_, _) -> false.

is_equal_node(A, B) -> ?Expr:value(A) =:= ?Expr:value(B).

%% Clean all semantic map nodes and refs under the specified function.
%clean_sub_graph(Func) ->
%    Maps = ?Graph:path(Func, [map]),
%    Fun = fun(M) -> ?Graph:path(M, [key]) end,
%    Keys = lists:flatmap(Fun, Maps),
%    lists:foreach(fun(Key) ->
%	[DNode] = ?Graph:path(Key, [{keydef, back}]),
%	?NodeSync:del_ref(key, {def, DNode}, Key),
%	RNodes = ?Graph:path(Key, [{keyref, back}]),
%	[?NodeSync:del_ref(key, {ref, RNode}, Key) || RNode <- RNodes]
%    end, Keys),
%    lists:foreach(fun(Map) ->
%	[DNode] = ?Graph:path(Map, [{mapdef, back}]),
%	?NodeSync:del_ref(map, {def, DNode}, Map),
%	RNodes = ?Graph:path(Map, [{mapref, back}]),
%	[?NodeSync:del_ref(map, {ref, RNode}, Map) || RNode <- RNodes]
%    end, Maps).

%% Given a node of map_update, returns true if it's top, else false.
%is_top_map_upd(MapUpd) ->
%    PExp = ?Graph:path(MapUpd, [{esub, back}]),
%    case PExp of
%	[ ] -> true;
%	[N] -> (?Graph:data(N))#expr.type =/= map_update
%   end.

%% Given a node of map_update (A), returns a list of map_update where all 
%% element of list is inherited from the specified map_update (A).
%group_map_upd(MapUpd) ->
%    [LS] = ?Graph:path(MapUpd, [{esub, 1}]),
%    Typ = (?Graph:data(LS))#expr.type,
%    case Typ of
%	map_update ->
%	    group_map_upd(LS) ++ [MapUpd];
%	_ ->
%	    [MapUpd]
%    end.
