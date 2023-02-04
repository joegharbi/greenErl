-module(reftr_elim_hof).
-export([prepare/1, error_text/2]).
-include("user.hrl").

error_text(ambiguous, _) -> 
	["A query is ambiguous, while it should only return a single value."];
error_text(bad_application, Type) ->
	["Selected expression must be an application, no application found up to expr type: ", ?MISC:format("~p",[Type])];
error_text(bad_hof, [AllowedHOFs, {Mod, Fun, Arity}]) ->
	[?MISC:format("Selected function call must be of the following: ~p, not: ~p:~p/~p", [AllowedHOFs, Mod, Fun, Arity])];
error_text(invalid_fun_arg, Type) ->
	["The first argument of the HOF must be fun_expr or implicit_fun, not: ", ?MISC:format("~p",[Type])];
error_text(pattern_num, N) ->
	[?MISC:format("The fun expr passed to the hof must contain exactly ~p parameters", [N])];
error_text(implicit_arity, N) ->
	[?MISC:format("The implicit function passed to the hof must have arity ~p", [N])].

prepare(Args) ->
	?d(Args),
	File = ?Args:file(Args),
	ExprSelected = ?Args:expression(Args),
	Expr = get_closest_parent_of_type(ExprSelected, application),

	Fun = ?Query:exec1(Expr, ?Expr:function(), ?LocalErr0r(ambiguous)),
    {_, MFA} = ?Fun:mod_fun_arity(Fun),

	validate_hof(MFA),

	FunType = get_fun_type(Expr),

	HOFArgs = ?Query:exec(Expr, ?Query:seq(?Expr:child(2), ?Expr:children())),

	DefaultMapping = [{implicit_fun, list_comp},
					  {fun_expr, recursive}],

	Mode = case get_mode(Args) of
		default -> 
			DefMap = element(2 ,lists:keyfind(FunType, 1, DefaultMapping)),
			case FunType of
				fun_expr -> PatternsGuardsBodies = get_patterns_gurads_bodies(lists:nth(1, HOFArgs)),
							Branch = determine_clause_type(PatternsGuardsBodies),
							case Branch of
								single_body -> list_comp;
								_ -> recursive
							end;
				_ -> DefMap
			end;
		M -> M
	end,

	transform_hof_call(MFA, Mode, FunType, Expr, HOFArgs, File).

get_closest_parent_of_type(Node, Type) ->
	case ?Expr:type(Node) of
		Type -> Node;
		OtherType -> Parent = ?Query:exec1(Node, ?Expr:parent(), ?LocalError(bad_application, OtherType)),
					 get_closest_parent_of_type(Parent, Type)
	end.

validate_hof(MFA) ->
	AllowedHOFs = [{lists, map, 2},
				   {lists, filter, 2}],
	?Check(lists:member(MFA, AllowedHOFs), ?LocalError(bad_hof, [AllowedHOFs, MFA])).

get_mode(Args) ->
	case lists:keyfind(hof_mode, 1, Args) of
		{_, Mode} when Mode =:= recursive orelse 
					   Mode =:= list_comp orelse 
					   Mode =:= default -> Mode;
		_ -> ask_hof_question("Please select transformation mode:")
	end.

ask_hof_question(Text) ->
	Question = get_hof_question(Text),
	Response = ?Transform:question(Question),
	case parse_response(Response) of
		invalid -> ask_hof_question("You must select a transformation mode:");
		ParsedResponse -> ParsedResponse
	end.

get_hof_question(Text) ->
	[add_to_proplist(info, Text, false)] ++ 
	[add_to_proplist(radio, "Recursive function", true)] ++
	[add_to_proplist(radio, "List comprehension", true)] ++
	[add_to_proplist(radio, "Default (let the refactoring decide)", true)].

parse_response([_, yes, no, no]) -> recursive;
parse_response([_, no, yes, no]) -> list_comp;
parse_response([_, no, no, yes]) -> default;
parse_response(_) -> invalid.

add_to_proplist(Format, Text, Default) ->
		[{format, Format}, {text, Text}, {default, Default}].

get_fun_type(Expr) ->
	ArgList = ?Query:exec1(Expr, ?Expr:child(2), ?LocalErr0r(ambiguous)),
	FunArg = ?Query:exec1(ArgList, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	FunArgType = ?Expr:type(FunArg),
	?Check(lists:member(FunArgType, [fun_expr, implicit_fun]), ?LocalError(invalid_fun_arg, FunArgType)),
	FunArgType.

% lists:map(fun f/1, List) =>
% [f(Elem1) || Elem1 <- List]
transform_hof_call({lists, map, 2}, list_comp, implicit_fun, Expr, Args, _File) ->
	ImplicitFunArg = lists:nth(1, Args),
	ImplicitFun = ?Query:exec1(ImplicitFunArg, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	check_implicit_fun_arity(ImplicitFunArg, 1),
	[fun() ->
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		NewImplicitFun = copy(ImplicitFun),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),
		ElementVar = ?Syn:construct({var, ElementVarName}),

		HeadApp = ?Syn:construct({app, NewImplicitFun, [ElementVar]}),

		ElementPattern = ?Syn:construct({var, ElementVarName}),

		NewList = copy(lists:nth(2, Args)),

		ListComp = create_list_comp(HeadApp, [{ElementPattern, NewList}]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

transform_hof_call({lists, map, 2}, list_comp, fun_expr, Expr, Args, _File) ->
	FunExpr = lists:nth(1, Args),

	PatternsGuardsBodies = get_patterns_gurads_bodies(FunExpr),

	Branch = determine_clause_type(PatternsGuardsBodies),

	ListArg = lists:nth(2, Args),

	map_list_comp_from_fun_expr(Branch, Expr, PatternsGuardsBodies, ListArg);

% lists:map(fun increase/1, List) =>
% map_recursion1(List)
% 
% map_recursion1([]) -> [];
% map_recursion1([Head | Tail]) -> [increase(Head) | map_recursion1(Tail)].
transform_hof_call({lists, map, 2}, recursive, implicit_fun, Expr, Args, File) ->
	ImplicitFunArg = lists:nth(1, Args),
	ImplicitFun = ?Query:exec1(ImplicitFunArg, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	% Create recursive function
	[fun() -> 
		?Transform:touch(File),
		FunName = new_funname_with_prefix(File, "map_recursion"),

		DefaultClause = create_default_clause(FunName, 1, 1),

		HeadName = "Head",
		HeadVar = ?Syn:construct({var, HeadName}),

		TailName = "Tail",
		TailVar = ?Syn:construct({var, TailName}),

		ListHeadPattern = create_list_head_pattern(HeadVar, TailVar),

		NewImplicitFun = copy(ImplicitFun),
		HeadBodyVar = ?Syn:construct({var, HeadName}),
		HeadApp = ?Syn:construct({app, NewImplicitFun, [HeadBodyVar]}),

		TailBodyVar = ?Syn:construct({var, TailName}),
		TailApp = create_fun_call({FunName}, [TailBodyVar]),

		Body = create_list_head_pattern(HeadApp, TailApp),

		MainClause = create_clause_from_name(FunName, [ListHeadPattern], [], [Body]),

		Func = ?Syn:construct({func, [DefaultClause, MainClause]}),

		?File:add_form(File, Func),
		FunName
	end,
	% Replace map call with recursive call
	fun(FunName) ->
		NewList = copy(lists:nth(2, Args)),
		replace_expr_with_call(Expr, FunName, [NewList])
	end];

% X = 1,
% lists:map(fun(X) when X =:= 42 -> Y = X + 2, Y + 3;
%			   (Elem) -> Elem + X end,
%			List) =>
% map_recursion1(List)
% 
% map_recursion1([], _) -> [];
% map_recursion1([X | Tail1], X1) -> Y = X + 2, [Y + 3 | map_recursion1(Tail1, X1)];
% map_recursion1([Elem | Tail1], X) -> [Elem + X | map_recursion1(Tail1, X)];
transform_hof_call({lists, map, 2}, recursive, fun_expr, Expr, Args, File) ->
	transform_recursion_fun_expr(Expr, Args, File, fun map_transform_pattern/2,  fun map_transform_last_body/5, "map_recursion");

% lists:filter(fun f/1, List) =>
% [Elem1 || Elem1 <- List, f(Elem1)]
transform_hof_call({lists, filter, 2}, list_comp, implicit_fun, Expr, Args, _File) ->
	ImplicitFunArg = lists:nth(1, Args),
	ImplicitFun = ?Query:exec1(ImplicitFunArg, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	[fun() ->
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),

		HeadElement = ?Syn:construct({var, ElementVarName}),

		ElementPattern = ?Syn:construct({var, ElementVarName}),

		NewList = copy(lists:nth(2, Args)),

		NewImplicitFun = copy(ImplicitFun),
		ElementVar = ?Syn:construct({var, ElementVarName}),
		FilterApp = ?Syn:construct({app, NewImplicitFun, [ElementVar]}),

		ListComp = create_list_comp(HeadElement, [{ElementPattern, NewList}], [FilterApp]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

transform_hof_call({lists, filter, 2}, list_comp, fun_expr, Expr, Args, _File) ->
	FunExpr = lists:nth(1, Args),

	PatternsGuardsBodies = get_patterns_gurads_bodies(FunExpr),

	Branch = determine_clause_type(PatternsGuardsBodies),

	ListArg = lists:nth(2, Args),

	filter_list_comp_from_fun_expr(Branch, Expr, PatternsGuardsBodies, ListArg);

% lists:filter(fun even/1, List) =>
% filter_recursion1(List)
% 
% filter_recursion1([]) -> [];
% filter_recursion1([Head | Tail]) -> 
% 	case even(Head) of
% 		true -> [Head | filter_recursion1(Tail)];
%		false -> filter_recursion1(Tail)
%	end.
transform_hof_call({lists, filter, 2}, recursive, implicit_fun, Expr, Args, File) ->
	ImplicitFunArg = lists:nth(1, Args),
	ImplicitFun = ?Query:exec1(ImplicitFunArg, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	% Create recursive function
	[fun() -> 
		?Transform:touch(File),
		FunName = new_funname_with_prefix(File, "filter_recursion"),

		DefaultClause = create_default_clause(FunName, 1, 1),

		HeadName = "Head",
		HeadVar = ?Syn:construct({var, HeadName}),

		TailName = "Tail",
		TailVar = ?Syn:construct({var, TailName}),

		ListHeadPattern = create_list_head_pattern(HeadVar, TailVar),

		NewImplicitFun = copy(ImplicitFun),
		HeadBodyVar1 = ?Syn:construct({var, HeadName}),
		ImplicitFunApp = ?Syn:construct({app, NewImplicitFun, [HeadBodyVar1]}),

		HeadBodyVar2 = ?Syn:construct({var, HeadName}),

		TailBodyVar1 = ?Syn:construct({var, TailName}),
		RecApp = create_fun_call({FunName}, [TailBodyVar1]),

		TrueAtom = ?Syn:construct({atom, true}),
		TrueBranchBody = create_list_head_pattern(HeadBodyVar2, RecApp),
		TruePattern = ?Syn:construct({pattern, [TrueAtom], [TrueBranchBody]}),

		TailBodyVar2 = ?Syn:construct({var, TailName}),
		FalseAtom = ?Syn:construct({atom, false}),
		FalseBranchBody = create_fun_call({FunName}, [TailBodyVar2]),
		FalsePattern = ?Syn:construct({pattern, [FalseAtom], [FalseBranchBody]}),

		Body = ?Syn:construct({'case', ImplicitFunApp, [TruePattern, FalsePattern]}),

		MainClause = create_clause_from_name(FunName, [ListHeadPattern], [], [Body]),

		Func = ?Syn:construct({func, [DefaultClause, MainClause]}),

		?File:add_form(File, Func),
		FunName
	end,
	% Replace map call with recursive call
	fun(FunName) ->
		NewList = copy(lists:nth(2, Args)),
		replace_expr_with_call(Expr, FunName, [NewList])
	end];

% X = 1,
% lists:filter(fun(X) when X =:= 42 -> Y = X + 2, Y =:= 3;
%			   (Elem) -> Elem =:= X end,
%			List) =>
% filter_recursion1(List)
% 
% filter_recursion1([], _) -> [];
% filter_recursion1([X | Tail1], X1) -> Y = X + 2, 
%										case Y =:= 3 of 
%											true -> [X | filter_recursion1(Tail1, X1)];
%											false -> filter_recursion1(Tail1, X1)
%										end;						
% filter_recursion1([Elem | Tail1], X) -> case Elem =:= X of
%											true -> [Elem | filter_recursion1(Tail1, X)];
%											false -> filter_recursion1(Tail1, X)
%										  end.
transform_hof_call({lists, filter, 2}, recursive, fun_expr, Expr, Args, File) ->
	transform_recursion_fun_expr(Expr, Args, File, fun filter_transform_pattern/2, fun filter_transform_last_body/5, "filter_recursion").

% lists:map(fun(_) -> 1 end, List) =>
% [1 || _ <- List]
map_list_comp_from_fun_expr(single_body_joker, Expr, PatternsGuardsBodies, List) ->
	map_list_comp_from_fun_expr(single_body_var, Expr, PatternsGuardsBodies, List);

% lists:map(fun(Elem) -> Elem + 1 end, List) =>
% [Elem + 1 || Elem <- List]
map_list_comp_from_fun_expr(single_body_var, Expr, [{[Pattern], [], [Body]}], List) ->
	[fun() -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		NewBody = copy(Body),

		ElementPattern = copy(Pattern),
		NewList = copy(List),

		ListComp = create_list_comp(NewBody, [{ElementPattern, NewList}]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:map(fun(_) -> X = 1, X + 1 end, List) =>
% [begin X = 1, X + 1 end || _ <- List]
map_list_comp_from_fun_expr(multiple_bodies_joker, Expr, PatternsGuardsBodies, List) ->
	map_list_comp_from_fun_expr(multiple_bodies_var, Expr, PatternsGuardsBodies, List);

% lists:map(fun(Elem) -> X = Elem + 1, X + 1 end, List) =>
% [begin X = Elem + 1, X + 1 end || Elem <- List]
map_list_comp_from_fun_expr(multiple_bodies_var, Expr, [{[Pattern], [], Bodies}], List) ->
	[fun() ->
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		NewBodies = copy(Bodies),
		BlockExpr = ?Syn:construct({block_expr, NewBodies}),

		ElementPattern = copy(Pattern),
		NewList = copy(List),

		ListComp = create_list_comp(BlockExpr, [{ElementPattern, NewList}]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:map(fun(X) when X =:= 1 -> 1; (_) -> 2 end, List)
% [case Elem1 of X when X =:= 1 -> 1; _ -> 2 end || Elem1 <- List]
map_list_comp_from_fun_expr(default, Expr, PatternsGuardsBodies, List) ->
	[fun() ->
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),

		CaseElementVar = ?Syn:construct({var, ElementVarName}), 
		CaseExpr = create_case_expr_from_pgb(PatternsGuardsBodies, CaseElementVar),

		ElementPattern = ?Syn:construct({var, ElementVarName}),
		NewList = copy(List),

		ListComp = create_list_comp(CaseExpr, [{ElementPattern, NewList}]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end].

% lists:filter(fun(_) -> true end, List) =>
% [Elem1 || Elem1 <- List, true]
filter_list_comp_from_fun_expr(single_body_joker, Expr, [{[_Pattern], [], [Body]}], List) ->
	[fun() -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),
		HeadVar = ?Syn:construct({var, ElementVarName}), 

		ElementPattern = ?Syn:construct({var, ElementVarName}), 
		NewList = copy(List),

		NewBody = copy(Body),

		ListComp = create_list_comp(HeadVar, [{ElementPattern, NewList}], [NewBody]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:filter(fun(Elem) -> Elem rem 2 =:= 0 end, List) =>
% [Elem || Elem <- List, Elem rem 2 =:= 0]
filter_list_comp_from_fun_expr(single_body_var, Expr, [{[Pattern], [], [Body]}], List) ->
	[fun() -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		HeadVar = copy(Pattern),

		ElementPattern = copy(Pattern),
		NewList = copy(List),

		NewBody = copy(Body),

		ListComp = create_list_comp(HeadVar, [{ElementPattern, NewList}], [NewBody]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:filter(fun(_) -> X = 42, X rem 2 =:= 0 end, List) =>
% [Elem1 || Elem1 <- List, begin X = 42, X rem 2 =:= 0 end]
filter_list_comp_from_fun_expr(multiple_bodies_joker, Expr, [{[_Pattern], [], Bodies}], List) ->
	[fun() -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),
		HeadVar = ?Syn:construct({var, ElementVarName}), 

		ElementPattern = ?Syn:construct({var, ElementVarName}), 
		NewList = copy(List),

		NewBodies = copy(Bodies),
		BlockExpr = ?Syn:construct({block_expr, NewBodies}),

		ListComp = create_list_comp(HeadVar, [{ElementPattern, NewList}], [BlockExpr]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:filter(fun(Elem) -> X = Elem + 1, X rem 2 =:= 0 end, List) =>
% [Elem || Elem <- List, begin X = Elem + 1, X rem 2 =:= 0 end]
filter_list_comp_from_fun_expr(multiple_bodies_var, Expr, [{[Pattern], [], Bodies}], List) ->
	[fun() -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		HeadVar = copy(Pattern),

		ElementPattern = copy(Pattern),
		NewList = copy(List),

		NewBodies = copy(Bodies),
		BlockExpr = ?Syn:construct({block_expr, NewBodies}),

		ListComp = create_list_comp(HeadVar, [{ElementPattern, NewList}], [BlockExpr]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end];

% lists:filter(fun(X) when X =:= 1 -> true; (_) -> false end, List)
% [Elem1 || Elem1 <- List, case Elem1 of X when X =:= 1 -> true; _ -> false end]
filter_list_comp_from_fun_expr(default, Expr, PatternsGuardsBodies, List) ->
	[fun() ->
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		ElementVarName = new_varname_with_prefix(Expr, "Elem"),
		HeadVar = ?Syn:construct({var, ElementVarName}),

		ElementPattern = ?Syn:construct({var, ElementVarName}),
		NewList = copy(List),

		CaseElementVar = ?Syn:construct({var, ElementVarName}), 
		CaseExpr = create_case_expr_from_pgb(PatternsGuardsBodies, CaseElementVar),

		ListComp = create_list_comp(HeadVar, [{ElementPattern, NewList}], [CaseExpr]),

		?Syn:replace(Parent, {node, Expr}, [ListComp])
	end].

transform_recursion_fun_expr(Expr, Args, File, TransformPatternFun, TransformLastBodyFun, Prefix) ->
	FunExpr = lists:nth(1, Args),

	Clauses = ?Query:exec(FunExpr, ?Expr:clauses()),

	% Check clause arities
	lists:map(fun(Clause) -> ?Query:exec1(Clause, ?Clause:patterns(), ?LocalError(pattern_num, 1)) end, Clauses),

	OutsideVarrefs = outside_varrefs(FunExpr),
	OutsideVarNames = lists:sort(lists:map(fun ?Var:name/1, OutsideVarrefs)),
	[fun() ->
		?Transform:touch(File),
		FunName = new_funname_with_prefix(File, Prefix),

		DefaultClause = create_default_clause(FunName, length(OutsideVarrefs) + 1, 1),

		NewClauses = lists:map(fun(Clause) -> 
								transform_clause(Clause, FunName, OutsideVarNames, TransformPatternFun, TransformLastBodyFun) 
							   end, 
							   Clauses),

		Func = ?Syn:construct({func, [DefaultClause | NewClauses]}),

		?File:add_form(File, Func),
		FunName
	end,
	% Replace map call with recursive call
	fun(FunName) ->
		NewList = copy(lists:nth(2, Args)),
		NewOutsideVars = [?Syn:construct({var, OutsideVar}) || OutsideVar <- OutsideVarNames],
		replace_expr_with_call(Expr, FunName, [NewList | NewOutsideVars])
	end].

transform_clause(Clause, FunName, OutsideVarNames, TransformPatternFun, TransformLastBodyFun) ->
	Pattern = ?Query:exec1(Clause, ?Clause:patterns(), ?LocalError(pattern_num, 1)),
	Guards = ?Query:exec(Clause, ?Clause:guard()),
	Bodies = ?Query:exec(Clause, ?Clause:body()),

	PatternVars = ?Query:exec(Pattern, ?Expr:varbinds()),
	PatternVarNames = lists:map(fun ?Var:name/1, PatternVars),
	ShadowAdjustedNames = get_shadow_adjusted_name(hd(Bodies), OutsideVarNames, PatternVarNames),

	% Construct list pattern
	HeadVar = TransformPatternFun(Pattern, ShadowAdjustedNames),

	TailName = new_varname_with_prefix(hd(Bodies), "Tail"),
	TailVar = ?Syn:construct({var, TailName}),

	ListHeadPattern = create_list_head_pattern(HeadVar, TailVar),

	% Construct pattern for all outside vars
	OutsideVarPatterns = [?Syn:construct({var, VarName}) || VarName <- ShadowAdjustedNames],

	NewPatterns = [ListHeadPattern | OutsideVarPatterns],

	% Copy guards
	NewGuards = copy(Guards),

	% Copy all bodies, except last, it will be transformed.
	{FrontBodies, [LastBody]} = lists:split(length(Bodies) - 1, Bodies),
	NewFrontBodies = copy(FrontBodies),

	Body = TransformLastBodyFun(LastBody, FunName, HeadVar, TailName, ShadowAdjustedNames),

	NewBodies = NewFrontBodies ++ [Body],

	create_clause_from_name(FunName, NewPatterns, NewGuards, NewBodies).

map_transform_last_body(LastBody, FunName, _Pattern, TailName, ShadowAdjustedNames) ->
	NewLastBody = copy(LastBody),

	TailBodyVar = ?Syn:construct({var, TailName}),
	OutsideBodyVars = [?Syn:construct({var, VarName}) || VarName <- ShadowAdjustedNames],
	TailApp = create_fun_call({FunName}, [TailBodyVar | OutsideBodyVars]),

	create_list_head_pattern(NewLastBody, TailApp).

map_transform_pattern(Pattern, _ShadowAdjustedNames) ->
	copy(Pattern).

filter_transform_last_body(LastBody, FunName, Pattern, TailName, ShadowAdjustedNames) ->
	NewLastBody = copy(LastBody),

	TailBodyVar = ?Syn:construct({var, TailName}),
	OutsideBodyVars = [?Syn:construct({var, VarName}) || VarName <- ShadowAdjustedNames],
	RecArgs = [TailBodyVar | OutsideBodyVars],

	RecApp = create_fun_call({FunName}, RecArgs),

	TrueAtom = ?Syn:construct({atom, true}),
	NewPattern = case ?Expr:type(Pattern) of
					match_expr -> copy(?Query:exec(Pattern, ?Expr:child(1)));
					_ -> copy(Pattern)
				end,
	TrueBranchBody = create_list_head_pattern(NewPattern, RecApp),
	TruePattern = ?Syn:construct({pattern, [TrueAtom], [TrueBranchBody]}),

	FalseAtom = ?Syn:construct({atom, false}),
	NewRecArgs = copy(RecArgs),
	FalseBranchBody = create_fun_call({FunName}, NewRecArgs),
	FalsePattern = ?Syn:construct({pattern, [FalseAtom], [FalseBranchBody]}),

	?Syn:construct({'case', NewLastBody, [TruePattern, FalsePattern]}).

filter_transform_pattern(Pattern, ShadowAdjustedNames) ->
	HasJokers = has_jokers(Pattern),
	NewPattern = copy(Pattern),
	case HasJokers of
		true -> ElemName = new_varname_with_prefix(Pattern, "Elem", ShadowAdjustedNames),
				ElemVar = ?Syn:construct({var, ElemName}),
				case ?Expr:type(Pattern) of
					joker -> ElemVar;
					match_expr -> Matched = ?Query:exec1(Pattern, ?Expr:child(1), ?LocalErr0r(ambiguous)),
							MatchedHasJokers = has_jokers(Matched),
							case MatchedHasJokers of
								true ->	case ?Expr:type(Matched) of
											joker ->  
												Matching = ?Query:exec1(Pattern, ?Expr:child(2), ?LocalErr0r(ambiguous)),
												NewMatching = copy(Matching),
												?Syn:construct({match_expr, ElemVar, NewMatching});
											_ -> ?Syn:construct({match_expr, ElemVar, NewPattern})
										end;
								false -> NewPattern
							end;
					_ -> ?Syn:construct({match_expr, ElemVar, NewPattern})
				end;
		false -> NewPattern
	end.

get_patterns_gurads_bodies(FunExpr) ->
	FunExprClauses = ?Query:exec(FunExpr, ?Expr:clauses()),

	Patterns = [?Query:exec(FunExprClause, ?Clause:patterns()) || FunExprClause <- FunExprClauses],
	Guards = [?Query:exec(FunExprClause, ?Clause:guard()) || FunExprClause <- FunExprClauses],
	Bodies = [?Query:exec(FunExprClause, ?Clause:body()) || FunExprClause <- FunExprClauses],

	lists:zip3(Patterns, Guards, Bodies).

create_case_expr_from_pgb(PatternsGuardsBodies, Var) ->
	CaseBranches = [?Syn:construct({pattern, copy(Patterns), copy(Guards), copy(Bodies)}) || {Patterns, Guards, Bodies} <- PatternsGuardsBodies],
	?Syn:construct({'case', Var, CaseBranches}).


% Single clause, single pattern, no guards, single body
determine_clause_type([{[Pattern], [], [_Body]}]) ->
	case ?Expr:type(Pattern) of
		variable -> single_body_var;
		joker -> single_body_joker;
		_ -> default
	end;

% Single clause, single pattern, no guards, multiple bodies
determine_clause_type([{[Pattern], [], _Bodies}]) ->
	case ?Expr:type(Pattern) of
		variable -> multiple_bodies_var;
		joker -> multiple_bodies_joker;
		_ -> default
	end;

% All other cases
determine_clause_type(_) -> default.

create_list_comp(Head, GeneratorElems) ->
	create_list_comp(Head, GeneratorElems, []).

create_list_comp(Head, GeneratorElems, FilterElems) ->
	Generators = [?Syn:construct({list_gen, Pattern, List}) || {Pattern, List} <-  GeneratorElems],
	Filters = [?Syn:construct({filter, FilterElem}) || FilterElem <- FilterElems],
	?Syn:construct({list_comp, Head, Generators ++ Filters}).

create_list_head_pattern(HeadVar, TailVar) ->
	ListPattern = ?Syn:construct({list, [HeadVar]}),
	?Syn:construct({cons, [ListPattern, TailVar]}).

create_clause_from_name(FunName, Patterns, Guards, Bodies) ->
	FunNameAtom = ?Syn:construct({atom, FunName}),
	?Syn:construct({fun_clause, FunNameAtom, Patterns, Guards, Bodies}).

% Create fun_name(_, _, [], _) -> []; like clauses based on the specified values.
create_default_clause(FunName, ArgCount, ListArgIndex) ->
	
	Patterns = lists:map(fun(Idx) when Idx =:= ListArgIndex -> ?Syn:construct({cons, []});
							(_) -> ?Syn:construct({joker, []}) end, 
						 lists:seq(1,ArgCount)),
	Body = ?Syn:construct({cons, []}),
	create_clause_from_name(FunName, Patterns, [], [Body]).

create_fun_call({Mod, Fun}, Args) ->
	ModAtom = ?Syn:construct({atom, Mod}),
	FunAtom = ?Syn:construct({atom, Fun}),
	Infix = ?Syn:construct({{infix_expr, ':'}, ModAtom, FunAtom}),
	?Syn:construct({app, Infix, Args});

create_fun_call({Fun}, Args) ->
	FunAtom = ?Syn:construct({atom, Fun}),
	?Syn:construct({app, FunAtom, Args}).

replace_expr_with_call(Expr, FunName, Args) ->
	?Transform:touch(Expr),
	[{_, Parent}] = ?Syn:parent(Expr),

	Call = create_fun_call({FunName}, Args),

	?Syn:replace(Parent, {node, Expr}, [Call]).

get_shadow_adjusted_name(Expr, OutsideVars, PatternVars) ->
	[case lists:member(OutsideVar, PatternVars) of
		true -> new_varname_with_prefix(Expr, OutsideVar);
		false -> OutsideVar
	 end || OutsideVar <- OutsideVars].

check_implicit_fun_arity(ImplicitFun, Arity) ->
	FunArity = ?Expr:value(?Query:exec1(ImplicitFun, ?Expr:child(2), ?LocalErr0r(ambiguous))),
	?Check(FunArity =:= Arity, ?LocalError(implicit_arity, Arity)).

outside_varrefs(FunExpr) ->
	VarrefsUniq = make_unique_list(?Query:exec(FunExpr, ?Expr:varrefs())),

	Clauses = ?Query:exec(FunExpr, ?Expr:clauses()),
	VarDefs = make_unique_list(lists:flatten([?Query:exec(Clause, ?Clause:variables()) || Clause <- Clauses])),

	list_subtract(VarrefsUniq, VarDefs).

has_jokers(Expr) ->
	DeepSubs = ?Query:exec(Expr, ?Expr:deep_sub()),
	lists:any(fun(DeepSub) -> ?Expr:type(DeepSub) =:= joker end, DeepSubs).

copy([]) -> [];
copy([This | Tail]) -> [copy(This) | copy(Tail)];
copy(This) -> proplists:get_value(This, ?Syn:copy(This)).

new_varname_with_prefix(Expr, Prefix, AdditionalUsedNames) ->
	Vars      = visible_vars(Expr),
	UsedNames = [?Var:name(X) || X <- Vars, lists:prefix(Prefix, ?Var:name(X))] ++ AdditionalUsedNames,
	name_with_next_idx(Prefix, UsedNames, 1).

new_varname_with_prefix(Expr, Prefix) ->
	new_varname_with_prefix(Expr, Prefix, []).

name_with_next_idx(Prefix, UsedNames, Idx) ->
	Name = Prefix ++ integer_to_list(Idx),
	case lists:member(Name, UsedNames) of
		true  -> name_with_next_idx(Prefix, UsedNames, Idx + 1);
		false -> Name
	end.

visible_vars(Expr) ->
	Clause = ?Query:exec1(Expr, ?Expr:clause(), ?LocalErr0r(ambiguous)),
	?Query:exec(Clause, ?Clause:variables()).

get_all_fun_names(File) ->
	Module = ?Query:exec1(File, ?File:module(), ?LocalErr0r(ambiguous)),
	FunDefs = ?Query:exec(Module, ?Mod:locals()),
	ImpordetFuns = ?Query:exec(Module, ?Mod:imports()),
	[?Fun:name(Fun) || Fun <- FunDefs ++ ImpordetFuns].

new_funname_with_prefix(File, Prefix) ->
	FunNames = get_all_fun_names(File),
	FunNameStrs = lists:map(fun atom_to_list/1, FunNames),
	list_to_atom(name_with_next_idx(Prefix, FunNameStrs, 1)).

make_unique_list(List) ->
	sets:to_list(sets:from_list(List)).

list_subtract(List1, List2) ->
	sets:to_list(sets:subtract(sets:from_list(List1), sets:from_list(List2))).