% Preconditions assumed and not checked:
% 1.) The selected Variable must be a list of binary tuples
% 2.) The keys of the tuples must be unique
% 3.) The order of the elements in the list does not matter
% 4.) All updates, deletes, insertions must preserve the uniqueness of the keys.
% 5.) The first element of the tuples is the key, the second element is the value
%     (this is checked for keyfind, keytake etc. calls)

% Preconditions  checked
% 1.) The selected list must be in a recursive function call
% 2.) All statements in the body of the recursive function,
%	  that contain the selected variable must be refactorable to maps
%	  Currently these include:
%	   - lists:keyfind/3
%	   - lists:keytake/3
%	   - lists:keystore/4
%	   - lists:keymember/3
%      - lists:keydelete/3
%	   - the list variable as a standalone statement

-module(reftr_transform_list).
-export([prepare/1, error_text/2]).
-include("user.hrl").

-define(SimpleTypes, [variable, integer, atom]).

error_text(non_recursive, [Mod, Fun, Arity]) -> 
	["The following function is not a recursive function: ", 
		?MISC:format("~p:~p/~p",[Mod, Fun, Arity])];
error_text(non_parameter, _) ->
	["The selected variable is not a parameter of the function containing it."];
error_text(parameter_type, [Type]) -> 
	[?MISC:format("The selected variable must be a parameter of the function, no type '~p' allowed.",[Type])]; 
error_text(bindings, _) -> 
	["Too many bindings for selected variable."];
error_text(ambiguous, _) -> 
	["A query is ambiguous, while it should only return a single value."];
error_text(bad_parent, [ParType, Allowed]) ->
	[?MISC:format("The selected variable is only allowed to appear in the followig expressions: ~p, not: ~p", [Allowed, ParType])];
error_text(bad_fun_call, [{Mod, Fun, Arity}, Allowed]) ->
	[?MISC:format("Not allowed to use selected variable in ~p:~p/~p call. ~nAllowed calls: ~p", [Mod, Fun, Arity, Allowed])];
error_text(bad_n_arg, [Mod, Fun, Arity]) ->
	[?MISC:format("The argument specifying the key part of a tuple of ~p:~p/~p is only allowed to be the literal '1'", [Mod, Fun, Arity])];
error_text(bad_list_arg, [Mod, Fun, Arity]) ->
	[?MISC:format("The third argument of ~p:~p/~p is only allowed to be the selected variable, no transformation or use of other variables allowed.", [Mod, Fun, Arity])];
error_text(unsupported_match, [MatchedType]) ->
	[?MISC:format("Cannot match result of keytake to this kind of expression of type '~p'.", [MatchedType])];
error_text(cons_error, _) ->
	["Only empty list is allowed for pattern matching in parameter list."];
error_text(invalid_pattern, [Type]) ->
	["Only patterns of type 'variable' and 'cons' are allowed, no ", ?MISC:format("'~p'", [Type])].

prepare(Args) ->
	?d(Args),
	% Get the selected variable
	Variable = ?Args:variable(Args),
	% Find where the variable is bound (currently only supporting a single binding)
	Binding = ?Query:exec1(Variable, ?Var:bindings(), ?LocalErr0r(bindings)),
	% Get the node of the parameter that binds the list variable
	Parameter = ?Query:exec1(Binding, [{flow, back}], ?LocalErr0r(non_parameter)),
	ParameterType = ?Expr:type(Parameter),
	?Check(ParameterType =:= fpar, ?LocalError(parameter_type, [ParameterType])),

	% Get the clause where the function is defined
	Clause = ?Query:exec1(Variable, ?Var:clause(), ?LocalErr0r(ambiguous)),
	% Get the form of the clause
	Form = ?Query:exec1(Clause, ?Clause:form(), ?LocalErr0r(ambiguous)),
	% Get the function node from the form
	Func = ?Query:exec1(Form, ?Form:func(), ?LocalErr0r(ambiguous)),

	% Get all functions that get called by our function
	CallsTo = ?Query:exec(Func, ?Fun:funcalls()),
	% Module name, function name and arity of our function
	{_, {Mod, Fun, Arity}} = ?Fun:mod_fun_arity(Func),
	% Check if the selected function is actually recursive
	?Check(lists:member(Func, CallsTo), ?LocalError(non_recursive, [Mod, Fun, Arity])),

	Patterns = ?Query:exec(Parameter, [flow]),
	TransformPatterns = lists:flatten(lists:map(fun transform_pattern/1, Patterns)),
	Vars = ?Query:exec(Parameter, ?Query:seq([flow], [varbind])),	

	TransformVarrefs = transform_variable(Vars, Func),

	% Get all the expressions that are calls to the recursive function
	ExprsCallingRec = ?Query:exec(Func, ?Fun:applications()),
	% Filter out the recursive calls that are inside the function
	OutsideExprsCallingRec = 
		lists:filter(
			fun(Expr) ->
				Func =/= ?Query:exec1(
							Expr, 
							?Query:seq(
								[?Expr:clause(), ?Clause:form(), ?Form:func()]), 
							?LocalErr0r(ambiguous)) 
			end,
		ExprsCallingRec),
	% Create the fun expressions that transform all calls to the selected function
	TransformOutsideCalls = 
		lists:flatten(
			lists:map(
				fun(Expr) -> 
					transform_oustide_call(Parameter, Expr, Fun) 
				end, 
			OutsideExprsCallingRec)),

	% The first fun does nothing, it is needed so we don't have to worry
	% about the first generated fun not taking any parameters
	[fun() -> 0 end] ++ TransformPatterns ++ TransformVarrefs ++ TransformOutsideCalls.


% Create the fun to transform a single call to the selected function.
transform_oustide_call(Parameter, Expr, FunName) ->
	% Get the node for the ArgList of the function call
	ArgList = ?Query:exec1(Expr, ?Expr:child(2), ?LocalErr0r(ambiguous)),
	% Get all the arguments
	Args = ?Query:exec(ArgList, ?Expr:children()),
	% Query and filter for arguments that are at the same position as the
	% parameter requires.
	CorrectArgs = lists:filter(
		fun(Arg) -> ?Query:exec(Arg, [call]) =:= [Parameter] end, Args),
	% There should only be one such Arg, so check that
	?Check(length(CorrectArgs) =:= 1, ?LocalErr0r(ambiguous)),
	[CorrectArg] = CorrectArgs,
	
	[fun(_) -> 
		?Transform:touch(Expr),
		[{_, Parent}] = ?Syn:parent(Expr),

		% Copy all the arguments
		CopyArgs = copy(Args),

		% Change the argument that was selected as CorrectArg
	    % to maps:from_list(CorrectArg)
		NewArgs = 
			lists:map(
				fun({CArg, Arg}) -> 
					case Arg =:= CorrectArg of 
						true -> create_fun_call({maps, from_list}, [CArg]); 
						false -> CArg 
					end 
				end, 
			lists:zip(CopyArgs, Args)),
		
		% Create the new function call with the modified arguments
		FunNameAtom = ?Syn:construct({atom, FunName}),
		NewApp = ?Syn:construct({app, FunNameAtom, NewArgs}),

		?Syn:replace(Parent, {node, Expr}, [NewApp])
	end].

%  Currently only support variables and empty list literals `[]`
transform_pattern(Pattern) ->
	case ?Expr:type(Pattern) of
		variable -> [];
		cons -> ?Query:exec1(Pattern, ?Expr:top_sub(), ?LocalErr0r(cons_error)),
				transform_empty_list_cons(Pattern);
		Type -> throw(?LocalError(invalid_pattern, [Type]))	
	end.

transform_empty_list_cons(Pattern) ->
	FunClause = ?Query:exec1(Pattern, ?Expr:clause(), ?LocalErr0r(ambiguous)),
	[fun(_) -> 
		?Transform:touch(Pattern),
		[{_, Parent}] = ?Syn:parent(Pattern),

		MapVarName = new_varname_with_prefix(Pattern, "Map"),
		NewVar = ?Syn:construct({var, MapVarName}),

		?Syn:replace(Parent, {node, Pattern}, [NewVar]),
		MapVarName
	end,
	fun(MapVarName) ->
		?Transform:touch(FunClause),
		[{_, Parent}] = ?Syn:parent(FunClause),

		MapVar = ?Syn:construct({var, MapVarName}),
		MapSizeFunCall = create_fun_call({map_size}, [MapVar]),
		IntegerZero = ?Syn:construct({integer, 0}),
		IsZeroSizedMap = ?Syn:construct({{infix_expr, '=:='}, MapSizeFunCall, IntegerZero}),

		Guards = ?Query:exec(FunClause, ?Clause:guard()),
		NewGuard = case Guards of
			[] -> IsZeroSizedMap;
			[Guard] -> GuardCopy = copy(Guard),
				?Syn:construct({{infix_expr, ','}, IsZeroSizedMap, GuardCopy})
		end,

		Name = ?Query:exec1(FunClause, ?Clause:name(), ?LocalErr0r(ambiguous)),
		NameCopy = copy(Name),

		Bodies = ?Query:exec(FunClause, ?Clause:body()),
		BodiesCopy = copy(Bodies),

		Patterns = ?Query:exec(FunClause, ?Clause:patterns()),
		PatternsCopy = copy(Patterns),	

		NewFunClause = ?Syn:construct({fun_clause, NameCopy, PatternsCopy, [NewGuard], BodiesCopy}),
		?Syn:replace(Parent, {node, FunClause}, [NewFunClause])
	end].

transform_variable(Vars, Func) ->
	Varrefs = ?Query:exec(Vars, ?Var:references()),

	lists:flatten(
		lists:map(
			fun(Varref) -> 
				transform_varref(Varref, Func) 
			end, 
			Varrefs)
		).


transform_varref(Varref, OriginalFun) ->
	Parent = ?Query:exec1(Varref, ?Query:any([{esub, back}], [{visib, back}]), ?LocalErr0r(ambiguous)),
	case ?Clause:is_clause(Parent) of
		true -> transform_standalone_list(Varref, OriginalFun);
		false -> transform_varref_par(Varref, Parent, OriginalFun)
	end.

% Only transform standalone lists if it is the return value
transform_standalone_list(Varref, OriginalFun) ->
	IsReturn = is_return_expr(Varref, OriginalFun),
	case IsReturn of
		true -> transform_return_list(Varref);
		false -> []
	end.
transform_return_list(Varref) ->
	[fun(_) ->
		?Transform:touch(Varref),
		[{_, Parent}] = ?Syn:parent(Varref),

		NewList = copy(Varref),

		MapsToList = create_fun_call({maps, to_list}, [NewList]),

		?Syn:replace(Parent, {node, Varref}, [MapsToList])
	end].

transform_varref_par(Varref, Parent, OriginalFun) ->
	ParType = ?Expr:type(Parent),
	Allowed = [arglist],
	?Check(lists:member(ParType, Allowed), 
		?LocalError(bad_parent, [ParType, Allowed])),
	case ParType of
		arglist -> transform_varref_arglist(Varref, Parent, OriginalFun)
	end.

transform_varref_arglist(Varref, ArgList, OriginalFun) ->
	FunApp = ?Query:exec1(ArgList, ?Expr:parent(), ?LocalErr0r(ambiguous)),
	FunNode = ?Query:exec1(FunApp, ?Expr:function(), ?LocalErr0r(ambiguous)),
	case FunNode =:= OriginalFun of
		% Do not transform recursive calls
		true -> []; 
		false -> 
			Allowed = [
				{lists, keyfind, 3}, 
				{lists, keytake, 3}, 
				{lists, keystore, 4},
				{lists, keymember, 3},
				{lists, keydelete, 3}],
			{_, {Mod, Fun, Arity}} = ?Fun:mod_fun_arity(FunNode),
			?Check(lists:member({Mod, Fun, Arity}, Allowed), 
				?LocalError(bad_fun_call, [{Mod, Fun, Arity}, Allowed])),
			Args = ?Query:exec(ArgList, ?Expr:children()),
			transform_varref_function_call({Mod, Fun, Arity}, Varref, Args, FunApp, OriginalFun)
	end.

transform_varref_function_call(MFA={lists, keyfind, 3}, Varref, Args, FunApp, OriginalFun) ->
	validate_n_arg(lists:nth(2, Args), MFA),
	
	validate_list_arg(lists:nth(3, Args), Varref, MFA),

	Branch = determine_keyfind_branch(FunApp, Args, OriginalFun),
	keyfind_transform(Branch, FunApp, Args);

transform_varref_function_call(MFA={lists, keytake, 3}, Varref, Args, FunApp, OriginalFun) ->
	validate_n_arg(lists:nth(2, Args), MFA),
	
	validate_list_arg(lists:nth(3, Args), Varref, MFA),

	NewlyBoundTransforms = transform_newly_bound_list(keytake, FunApp, OriginalFun),

	KeyArg = lists:nth(1, Args),
	KeyType = keytype(KeyArg),

	IsReturn = is_expr_or_parent_return_expr(FunApp, OriginalFun),

	keytake_transform(KeyType, IsReturn, FunApp, Args) ++ NewlyBoundTransforms;

transform_varref_function_call(MFA={lists, keystore, 4}, Varref, Args, FunApp, OriginalFun) ->
	validate_n_arg(lists:nth(2, Args), MFA),
	
	validate_list_arg(lists:nth(3, Args), Varref, MFA),

	NewlyBoundTransforms = transform_newly_bound_list(keystore, FunApp, OriginalFun),

	FirstKey = lists:nth(1, Args),
	Tuple = lists:nth(4, Args),

	Branch = determine_keystore_branch(FirstKey, Tuple),

	IsReturn = is_expr_or_parent_return_expr(FunApp, OriginalFun),

	keystore_transform(Branch, IsReturn, FunApp, Args) ++ NewlyBoundTransforms;

transform_varref_function_call(MFA={lists, keymember, 3}, Varref, Args, FunApp, _OriginalFun) ->
	validate_n_arg(lists:nth(2, Args), MFA),

	validate_list_arg(lists:nth(3, Args), Varref, MFA),

	keymember_transform(FunApp, Args);

transform_varref_function_call(MFA={lists, keydelete, 3}, Varref, Args, FunApp, OriginalFun) ->
	validate_n_arg(lists:nth(2, Args), MFA),

	validate_list_arg(lists:nth(3, Args), Varref, MFA),

	NewlyBoundTransforms = transform_newly_bound_list(keydelete, FunApp, OriginalFun),
	
	IsReturn = is_expr_or_parent_return_expr(FunApp, OriginalFun),

	keydelete_transform(IsReturn, FunApp, Args) ++ NewlyBoundTransforms.

validate_n_arg(NArg, {Mod, Fun, Arity}) ->
	?Check(?Expr:type(NArg) =:= integer andalso ?Expr:value(NArg) =:= 1,
		?LocalError(bad_n_arg, [Mod, Fun, Arity])).

validate_list_arg(ListArg, Varref, {Mod, Fun, Arity}) ->
	?Check(ListArg =:= Varref, ?LocalError(bad_list_arg, [Mod, Fun, Arity])).

transform_newly_bound_list(Branch, FunApp, OriginalFun) ->
	Parents = ?Query:exec(FunApp, ?Expr:parent()),
	case Parents of
		[Parent] -> 
					case is_match_expr(Parent) of
						true -> spec_transform_newly_bound_list(Branch, Parent, OriginalFun);
						false -> []
					end; 
		_ -> []
	end.

spec_transform_newly_bound_list(keytake, Parent, OriginalFun) ->
	LHS = ?Query:exec1(Parent, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	case ?Expr:type(LHS) of
		tuple -> 	NewListPart = ?Query:exec1(LHS, ?Expr:child(3), ?LocalError(unsupported_match, [tuple])),
					NewListType = ?Expr:type(NewListPart),
					?Check(NewListType =:= variable, ?LocalError(unsupported_match, [NewListType])),
					transform_new_list_part(NewListPart, OriginalFun);
		% Do not allow matching to a variable, since we can't controll how that variable will
		% be used, so we can't transform all uses of that variable to be with a map instead of list.
		% Only allow this, if it is a return statement.
		variable -> ?Check(is_return_expr(Parent, OriginalFun), ?LocalError(unsupported_match, [variable])),
					[];
		% Other kinds of matches are allowed, though would result in runtime error even in the
		% original program.
		_ -> []
	end;

spec_transform_newly_bound_list(Branch, Parent, OriginalFun)
	when Branch =:= keystore orelse 
		 Branch =:= keydelete ->
	NewListPart = ?Query:exec1(Parent, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	Type = ?Expr:type(NewListPart),
	case Type of
		% In this case only allow binding to a variable
		variable -> transform_new_list_part(NewListPart, OriginalFun);
		_ -> throw(?LocalError(unsupported_match, [Type]))
	end.

transform_new_list_part(NewListPart, OriginalFun) ->
	VarBinds = ?Query:exec(NewListPart, ?Expr:varbinds()),
	case VarBinds of
		[] -> [];
		[VarBind] -> transform_variable([VarBind], OriginalFun);
		_ -> throw(?LocalError(bindings, []))
	end.

determine_keyfind_branch(FunApp, Args, OriginalFun) ->
	KeyArg = lists:nth(1, Args),

	Parents = ?Query:exec(FunApp, ?Expr:parent()),
	{MatchType, IsReturn} = case Parents of
		[Parent] -> IsRet = is_return_expr(Parent, OriginalFun),
					case is_match_expr(Parent) of
						true ->  	LHS = ?Query:exec1(Parent, ?Expr:child(1), ?LocalErr0r(ambiguous)),
									case ?Expr:type(LHS) of
										tuple -> {classify_keyfind_tuple(LHS, KeyArg), IsRet};
										_ -> {default, IsRet}
									end;
						false -> 	{default, IsRet}
					end;
		_ -> 		{default, is_return_expr(FunApp, OriginalFun)}
	end,
	KeyType = keytype(KeyArg),
	case MatchType of
		default -> 	KeyType;
		_ when not IsReturn -> 	MatchType;
	    _ when IsReturn -> 
			case KeyType of
				simple_key -> simple_key_ret;
				complex_key -> complex_key_ret
			end
	end.

classify_keyfind_tuple(LHS, KeyArg) ->
	KeyPart = ?Query:exec1(LHS, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	KeyPartValue = ?Expr:value(KeyPart),
	KeyArgType = ?Expr:type(KeyArg),
	KeyArgValue = ?Expr:value(KeyArg),
	IsSimpleType = lists:member(KeyArgType, ?SimpleTypes),

	case ?Expr:type(KeyPart) of
		joker when IsSimpleType -> match_joker;
		KeyArgType 
			when KeyPartValue =:= KeyArgValue andalso IsSimpleType -> match_same_key;
		_ -> match_different_key
	end.

determine_keystore_branch(FirstKey, Tuple) ->
	TupleType = ?Expr:type(Tuple),
	case TupleType of
		tuple -> SecondKeys = ?Query:exec(Tuple, ?Expr:child(1)),
				 Values = ?Query:exec(Tuple, ?Expr:child(2)),
				 case length(SecondKeys) =:= 1 andalso length(Values) =:= 1 of
					true ->
						[SecondKey] = SecondKeys, 
						FirstType = ?Expr:type(FirstKey),
						SecondType = ?Expr:type(SecondKey),
						case FirstType =:= SecondType andalso 
							lists:member(FirstType, ?SimpleTypes) andalso
							?Expr:value(FirstKey) =:= ?Expr:value(SecondKey) of
							true -> same_keys;
							false -> different_keys
						end;
					false -> no_tuple_complex
				 end;
		Type -> case lists:member(Type, ?SimpleTypes) of 
					true -> no_tuple_simple;
					false -> no_tuple_complex
				end
	end.

keytype(KeyArg) ->
	KeyExprType = ?Expr:type(KeyArg),
	case lists:member(KeyExprType, ?SimpleTypes) of
		true -> simple_key;
		false -> complex_key
	end.

is_match_expr(Expr) ->
	MatchTypes = [match_expr],
	?Expr:is_expr(Expr) andalso lists:member(?Expr:type(Expr), MatchTypes).

is_return_expr(Expr, Func) ->
	Returns = ?Query:exec(Func, ?Fun:return_points(Func)),
	lists:member(Expr, Returns).

is_expr_or_parent_return_expr(Expr, Func) ->
	Parents = ?Query:exec(Expr, ?Expr:parent()),
	case Parents of
		[Parent] -> is_return_expr(Parent, Func);
		_ -> is_return_expr(Expr, Func)
	end.

% {_, V} = lists:keyfind(Key, 1, List) => #{Key := V} = List
keyfind_transform(match_joker, FunApp, Args) -> 
	Parent = ?Query:exec1(FunApp, ?Expr:parent(), ?LocalErr0r(ambiguous)),
	Tuple = ?Query:exec1(Parent, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	Result = ?Query:exec1(Tuple, ?Expr:child(2), ?LocalErr0r(ambiguous)),
	[fun(_) ->
		?Transform:touch(Parent),
		[{_, GrandParent}] = ?Syn:parent(Parent),

		% Copy key, list and result pattern
		NewKey = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),
		NewResult = copy(Result),

		% Construct exact_map_expr and match_expr
		ExactMapExpr = create_exact_map_expr(NewKey, NewResult),
		MatchExpr = ?Syn:construct({match_expr, ExactMapExpr, NewList}),

		?Syn:replace(GrandParent, {node, Parent}, [MatchExpr])
	end];

%  {Key, V} = lists:keyfind(Key, 1, List) => #{Key := V} = List
keyfind_transform(match_same_key, FunApp, Args) -> keyfind_transform(match_joker, FunApp, Args);

% {K, V} = lists:keyfind(Key, 1, List) => begin K = Key, #{K := V} = List end
keyfind_transform(match_different_key, FunApp, Args) ->
	Parent = ?Query:exec1(FunApp, ?Expr:parent(), ?LocalErr0r(ambiguous)),
	Tuple = ?Query:exec1(Parent, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	MatchKey = ?Query:exec1(Tuple, ?Expr:child(1), ?LocalErr0r(ambiguous)),
	Result = ?Query:exec1(Tuple, ?Expr:child(2), ?LocalErr0r(ambiguous)),
	IsJoker = ?Expr:type(MatchKey) =:= joker,
	[fun(_) ->
		?Transform:touch(Parent),
		[{_, GrandParent}] = ?Syn:parent(Parent),

		% Construct first match expression
		NewMatchKey = 
			case IsJoker of
				true -> NewKeyName = new_varname_with_prefix(Parent, "Key"),
						?Syn:construct({var, NewKeyName});
				false -> copy(MatchKey)
			end,
		NewKey = copy(lists:nth(1, Args)),
		MatchExpr = ?Syn:construct({match_expr, NewMatchKey, NewKey}),

		% Copy key, list and result pattern
		MapKey = copy(NewMatchKey),
		NewList = copy(lists:nth(3, Args)),
		NewResult = copy(Result),

		% Construct exact_map_expr and match_expr
		ExactMapExpr = create_exact_map_expr(MapKey, NewResult),
		MapMatchExpr = ?Syn:construct({match_expr, ExactMapExpr, NewList}),

		% Create begin end block
		BlockExpr = ?Syn:construct({block_expr, [MatchExpr, MapMatchExpr]}),

		?Syn:replace(GrandParent, {node, Parent}, [BlockExpr])
	end];

% lists:keyfind(Key, 1, List) =>
% case maps:find(Key, List) of
% 	error -> false;
% 	{ok, Value1} -> {Key, Value1}
% end
keyfind_transform(simple_key, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		CaseExpr = create_case_expr_keyfind(FunApp, Args),

		?Syn:replace(Parent, {node, FunApp}, [CaseExpr])
	end];

% lists:keyfind(Key, 1, List) =>
% begin
% Key1 = Key,
% case maps:find(Key1, List) of
% 	error -> false;
% 	{ok, Value1} -> {Key1, Value1}
% end
% end
keyfind_transform(complex_key, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		BlockExpr = create_block_expr_keyfind(FunApp, Args),

		?Syn:replace(Parent, {node, FunApp}, [BlockExpr])
	end];

% {K, V} = lists:keyfind(Key, 1, List) =>
% case maps:find(Key, List) of
% 	error -> false;
% 	{ok, Value1} -> {Key, Value1}
% end
keyfind_transform(simple_key_ret, FunApp, Args) ->
	Parent = ?Query:exec1(FunApp, ?Expr:parent(), ?LocalErr0r(ambiguous)),
	[fun(_) ->
		?Transform:touch(Parent),
		[{_, GrandParent}] = ?Syn:parent(Parent),

		CaseExpr = create_case_expr_keyfind(FunApp, Args),

		?Syn:replace(GrandParent, {node, Parent}, [CaseExpr])
	end];

% {NewKey, V} = lists:keyfind(Key, 1, List) =>
% begin
% Key1 = Key,
% case maps:find(Key1, List) of
% 	error -> false;
% 	{ok, Value1} -> {Key1, Value1}
% end
% end
keyfind_transform(complex_key_ret, FunApp, Args) ->
	Parent = ?Query:exec1(FunApp, ?Expr:parent(), ?LocalErr0r(ambiguous)),
	[fun(_) ->
		?Transform:touch(Parent),
		[{_, GrandParent}] = ?Syn:parent(Parent),

		BlockExpr = create_block_expr_keyfind(FunApp, Args),

		?Syn:replace(GrandParent, {node, Parent}, [BlockExpr])
	end].

create_block_expr_keyfind(FunApp, Args) ->
	% Extract Key to a new variable
	NewKeyName = new_varname_with_prefix(FunApp, "Key"),
	KeyVarIntro = ?Syn:construct({var, NewKeyName}),
	NewKey = copy(lists:nth(1, Args)),
	KeyMatch = ?Syn:construct({match_expr, KeyVarIntro, NewKey}),

	KeyVarFind = ?Syn:construct({var, NewKeyName}),
	KeyVarReturn = ?Syn:construct({var, NewKeyName}),

	% Construct case expression
	CaseExpr = create_case_expr_with_key_keyfind(FunApp, Args, KeyVarFind, KeyVarReturn),

	?Syn:construct({block_expr, [KeyMatch, CaseExpr]}).

create_case_expr_keyfind(FunApp, Args) ->
	NewKey = copy(lists:nth(1, Args)),
	NewKeyReturn = copy(lists:nth(1, Args)),
	create_case_expr_with_key_keyfind(FunApp, Args, NewKey, NewKeyReturn).

create_case_expr_with_key_keyfind(FunApp, Args, NewKey, NewKeyReturn) ->
	NewList = copy(lists:nth(3, Args)),

	MapsFind = create_fun_call({maps, find}, [NewKey, NewList]),

	% Construct atoms and pattern for error case
	ErrorAtom = ?Syn:construct({atom, error}),
	FalseAtom = ?Syn:construct({atom, false}),
	ErrorPattern = ?Syn:construct({pattern, [ErrorAtom], [FalseAtom]}),

	% Construct atoms and pattern for ok case
	OkAtom = ?Syn:construct({atom, ok}),
	NewVarName = new_varname_with_prefix(FunApp, "Value"),
	NewVarPattern = ?Syn:construct({var, NewVarName}),
	NewVarBody = ?Syn:construct({var, NewVarName}),

	TuplePattern = ?Syn:construct({tuple, [OkAtom, NewVarPattern]}),
	TupleBody = ?Syn:construct({tuple, [NewKeyReturn, NewVarBody]}),
	OkPattern = ?Syn:construct({pattern, [TuplePattern], [TupleBody]}),

	% Construct case expression
	?Syn:construct({'case', MapsFind, [ErrorPattern, OkPattern]}).

% if IsReturn =:= false
% lists:keytake(Key, 1, List) => 
% case maps:take(Key, List) of
% 	error -> false
% 	{Value1, List1} -> {value, {Key, Value1}, List1}
% end
%
% if IsReturn =:= true
% lists:keytake(Key, 1, List) => 
% case maps:take(Key, List) of
% 	error -> false
% 	{Value1, List1} -> {value, {Key, Value1}, maps:to_list(List1)}
% end
keytake_transform(simple_key, IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey = copy(lists:nth(1, Args)),
		NewKeyReturn = copy(lists:nth(1, Args)),

		CaseExpr = create_case_expr_with_key_keytake(FunApp, Args, NewKey, NewKeyReturn, IsReturn),

		?Syn:replace(Parent, {node, FunApp}, [CaseExpr])
	end];

% if IsReturn =:= false
% lists:keytake(Key, 1, List) => 
% begin
% 	Key1 = Key,
% 	case maps:take(Key1, List) of
% 		error -> false
% 		{Value1, List1} -> {value, {Key1, Value1}, List1}
% 	end
% end
%
% if IsReturn =:= true
% lists:keytake(Key, 1, List) => 
% begin
% 	Key1 = Key,
% 	case maps:take(Key1, List) of
% 		error -> false
% 		{Value1, List1} -> {value, {Key1, Value1}, maps:to_list(List1)}
% 	end
% end
keytake_transform(complex_key, IsReturn, FunApp, Args) ->
	[fun(_) -> 
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		% Extract Key to a new variable
		NewKeyName = new_varname_with_prefix(FunApp, "Key"),
		KeyVarIntro = ?Syn:construct({var, NewKeyName}),
		NewKey = copy(lists:nth(1, Args)),
		KeyMatch = ?Syn:construct({match_expr, KeyVarIntro, NewKey}),

		KeyVarTake = ?Syn:construct({var, NewKeyName}),
		KeyVarReturn = ?Syn:construct({var, NewKeyName}),

		% Construct case expression
		CaseExpr = create_case_expr_with_key_keytake(FunApp, Args, KeyVarTake, KeyVarReturn, IsReturn),

		BlockExpr = ?Syn:construct({block_expr, [KeyMatch, CaseExpr]}),

		?Syn:replace(Parent, {node, FunApp}, [BlockExpr])
	end].

create_case_expr_with_key_keytake(FunApp, Args, NewKey, NewKeyReturn, IsReturn) ->
	NewList = copy(lists:nth(3, Args)),

	MapsTake = create_fun_call({maps, take}, [NewKey, NewList]),

	% Construct atoms and pattern for error case
	ErrorAtom = ?Syn:construct({atom, error}),
	FalseAtom = ?Syn:construct({atom, false}),
	ErrorPattern = ?Syn:construct({pattern, [ErrorAtom], [FalseAtom]}),

	% Construct atoms and pattern for ok case
	ValueAtom = ?Syn:construct({atom, value}),
	ValueVarName = new_varname_with_prefix(FunApp, "Value"),
	ValueVarPattern = ?Syn:construct({var, ValueVarName}),
	ValueVarBody = ?Syn:construct({var, ValueVarName}),
	ListVarName = new_varname_with_prefix(FunApp, "List"),
	ListVarPattern = ?Syn:construct({var, ListVarName}),
	ListVarBody = ?Syn:construct({var, ListVarName}),

	ListExprBody = case IsReturn of
		true -> create_fun_call({maps, to_list}, [ListVarBody]);
		false -> ListVarBody
	end,

	KeyValueTuple = ?Syn:construct({tuple, [NewKeyReturn, ValueVarBody]}),

	TuplePattern = ?Syn:construct({tuple, [ValueVarPattern, ListVarPattern]}),
	TupleBody = ?Syn:construct({tuple, [ValueAtom, KeyValueTuple, ListExprBody]}),
	OkPattern = ?Syn:construct({pattern, [TuplePattern], [TupleBody]}),

	% Construct case expression
	?Syn:construct({'case', MapsTake, [ErrorPattern, OkPattern]}).

% if IsReturn =:= false
% lists:keystore(Key, 1, List, {Key, Value}) =>
% List#{Key => Value}
%
% if IsReturn =:= true
% lists:keystore(Key, 1, List, {Key, Value}) =>
% maps:to_list(List#{Key => Value})
keystore_transform(same_keys, IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),
		Tuple = lists:nth(4, Args),
		% This query will never throw an exception, since it has been checked before,
		% that the value exists and is unique.
		Value = ?Query:exec1(Tuple, ?Expr:child(2), ?LocalErr0r(ambiguous)),
		NewValue = copy(Value),
		MapUpdateExpr = create_map_update_expr(NewList, NewKey, NewValue),
		FinalMapUpdateExpr = case IsReturn of
			true -> create_fun_call({maps, to_list}, [MapUpdateExpr]);
			false -> MapUpdateExpr
		end,

		?Syn:replace(Parent, {node, FunApp}, [FinalMapUpdateExpr])
	end];

% if IsReturn =:= false
% lists:keystore(Key1, 1, List, {Key2, Value}) =>
% (maps:remove(Key1, List))#{Key2 => Value}
%
% if IsReturn =:= false
% lists:keystore(Key1, 1, List, {Key2, Value}) =>
% maps:to_list((maps:remove(Key1, List))#{Key2 => Value})
keystore_transform(different_keys, IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey1 = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),
		Tuple = lists:nth(4, Args),
		% These queries will never throw an exception, since it has been checked before,
		% that the value exists and is unique.
		Key2 = ?Query:exec1(Tuple, ?Expr:child(1), ?LocalErr0r(ambiguous)),
		Value = ?Query:exec1(Tuple, ?Expr:child(2), ?LocalErr0r(ambiguous)),
		NewKey2 = copy(Key2),
		NewValue = copy(Value),

		MapUpdateExpr = create_map_update_expr_with_remove(NewKey1, NewList, NewKey2, NewValue),
		FinalMapUpdateExpr = case IsReturn of
			true -> create_fun_call({maps, to_list}, [MapUpdateExpr]);
			false -> MapUpdateExpr
		end,

		?Syn:replace(Parent, {node, FunApp}, [FinalMapUpdateExpr])
	end];

% if IsReturn =:= false
% lists:keystore(Key, 1, List, Tuple) =>
% (maps:remove(Key, List))#{element(1, Tuple) => element(2, Tuple)}
%
% if IsReturn =:= true
% lists:keystore(Key, 1, List, Tuple) =>
% maps:to_list((maps:remove(Key, List))#{element(1, Tuple) => element(2, Tuple)})
keystore_transform(no_tuple_simple, IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey1 = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),
		Tuple = lists:nth(4, Args),

		NewTuple1 = copy(Tuple),
		NewTuple2 = copy(Tuple),

		Integer1 = ?Syn:construct({integer, 1}),
		Integer2 = ?Syn:construct({integer, 2}),
		NewKey2 = create_fun_call({element}, [Integer1, NewTuple1]),
		NewValue = create_fun_call({element}, [Integer2, NewTuple2]),

		MapUpdateExpr = create_map_update_expr_with_remove(NewKey1, NewList, NewKey2, NewValue),
		FinalMapUpdateExpr = case IsReturn of
			true -> create_fun_call({maps, to_list}, [MapUpdateExpr]);
			false -> MapUpdateExpr
		end,

		?Syn:replace(Parent, {node, FunApp}, [FinalMapUpdateExpr])
	end];

% if IsReturn =:= false
% lists:keystore(Key, 1, List, Tuple) =>
% begin
% 	Tuple1 = Tuple,
% 	(maps:remove(Key, List))#{element(1, Tuple1) => element(2, Tuple1)}
% end
%
% if IsReturn =:= true
% lists:keystore(Key, 1, List, Tuple) =>
% begin
% 	Tuple1 = Tuple,
% 	maps:to_list((maps:remove(Key, List))#{element(1, Tuple1) => element(2, Tuple1)})
% end
keystore_transform(no_tuple_complex, IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey1 = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),
		NewTuple = copy(lists:nth(4, Args)),

		TupleVarName = new_varname_with_prefix(FunApp, "Tuple"),
		Tuple1 = ?Syn:construct({var, TupleVarName}),
		Tuple2 = ?Syn:construct({var, TupleVarName}),
		Tuple3 = ?Syn:construct({var, TupleVarName}),

		Integer1 = ?Syn:construct({integer, 1}),
		Integer2 = ?Syn:construct({integer, 2}),
		NewKey2 = create_fun_call({element}, [Integer1, Tuple2]),
		NewValue = create_fun_call({element}, [Integer2, Tuple3]),

		MapUpdateExpr = create_map_update_expr_with_remove(NewKey1, NewList, NewKey2, NewValue),
		FinalMapUpdateExpr = case IsReturn of
			true -> create_fun_call({maps, to_list}, [MapUpdateExpr]);
			false -> MapUpdateExpr
		end,

		TupleMatch = ?Syn:construct({match_expr, Tuple1, NewTuple}),

		BlockExpr = ?Syn:construct({block_expr, [TupleMatch, FinalMapUpdateExpr]}),

		?Syn:replace(Parent, {node, FunApp}, [BlockExpr])
	end].

% lists:keymember(Key, 1, List) =>
% maps:is_key(Key, List)
keymember_transform(FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),

		IsKeyCall = create_fun_call({maps, is_key}, [NewKey, NewList]),

		?Syn:replace(Parent, {node, FunApp}, [IsKeyCall])
	end].

% if IsReturn =:= false
% lists:keydelete(Key, 1, List) =>
% maps:remove(Key, List)
%
% if IsReturn =:= true
% lists:keydelete(Key, 1, List) =>
% maps:to_list(maps:remove(Key, List))
keydelete_transform(IsReturn, FunApp, Args) ->
	[fun(_) ->
		?Transform:touch(FunApp),
		[{_, Parent}] = ?Syn:parent(FunApp),

		NewKey = copy(lists:nth(1, Args)),
		NewList = copy(lists:nth(3, Args)),

		RemoveCall = create_fun_call({maps, remove}, [NewKey, NewList]),

		FinalExpr = case IsReturn of
			true -> create_fun_call({maps, to_list}, [RemoveCall]);
			false -> RemoveCall
		end,

		?Syn:replace(Parent, {node, FunApp}, [FinalExpr])
	end].

create_map_update_expr_with_remove(Key1, List, Key2, Value) ->
	MapsRemoveExpr = create_fun_call({maps, remove}, [Key1, List]),
	MapsRemoveExprParen = ?Syn:construct({paren, MapsRemoveExpr}),
	create_map_update_expr(MapsRemoveExprParen, Key2, Value).

create_map_update_expr(List, Key, Value) ->
	InfixExpr = ?Syn:construct({{infix_expr, '=>'}, Key, Value}),
	?Syn:construct({{map_update}, List, [InfixExpr]}).

create_exact_map_expr(Key, Value) ->
	InfixExpr = ?Syn:construct({{infix_expr, ':='}, Key, Value}),
	?Syn:construct({{exact_map_expr}, [InfixExpr]}).

create_fun_call({Mod, Fun}, Args) ->
	ModAtom = ?Syn:construct({atom, Mod}),
	FunAtom = ?Syn:construct({atom, Fun}),
	Infix = ?Syn:construct({{infix_expr, ':'}, ModAtom, FunAtom}),
	?Syn:construct({app, Infix, Args});

create_fun_call({Fun}, Args) ->
	FunAtom = ?Syn:construct({atom, Fun}),
	?Syn:construct({app, FunAtom, Args}).

% Use ?Syn:copy/1 to copy lists, and discard of the old node values
copy([]) -> [];
copy([This | Tail]) -> [copy(This) | copy(Tail)];
copy(This) -> proplists:get_value(This, ?Syn:copy(This)).

new_varname_with_prefix(Expr, Prefix) ->
	Vars      = visible_vars(Expr),
	UsedNames = [?Var:name(X) || X <- Vars, lists:prefix(Prefix, ?Var:name(X))],
	varname_with_next_idx(Prefix, UsedNames, 1).

varname_with_next_idx(Prefix, UsedNames, Idx) ->
	VarName = Prefix ++ integer_to_list(Idx),
	case lists:member(VarName, UsedNames) of
		true  -> varname_with_next_idx(Prefix, UsedNames, Idx + 1);
		false -> VarName
	end.

visible_vars(Expr) ->
	Clause = ?Query:exec1(Expr, ?Expr:clause(), ?LocalErr0r(ambiguous)),
	?Query:exec(Clause, ?Clause:variables()).