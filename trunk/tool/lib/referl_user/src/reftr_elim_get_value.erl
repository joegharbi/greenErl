-module(reftr_elim_get_value).
-export([prepare/1, error_text/2]).
-include("user.hrl").

error_text(bad_expr_range, Exprs) ->
    ["Select only a single expression, currently selected expression count: ", ?MISC:format("~p",[length(Exprs)])];
error_text(bad_application, Type) ->
    ["Selected expression must be application, not: ", ?MISC:format("~p",[Type])];
error_text(bad_function, [Module, Function, Arity]) ->
    ["Used function is not proplists:get_value/2 or proplists:get_value/3: ", ?MISC:format("~p:~p/~p",[Module, Function, Arity])];
error_text(ambiguous, _) -> 
	["A query is ambiguous, while it should only return a single value."].

prepare(Args) -> 
    ?d(Args),
    ExprSelected = ?Args:expression(Args),
    Expr = Expr = get_closest_parent_of_type(ExprSelected, application),

    Fun = ?Query:exec1(Expr, ?Expr:function(), ?LocalErr0r(ambiguous)),
    {_, {ModName, FunName, Arity}} = ?Fun:mod_fun_arity(Fun),
    ?Check(ModName =:= proplists andalso
           FunName =:= get_value,
           ?LocalError(bad_function, [ModName, FunName, Arity])),
    ?Check(Arity =:= 2 orelse
           Arity =:= 3,
           ?LocalError(bad_function, [ModName, FunName, Arity])),

    [_, ArgList] = ?Query:exec(Expr, ?Expr:children()),
    GetvalueArgs = ?Query:exec(ArgList, ?Expr:children()),

    NewVarName = ?Var:new_varname_with_prefix(Expr, "Var"),

    [fun() -> 
        ?Transform:touch(Expr),
        [{_, Parent}] = ?Syn:parent(Expr),

        [KeyVar, ListVar | ArgsLeft ] = copy(GetvalueArgs),
        IntegerOne = ?Syn:construct({integer, 1}),
        KeyfindArgs = [KeyVar, IntegerOne, ListVar],

        ListsAtom = ?Syn:construct({atom, lists}),
        KeyFindAtom = ?Syn:construct({atom, keyfind}),
        NewInfixExpr = ?Syn:construct({{infix_expr, ':'}, ListsAtom, KeyFindAtom}),

        KeyFindApp = ?Syn:construct({app, NewInfixExpr, KeyfindArgs}),
        
        DefaultValue = 
            case Arity of
                    2 -> 
                        ?Syn:construct({atom, undefined});
                    3 ->
                        [Def] = ArgsLeft,
                        Def
            end,
        FalseAtom = ?Syn:construct({atom, false}),
        FalseBranch = ?Syn:construct({pattern, [FalseAtom], [DefaultValue]}),

        Joker = ?Syn:construct({joker, []}),
        NewVar = ?Syn:construct({var, NewVarName}),
        TuplePattern = ?Syn:construct({tuple, [Joker, NewVar]}),
        VarBody = ?Syn:construct({var, NewVarName}),
        TupleBranch = ?Syn:construct({pattern, [TuplePattern], [VarBody]}),

        CaseExpr = ?Syn:construct({'case', KeyFindApp, [FalseBranch, TupleBranch]}),
        ?Syn:replace(Parent, {node, Expr}, [CaseExpr])
    end].

get_closest_parent_of_type(Node, Type) ->
    case ?Expr:type(Node) of
        Type -> Node;
        OtherType -> Parent = ?Query:exec1(Node, ?Expr:parent(), ?LocalError(bad_application, OtherType)),
                     get_closest_parent_of_type(Parent, Type)
    end.

copy([]) -> [];
copy([This | Tail]) -> [copy(This) | copy(Tail)];
copy(This) -> proplists:get_value(This, ?Syn:copy(This)).