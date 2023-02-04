-module(cases).
-compile([export_all]).

main() ->
	case 1 of
		Var1 -> ok
	end,
	case 2 of
		Var1 -> ok;
		Var2 -> error
	end.