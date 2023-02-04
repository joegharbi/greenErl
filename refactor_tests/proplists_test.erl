-module(proplists_test).

case_keyfind(List) ->
	case lists:keyfind(alma, 1, List) of
		false -> undefined;
		{_, X} -> X
	end.
