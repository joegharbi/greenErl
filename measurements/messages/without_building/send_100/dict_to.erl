-module(dict_to).

-export([generate_input/1, binary/2]).

generate_input(ListSize) ->
	[dict:from_list(generate_list(ListSize)), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

binary(_, 0) -> ok;
binary(M, Times) -> 
	term_to_binary(M),
	binary(M, Times-1).