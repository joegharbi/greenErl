-module(map_from).

-export([generate_input/1, binary/2]).

generate_input(ListSize) ->
	[term_to_binary(maps:from_list(generate_list(ListSize))),1000].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

binary(_, 0) -> ok;
binary(M, Times) -> 
	binary_to_term(M),
	binary(M, Times-1).