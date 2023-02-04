-module(dict_from).

-export([generate_input/1, binary/1]).

generate_input(ListSize) ->
	[term_to_binary(dict:from_list(generate_list(ListSize)))].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

binary(M) -> binary_to_term(M).