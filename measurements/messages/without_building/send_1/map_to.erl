-module(map_to).

-export([generate_input/1, binary/1]).

generate_input(ListSize) ->
	[maps:from_list(generate_list(ListSize))].

generate_list(0) -> [];
generate_list(N) -> [{N, N + 1} | generate_list(N - 1)].

binary(M) -> term_to_binary(M).