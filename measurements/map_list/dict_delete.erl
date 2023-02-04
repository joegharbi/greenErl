-module(dict_delete).
-export([generate_input/1, take/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, dict:from_list(generate_list(Size)), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

take(_, _, 0) -> ok;
take(Key, Dict, N) -> 
	{_, NewDict} = dict:take(Key, Dict),
	NewDict,
	take(Key, Dict, N - 1).
