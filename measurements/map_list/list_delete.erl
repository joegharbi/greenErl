-module(list_delete).
-export([generate_input/1, recursive/3, keytake/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, generate_list(Size), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

% Call delete_recursive N times
recursive(_, _, 0) -> ok;
recursive(Key, List, N) -> 
	delete_recursive(Key, List),
	recursive(Key, List, N - 1).

% This does the actual deleting, the one above is for repeating
delete_recursive(_, []) -> [];
delete_recursive(Key, [{K,V}|T]) ->
	if 
		Key == K -> 
			T;
		true -> 
			[{K,V}|delete_recursive(Key, T)]
	end.

keytake(_, _, 0) -> ok;
keytake(Key, List, N) ->
	{_, _, NewList} = lists:keytake(Key, 1, List),
	NewList,
	keytake(Key, List, N - 1).
