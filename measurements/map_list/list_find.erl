-module(list_find).
-export([generate_input/1, recursive/3, keyfind/3, get_value/3]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[Size div 2, generate_list(Size), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

% Call find_key N times
recursive(_, _, 0) -> ok;
recursive(Key, List, N) -> 
	find_key_recursive(Key, List),
	recursive(Key, List, N - 1).

% This does the actual finding, the one above is for repeating
find_key_recursive(Key, [{K,V}|T]) ->
	if 
		Key == K -> 
			V;
		true -> 
			find_key_recursive(Key, T)
	end.

keyfind(_, _, 0) -> ok;
keyfind(Key, List, N) ->
	lists:keyfind(Key, 1, List),
	keyfind(Key, List, N - 1).

get_value(_, _, 0) -> ok;
get_value(Key, List, N) ->
	proplists:get_value(Key, List),
	get_value(Key, List, N - 1).