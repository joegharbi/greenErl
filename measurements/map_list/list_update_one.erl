-module(list_update_one).
-export([generate_input/1, recursive/4, keyreplace/4, keystore/4]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[generate_list(Size), Size div 2, Size, 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

% Call recursive_update N times
recursive(_, _, _, 0) -> ok;
recursive(List, Key, Value, N) ->
	recursive_update(List, Key, Value),
	recursive(List, Key, Value, N - 1).

% This does the actual updating, the one above is for repeating
recursive_update([], _, _) -> error;
recursive_update([{K, V} | T], Key, Value) -> 
	if
		K == Key -> [{Key, Value} | T];
		true -> [{K, V} | recursive_update(T, Key, Value)]
	end.

keyreplace(_, _, _, 0) -> ok;
keyreplace(List, Key, Value, N) -> 
	lists:keyreplace(Key, 1, List, {Key, Value}),
	keyreplace(List, Key, Value, N - 1).

keystore(_, _, _, 0) -> ok;
keystore(List, Key, Value, N) -> 
	lists:keystore(Key, 1, List, {Key, Value}),
	keystore(List, Key, Value, N - 1).