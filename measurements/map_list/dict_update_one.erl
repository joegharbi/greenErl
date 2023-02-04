-module(dict_update_one).
-export([generate_input/1, update/4, store/4]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[dict:from_list(generate_list(Size)), Size div 2, Size, 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

update(_, _, _, 0) -> ok;
update(Dict, Key, Value, N) -> 
	dict:update(Key, fun(_) -> Value end, Dict),
	update(Dict, Key, Value, N - 1).

store(_, _, _, 0) -> ok;
store(Dict, Key, Value, N) -> 
	dict:store(Key, Value, Dict),
	store(Dict, Key, Value, N - 1).
