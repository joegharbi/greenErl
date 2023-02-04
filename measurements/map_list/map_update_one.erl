-module(map_update_one).
-export([generate_input/1, update/4, update_with/4, pattern_match/4, overwrite/4]).
% -compile(export_all).

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
	[maps:from_list(generate_list(Size)), Size div 2, Size, 1000000].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

update(_, _, _, 0) -> ok;
update(Map, Key, Value, N) -> 
	maps:update(Key, Value, Map),
	update(Map, Key, Value, N - 1).

update_with(_, _, _, 0) -> ok;
update_with(Map, Key, Value, N) -> 
	maps:update_with(Key, fun(_) -> Value end, Map),
	update_with(Map, Key, Value, N - 1).

pattern_match(_, _, _, 0) -> ok;
pattern_match(Map, Key, Value, N) -> 
	Map#{Key := Value},
	pattern_match(Map, Key, Value, N - 1).

overwrite(_, _, _, 0) -> ok;
overwrite(Map, Key, Value, N) -> 
	Map#{Key => Value},
	overwrite(Map, Key, Value, N - 1).