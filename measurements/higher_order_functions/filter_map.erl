-module(filter_map).
-export([generate_input/1, filter_map_comp/1, filtermap/1, recursive/1, recursive_named/1, list_comprehension/1, list_comprehension_named/1, my_filter_map/1]).
% -compile(export_all).

generate_input(Length) -> [lists:seq(1,Length)].

increase(X) -> X + 1.
even(N) -> N rem 2 == 0.

filter_map_comp(L) -> 
	lists:map(fun increase/1, lists:filter(fun even/1, L)).

increase_even(X) ->
	case X rem 2 == 0 of
		true -> {true, X + 1};
		false -> false
	end.

filtermap(L) -> 
	lists:filtermap(fun increase_even/1, L).

recursive([]) -> [];
recursive([H | T]) -> 
	case H rem 2 == 0 of
		true -> [H + 1 | recursive(T)];
		false -> recursive(T)
	end.

recursive_named([]) -> [];
recursive_named([H | T]) -> 
	case even(H) of
		true -> [increase(H) | recursive_named(T)];
		false -> recursive_named(T)
	end.

list_comprehension(L) ->
	[Elem + 1 || Elem<-L, Elem rem 2 == 0].

list_comprehension_named(L) ->
	[increase(Elem) || Elem<-L, even(Elem)].

my_filter_map(_, _, []) -> [];
my_filter_map(F, M, [H | T]) ->
	case F(H) of
		true -> [M(H) | my_filter_map(F, M, T)];
		false -> my_filter_map(F, M, T)
	end.

my_filter_map(L) ->
	my_filter_map(fun even/1, fun increase/1, L).

% inputs:
% 10000, 50000, 100000, 500000, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000