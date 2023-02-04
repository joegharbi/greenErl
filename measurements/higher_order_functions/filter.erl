-module(filter).
-export([generate_input/1, filter_named/1, filter_fun/1, recursive/1, list_comprehension/1, list_comprehension_fun/1, list_comprehension_named/1, my_filter_named/1, my_filter_fun/1]).
% -compile(export_all).

generate_input(Length) -> [lists:seq(1, Length)].

even(N) -> N rem 2 == 0.

filter_named(L) -> 
	lists:filter(fun even/1, L).

filter_fun(L) -> 
	lists:filter(fun(N) -> N rem 2 == 0 end, L).

recursive([]) -> [];
recursive([H | T]) -> 
	if 
		H rem 2 == 0 -> [H | recursive(T)];
		true -> recursive(T)
	end.

list_comprehension(L) ->
	[Elem || Elem<-L, Elem rem 2 == 0].

list_comprehension_fun(L) -> 
	[Elem || Elem<-L, fun(N) -> N rem 2 == 0 end(Elem)].

list_comprehension_named(L) -> 
	[Elem || Elem<-L, even(Elem)].

my_filter(_, []) -> [];
my_filter(F, [H | T]) -> 
	case F(H) of
		true -> [H | my_filter(F, T)];
		false -> my_filter(F, T)
	end.

my_filter_named(L) ->
	my_filter(fun even/1, L).

my_filter_fun(L) ->
	my_filter(fun(N) -> N rem 2 == 0 end, L).