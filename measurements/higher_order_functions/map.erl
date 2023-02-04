-module(map).
-export([generate_input/1, map_named/1, map_fun/1, recursive/1, list_comprehension/1, list_comprehension_fun/1, list_comprehension_named/1, my_map_named/1, my_map_fun/1]).
% -compile(export_all).

generate_input(Length) -> [lists:seq(1,Length)].

% Increment all items in a list by 1 using different map implementations

% Increase the elem by one
increase(X) -> X + 1.


% Using lists:map and named function
map_named(L) ->
	lists:map(fun increase/1, L).

% Using lists:map and fun
map_fun(L) ->
	lists:map(fun(X) -> X + 1 end, L).

% Using recursion, no higher order function
recursive([]) -> [];
recursive([L|Ls]) -> 
	[L+1 | recursive(Ls)].

% Using list comprehension with simple expression
list_comprehension(L) ->
	[Elem + 1 || Elem<-L].

% Using list comprehension with fun
list_comprehension_fun(L) ->
	[fun(X) -> X + 1 end(Elem) || Elem<-L].

% Using list comprehension with named function
list_comprehension_named(L) ->
	[increase(Elem) || Elem<-L].

my_map(_, []) -> [];
my_map(F, [H | T]) -> [F(H) | my_map(F, T)].

% Using my_map and named function
my_map_named(L) ->
	my_map(fun increase/1, L).

% Using my_map and fun
my_map_fun(L) ->
	my_map(fun(X) -> X + 1 end, L).