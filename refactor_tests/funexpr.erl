-module(funexpr).
-export([f/2]).

f(List, Y) ->
    X = 2,
    Z = 3,
    A = 4,
    lists:map(fun(Elem) when Elem =:= 42 -> 
		      Z = X + Y,
		      Elem + Z,
		      Q = 2;
		 (X) -> X1 = X + Y;
		 ({X,Y}) -> oh end, List).
		      
