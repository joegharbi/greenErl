-module(filtexpr).
-export([f/2]).

f(List, Y) ->
    X = 2,
    Z = 3,
    A = 4,
    lists:filter(fun(Elem) when Elem =:= 42 -> 
		      Z = X + Y,
		      Elem + Z,
		      5 =:= 2;
		 (X) -> X1 = X =:= Y;
		 ({X,Y}) -> false;
		 (_) -> true;
		 (Alma = {X, _}) -> false;
		 (_ = []) -> 2 =:= 3 orelse false end, List).
		      
