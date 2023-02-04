-module(hof_before).
-export([generate_input/1, f/1]).

% Inputs:
% 10000,50000,100000,500000,1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,10000000

generate_input(N) -> [lists:seq(1, N)].

do_stuff(X) ->
    A = X,
    B = X*X,
    C = X*X*X,
    A + B + C.

even(X) -> X rem 2 =:= 0.

f(List) ->
    Y = 42,
    NewList = lists:map(fun(X) -> case even(X) of true -> Y; false -> X end end, List),
    NewList2 = lists:filter(fun(X) -> X =/= Y end, NewList),
    lists:map(fun do_stuff/1, NewList2).
