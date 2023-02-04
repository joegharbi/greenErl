-module(skellib).
-export([generate_input/1,farm_fun/2,farm_fun_ord/2]).

generate_input(Length) -> [lists:seq(1,Length), 8].

increase(X) -> X + 1.


farm_named(L, K) -> 
    skel:do([{farm, [{seq, fun increase/1}], K}], L).

farm_named_ord(L, K) -> 
    skel:do([{ord, [{farm, [{seq, fun increase/1}], K}]}], L).

farm_fun(L, K) -> 
    skel:do([{farm, [{seq, fun(X) -> X + 1 end}], K}], L).

farm_fun_ord(L, K) -> 
    skel:do([{ord, [{farm, [{seq, fun(X) -> X + 1 end}], K}]}], L).