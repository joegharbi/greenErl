-module(skel_fib).
-export([generate_input/1,farm/2,farm_ord/2]).

generate_input(Length) -> [generate_list(Length), 8].

generate_list(0) -> [];
generate_list(Length) -> [15| generate_list(Length-1)].

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).

farm(L, K) -> 
    skel:do([{farm, [{seq, fun fib/1}], K}], L).

farm_ord(L, K) -> 
    skel:do([{ord, [{farm, [{seq, fun fib/1}], K}]}], L).
