-module(skellib).
-export([generate_input/1,fib/2,fib_ord/2]).

generate_input(Length) -> [generate_list(Length), 8].

generate_list(0) -> [];
generate_list(Length) -> [15| generate_list(Length-1)].

fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N-1) + fibonacci(N-2).

fib(L, K) -> 
    skel:do([{farm, [{seq, fun fibonacci/1}], K}], L).

fib_ord(L, K) -> 
    skel:do([{ord, [{farm, [{seq, fun fibonacci/1}], K}]}], L).
