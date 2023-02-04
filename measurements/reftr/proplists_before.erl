-module(proplists_before).
-export([generate_input/1, f/3]).


% Inputs:
% 10000,50000,100000,500000,1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,10000000

% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
    [Size div 2, generate_list(Size), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

f(_, _, 0) -> ok;
f(Key, List, N) ->
    proplists:get_value(Key, List),
    f(Key, List, N - 1).