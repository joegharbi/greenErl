-module(proplists_after).
-export([generate_input/1, f/3]).


% All measured function in this module are run a 100 times
% so that energy consumption is measured more reliably.
generate_input(Size) ->
    [Size div 2, generate_list(Size), 100].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

f(_, _, 0) -> ok;
f(Key, List, N) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Var1} -> Var1
    end,
    f(Key, List, N - 1).
