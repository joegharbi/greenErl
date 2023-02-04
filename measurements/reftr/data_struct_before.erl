-module(data_struct_before).
-export([generate_input/1, main/1]).

% Inputs:
% 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 14000, 16000, 18000, 20000
generate_input(Size) ->
    random:seed(0),
    [shuffle(generate_list(Size))].

generate_list(0) -> [];
generate_list(N) -> [{N, N} | generate_list(N - 1)].

shuffle([])     -> [];
shuffle([Elem]) -> [Elem];
shuffle(List)   -> shuffle(List, length(List), []).

shuffle([], 0, Result) ->
    Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(random:uniform(Len), List),
    shuffle(Rest, Len - 1, [Elem|Result]).

nth_rest(N, List) -> nth_rest(N, List, []).

nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).

main(List) -> recursive(List, 1).

recursive([], _) -> ok;
recursive(List, Key) ->
    {_, Value} = lists:keyfind(Key, 1, List),
    NewList = lists:keystore(Key, 1, List, {Key, Value + 1}),
    {value, _, NewList2} = lists:keytake(Key, 1, NewList),
    recursive(NewList2, Key + 1).