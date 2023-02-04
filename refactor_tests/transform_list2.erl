-module(transform_list2).
-export([main/0]).

main() -> 
    ListMain = [{2, 42}, {42, 2}, {a, b}],
    KeyMain = 1,
    AsdList = [{1,alma},{3,4},{4, yaay}],
    % recursive(123, ListMain, 100),
    recursive(KeyMain, ListMain ++ AsdList, 100000).
    % case recursive(321, AsdList, 10) of
    %    [] -> true;
    %     _ -> false
    % end.

recursive(_Key, List1, 0) -> List1;
recursive(Key, List, N) ->
    {_, V1} = lists:keyfind(Key, 1, List),
    {K, V2} = lists:keyfind(Key, 1, List),
    {Key, V3} = lists:keyfind(Key, 1, List),
    {K, V2} = lists:keyfind(Key, 1, List),
    {K2, V4} = lists:keyfind(Key, 1, List),
    {_, V5} = lists:keyfind(Key + 1, 1, List),
    Result = lists:keyfind(Key, 1, List),
    % io:format("V1: ~p~nK: ~p~nV2: ~p~nKey: ~p~nV3: ~p~nK2: ~p~nV4: ~p~nV5: ~p~nResult: ~p~n",
    %           [V1, K, V2, Key, V3, K2, V4, V5, Result]),
    % io:format("~p~n",[lists:keyfind(Key + 1, 1, List)]),
    % io:format("~p~n",[lists:keyfind(1 + hd([3, 4, 5]), 1, List)]),
    % io:format("~p~n",[lists:keyfind(hd([3,4,5]), 1, List)]),
    % io:format("~p~n",[lists:keyfind(42, 1, List)]),
    % io:format("~p~n",[lists:keyfind(a, 1, List)]),
    % case lists:keyfind(1, 1, List) of
    %     {1,alma} -> io:format("ok~n");
    %     _ -> io:format("error~n")
    % end,
    recursive(Key, List, N - 1).

alma() ->
    recursive(1, [{1, 1}, {2, 2}, {3, 3}], 10).
