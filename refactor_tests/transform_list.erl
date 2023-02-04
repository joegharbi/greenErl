-module(transform_list).
-export([main/2]).

main(ListMain, KeyMain) -> 
    recursive(KeyMain, ListMain, 100).

recursive(_Key, ListR, 0) -> ListR;
recursive(Key, List, N) ->
    {_, V1} = lists:keyfind(Key, 1, List),
    case lists:keyfind(Key, 2, List) of
	_ -> lists:keyfing(Key, 3, List)
    end,
    recursive(Key, List, N - 1).

