-module(data_struct_after).
-export([generate_input/1, main/1]).


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

main(List) -> recursive(maps:from_list(List), 1).

recursive(Map1, _) when map_size(Map1) =:= 0 -> ok;
recursive(List, Key) ->
    #{Key:=Value} = List,
    NewList = List#{Key=>Value + 1},
    {value, _, NewList2} = case maps:take(Key, NewList) of
            error -> false;
            {Value1, List1} -> {value, {Key, Value1}, List1}
        end,
    recursive(NewList2, Key + 1).
