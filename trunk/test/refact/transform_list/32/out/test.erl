-module(test).

create_tuple() -> {1,2}.

recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    maps:to_list(List#{Key1=>a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    maps:to_list((maps:remove(Key1 + 1, List))#{Key1 + 1=>a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    maps:to_list((maps:remove(Key1, List))#{Key2=>a});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    maps:to_list((maps:remove(Key1,
            List))#{element(1, Tuple)=>element(2, Tuple)});
recursive(Key1, Key2, Tuple, List) ->
    recursive(Key1, Key2, Tuple, List),
    begin
        Tuple1 = create_tuple(), maps:to_list(
            (maps:remove(Key1, List))#{element(1, Tuple1)=>element(2, Tuple1)})
    end.
