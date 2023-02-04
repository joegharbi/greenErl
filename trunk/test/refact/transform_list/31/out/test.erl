-module(test).

create_tuple() -> {1,2}.

recursive(Key, List) ->
    begin
        Tuple1 = create_tuple(), (maps:remove(Key,
            List))#{element(1, Tuple1)=>element(2, Tuple1)}
    end,
    recursive(Key, List).
