-module(ex).
-compile(export_all).

%Summation using foldr
sum(L) -> lists:foldr(fun (A,B) -> A+B end, 0, L).

%Return only the positive elements of a list using filter
positives(L) -> lists:filter(fun (A) -> A>0 end, L).

%Decide if all elements are positive using any
all_positive(L) -> lists:all(fun (A) -> A>0 end, L).

%Generate a list and test the previous functions
test() ->
    Lista = [2*X + 3 || X <-lists:seq(-5,5)],
    io:format("~p~n",[sum(Lista)]),
    io:format("~p~n",[positives(Lista)]),
    io:format("~p~n",[all_positive(Lista)]),
    ok.
