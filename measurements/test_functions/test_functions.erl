-module(test_functions).
-compile(export_all).

create(1) -> [1];
create(N) -> [N|create(N-1)].
reverse_create(N) -> lists:reverse(create(N)).
list_create(N) -> lists:map(fun (X) -> [X] end, reverse_create(N)).


    % non tail recursive
    fib_acc(0) ->
        0;
    fib_acc(1) ->
        1;
    fib_acc(N) ->
        fib_acc(N-1) + fib_acc(N-2).
     
    % tail recursive
    fib_tail(0) ->
        0;
    fib_tail(1) ->
        1;
    fib_tail(N) ->
        fib_tail(0, 1, 2, N).
     
    fib_tail(L, H, C, N) when C >= N ->
        L + H;
    fib_tail(L, H, C, N) ->
        fib_tail(H, L + H, C + 1, N).
    
    %paralell
    
    fib_paralell(N) when N < 15, N >= 0 ->
        fib_p(N);
    fib_paralell(N) when N > 1->
        SubProb = [N-1, N-2],
        SubSol = pmap(fun fib_p/1, SubProb),
        lists:sum(SubSol).
    
    fib_p(0) ->
        0;
    fib_p(1) ->
        1;
    fib_p(N) when N > 1->
        fib_p(N-1) + fib_p(N-2).
    
    pmap(F, L) ->
        MyPid = self(),
        [spawn(fun() -> MyPid ! F(H)  end) || H <- L],
        [receive
         Res -> Res
         end || _ <- L].
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    fib_1(0) ->    0;
    fib_1(1) ->    1;
    fib_1(N) ->
        fib_1(N-1) + fib_1(N-2).
    
    %%% Itroduce variable
    fib_2(1) -> 1;
    fib_2(0) -> 0;
    fib_2(N) ->
        A = fib_2(N - 1),
        B = fib_2(N - 2),
        A + B.
    
    %%% Bindings to list
    fib_3(1) -> 1;
    fib_3(0) -> 0;
    fib_3(N) ->
        [A, B] = [fib_3(N - 1), fib_3(N - 2)],
        A + B.
    
    %%%% Itroduce map
    
    fib_4(1) -> 1;
    fib_4(0) -> 0;
    fib_4(N) ->
        [A, B] = lists:map(fun fib_4/1, [N - 1, N - 2]),
        A + B.
    
    %%%% Introduce variables
    
    fib_5(1) -> 1;
    fib_5(0) -> 0;
    fib_5(N) ->
        SubPr = [N - 1, N - 2],
        [A, B] = lists:map(fun fib_5/1, SubPr),
        A + B.
    
    %%%% Introduce variable
    fib_6(1) -> 1;
    fib_6(0) -> 0;
    fib_6(N) ->
        SubPr = [N - 1, N - 2],
        SubSols = lists:map(fun fib_6/1, SubPr),
        [A, B] = SubSols,
        A + B.
    
    %%% Introduce lists:sum
    fib_7(1) -> 1;
    fib_7(0) -> 0;
    fib_7(N) ->
        SubPr = [N - 1, N - 2],
        SubSols = pmap(fun fib_7/1, SubPr),
        lists:sum(SubSols).
    
    
    %%%%%%%%%%%%%%%%%%%%
    
    
    %%-----------------------------------------------------------------------------
    %% Naive tail-recursive.
    %%-----------------------------------------------------------------------------
    fib_rec_naive(0) -> 0;
    fib_rec_naive(1) -> 1;
    fib_rec_naive(N) -> fib_rec_naive(N - 1) + fib_rec_naive(N - 2).
    
    
    
    %%-----------------------------------------------------------------------------
    %% Naive translation of mutable array-style.
    %%-----------------------------------------------------------------------------
    fib_arr(0) -> 0;
    fib_arr(1) -> 1;
    fib_arr(N) ->
        Begin = 0,
        End = N + 1,
        fib_arr(N, End, 2, array:from_list(lists:seq(Begin, End))).
    
    fib_arr(N, End, I, Fibs) when I == End -> array:get(N, Fibs);
    fib_arr(N, End, I, Fibs) ->
        Fib = array:get(I-1, Fibs) + array:get(I-2, Fibs),
        fib_arr(N, End, I+1, array:set(I, Fib, Fibs)).
    
    
    %%-----------------------------------------------------------------------------
    %% Naive translation of mutable array-style, but using list structure.
    %%-----------------------------------------------------------------------------
    fib_list_naive(0) -> 0;
    fib_list_naive(1) -> 1;
    fib_list_naive(N) ->
        fib_list_naive(N + 2, 3, [0, 1]).
    
    fib_list_naive(End, I, Fibs) when I == End -> lists:last(Fibs);
    fib_list_naive(End, I, Fibs) ->
        Fib = lists:nth(I-1, Fibs) + lists:nth(I-2, Fibs),
        fib_list_naive(End, I+1, Fibs++[Fib]).
    
    
    %%-----------------------------------------------------------------------------
    %% Idiomatic use of the list (in reverse order).
    %% Credit: yrashk (Yurii Rashkovskii)
    %%-----------------------------------------------------------------------------
    fib_list(0) -> 0;
    fib_list(1) -> 1;
    fib_list(N) ->
        fib_list(N + 1, [1,0]).
    
    fib_list(End, [H|_]=L) when length(L) == End -> H;
    fib_list(End, [A,B|_]=L) ->
        fib_list(End, [A+B|L]).
    
    
    %%-----------------------------------------------------------------------------
    %% Minimal arithmetic.
    %% Credit: richcarl (Richard Carlsson)
    %%-----------------------------------------------------------------------------
    fib_arith(N) when N > 0 -> fib_arith(N, 0, 1).
    
    fib_arith(0, F1, _F2) -> F1;
    fib_arith(N, F1,  F2) -> fib_arith(N - 1, F2, F1 + F2).
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %-----RECURSION---------
    sum_recursion([]) ->
        0; 
    sum_recursion([H|T]) ->
        H+sum_recursion(T).
    
    %----ACCUMULATOR-------------
    sum_acc([H|T]) ->
        sum_acc_help([H|T],0).
    
    sum_acc_help([H|T],Acc)->
        sum_acc_help(T,Acc+H);
    
    sum_acc_help([],Acc) ->
        Acc.
    
    %----VARIABLES---------------------
    sum_var([H|T]) ->
        sum_var_help([H|T],0).
    
    sum_var_help([H|T],Acc)->
        X=Acc+H,
        sum_acc_help(T,X);
    
    sum_var_help([],Acc) ->
        Acc.
    
    %--------HIGH ORDER FUNCTION--------
    
    sum_hod(L) -> 
     lists:foldl(fun(X, Sum) -> X + Sum end, 0, L).
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %karatsuba
    
    %%%%%%%%%%%%%%%%%%%binary list
    
    karatsuba(A,B) ->
        X = bit_size(A),
        Y = bit_size(B),
        if 
        X < Y -> karatsuba_(shifte(A, Y-X), B);
        X > Y -> karatsuba_(A, shifte(B, X-Y));
        true -> karatsuba_(A, B)
        end.
    
    karatsuba_(<<>>, _)->
        <<>>;
    karatsuba_(_, <<>>)-> 
        <<>>;
    karatsuba_(Num1, Num2) ->
        case {Num1, Num2} of
            {<<0:1>>, _} -> <<0:(bit_size(Num2))>>;
            {<<1:1>>, _} -> Num2;
            {_, <<0:1>>} -> <<0:(bit_size(Num1))>>;
            {_, <<1:1>>} -> Num1;
            _ ->
                M = max(bit_size(Num1), bit_size(Num2)),
                M2 = M - (M div 2),
                <<Low1:M2/bitstring, High1/bitstring>> = Num1,
                <<Low2:M2/bitstring, High2/bitstring>> = Num2,
                Z0 = karatsuba_(Low1, Low2),
                Z1 = karatsuba_(add(Low1, High1), add(Low2, High2)),
                Z2 = karatsuba_(High1, High2),
                add(add(shift(Z2, M2 * 2), Z0), shift(sub(Z1, add(Z2, Z0)), M2))
        end.
    
    shift(B, P)->
        <<0:P, B/bitstring>>.
    
    shifte(B, P)->
        <<B/bitstring, 0:P>>.
    
    add(A, B)->
        X = bit_size(A),
        Y = bit_size(B),
        if 
        X < Y -> add(shifte(A, Y-X), B, 0);
        X > Y -> add(A, shifte(B, X-Y), 0);
        true -> add(A, B, 0)
        end.
    
    add(<<>>,<<>>,0)->
        <<>>;
    add(<<>>,<<>>,1)->
        <<1:1>>;
    add(<<X:1,Y/bitstring>>, <<Z:1, V/bitstring>>, C)-> 
         R = X + Z + C, 
         if 
         R =< 1 -> 
             End = add(Y, V, 0),
             <<R:1, End/bitstring>>;
         R == 2 -> 
             End = add(Y, V, 1),
             <<0:1, End/bitstring>>;
         R == 3 ->
             End = add(Y, V, 1),
             <<1:1, End/bitstring>>
         end.
    
    
    sub(A, B)->
        AS = bit_size(A),
        BS = bit_size(B),
        if 
        AS < BS -> sub(shifte(A, BS-AS), B, 0);
        AS > BS -> sub(A, shifte(B, AS-BS), 0);
        true -> sub(A, B, 0)
        end.
    
    sub(<<>>,<<>>,_) ->
        <<>>;
    sub(<<X:1, Y/bitstring>>, <<Z:1, V/bitstring>>, C) -> 
        R = X - Z - C, 
        if 
            R >= 0 -> 
                End = sub(Y, V, 0),
                <<R:1, End/bitstring>>;
            R == -1 -> 
                End = sub(Y, V, 1),
                <<1:1, End/bitstring>>;
            R == -2 ->
                End = sub(Y, V, 1),
                <<0:1, End/bitstring>>
        end.
    
    bs2int(<<>>) -> 0;
    bs2int(<<X:1, XS/bitstring>>) -> X + 2 * bs2int(XS).
    
    int2bs(0) ->
        <<0:1>>;
    int2bs(1) ->
        <<1:1>>;
    int2bs(N) ->
        <<(N rem 2):1, (int2bs(N div 2))/bitstring>>.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% will
    big_mult(N1,N2) ->
      S1 = integer_to_list(N1),
      S2 = integer_to_list(N2),
      L1 = lists:reverse(stringToIntList(S1)),
      L2 = lists:reverse(stringToIntList(S2)),
      convertListToInt(lists:reverse(karatsuba_2(L1,L2))).
    
    convertIntToList(N) ->
      stringToIntList(integer_to_list(N)).
      
    stringToIntList(List) ->
      [list_to_integer(X) || X<-[[Y] || Y<-List]].
      
    convertListToInt(List) ->
      L = [integer_to_list(Y) || Y<-List],
      list_to_integer(helperIntListToString(L)).
    
    helperIntListToString([]) ->
      "";
      
    helperIntListToString([H|T]) ->
      H ++ helperIntListToString(T).
      
    addZerosToIntList(NZeros, L) ->
      if NZeros > 0 -> addZerosToIntList(NZeros-1,L ++ [0]);
         true -> L 
      end.
      
    opLists(_, [], []) -> [];
    
    opLists(_, _, []) -> [];
    
    opLists(_, [], _) -> [];
      
    opLists(Op, L1, L2) ->
      if 
        Op == "+" -> opLAdd(L1, L2);
        Op == "-" -> opLSub(L1, L2);
        Op == "*" -> opLMul(L1, L2)
      end.
    
    calculateN(L1,L2) ->
      max(length(L1), length(L2)) div 2.
      
    getSecondDigit(N) ->
      N rem 10.
      
    opLAdd(L1, []) -> 
      L1;
    
    opLAdd([],L2) -> 
      L2;
    
    opLAdd([H1|T1],[H2|T2]) -> 
      H = H1 + H2,
      if (H < 10)   -> [H] ++ (opLAdd(T1,T2));
         (H >= 10)  -> if (length(T1) > 0)                     -> [getSecondDigit(H)] ++ opLAdd(([hd(T1)+1] ++ tl(T1)),T2);
                     ((length(T1) == 0) and (length(T2) > 0))  -> [getSecondDigit(H)] ++ opLAdd(T1,([hd(T2)+1] ++ tl(T2)));
                     ((length(T1) == 0) and (length(T1) == 0)) -> [getSecondDigit(H)] ++ [1]
                     end
      end.
      
    opLSub(L1,[]) -> 
      L1;
      
    opLSub([],L2) -> 
      L2;
      
    opLSub([H1|T1],[H2|T2]) -> 
      H = H1 - H2,
      if (H >= 0) -> [H] ++ (opLSub(T1,T2));
             (H < 0)  -> H3 = H1 - H2 + 10,
                     if (length(T1) > 0)                          -> [H3] ++ opLSub(([hd(T1)-1] ++ tl(T1)),T2);
                                        ((length(T1) == 0) and (length(T2) > 0))  -> [H3] ++ opLSub(T1,([hd(T2)-1] ++ tl(T2)));
                        ((length(T1) == 0) and (length(T1) == 0)) -> [H3]
                                     end
        end.
    
    opLMul(L,N) -> 
      lists:reverse(addZerosToIntList(N,lists:reverse(L))).
      
    karatsuba_2(L1,L2) ->
      if 
        (length(L1) < 5) or (length(L2) < 5) -> lists:reverse(convertIntToList(convertListToInt(lists:reverse(L1)) * convertListToInt(lists:reverse(L2))));
        true -> 
          Diff = length(L1) - length(L2),
          {L11, L22} = 
          if
            (Diff > 0) -> {L1, addZerosToIntList(Diff,L2)};
            (Diff < 0) -> {addZerosToIntList((Diff*-1),L1), L2};
            true       -> {L1, L2}
          end,
          N = calculateN(L11,L22),
          {Low1, High1} = lists:split(N,L11),
          {Low2, High2} = lists:split(N,L22),
          %%{Low1,High1,Low2,High2}
          Z0 = karatsuba_2(Low1,Low2),
          Z1 = karatsuba_2(opLists("+",Low1,High1),opLists("+",Low2,High2)),
          Z2 = karatsuba_2(High1,High2),
          %%{Z0,Z1,Z2}
          (opLists("+",opLists("+",opLists("*",Z2,(2*N)),opLists("*",opLists("-",(opLists("-",Z1,Z2)),Z0),N)),Z0))
          
      end.
    
    test() ->
      big_mult(1234567890123,123456789012345) == 1234567890123 *123456789012345.