-module(n_queens).
-export([queens_list/1, queens_nohof/1, queens_array/1, queens_array_fix/1, queens_par/1, generate_input/1]).
-import(tr11,[parmap/3]).

generate_input(N) -> [N].

% Test sleeping and measuring idle energy consumption
queens_sleep(Time) -> timer:sleep(round(Time)).

% Attacks function used by solutions
attacks({RowA, ColA}, {RowB, ColB}) ->
		RowA == RowB orelse ColA == ColB orelse
			abs(RowA - RowB) == abs(ColA - ColB).

% N queens with lists and Higher Order Functions
legal_list(Queen, Queens) ->
	lists:all(fun(Q) -> not (attacks(Queen, Q)) end, Queens).
solve_list(N, Row, Queens) when Row > N -> [Queens];
solve_list(N, Row, Queens) ->
	lists:flatmap(
		fun(Qs) -> solve_list(N,Row+1,Qs) end,
		[ [{Col,Row} | Queens] || Col <- lists:seq(1,N),legal_list({Col,Row},Queens) ]
	).

queens_list(N) when N > 0 -> solve_list(N,1,[]).

% N queens without Higher Order Functions --- the result is reversed compared to the previous version

solve_nohof(N, Row, Queens) when Row > N -> [Queens];
solve_nohof(N, Row, Queens) ->
	flatmap_nohof(
		[ [{Col,Row} | Queens] || Col <- lists:seq(1,N),all_nohof({Col,Row},Queens) ],N,Row
	).

queens_nohof(N) when N > 0 -> solve_nohof(N,1,[]).

all_nohof(_,[]) -> true;
all_nohof(Queen,[Q|Queens]) -> 
	G = attacks(Queen,Q),
	if G -> false;
	true -> all_nohof(Queen,Queens)
end.

flatmap_nohof(Queens,N,Row) -> flatmap_nohof(Queens,[],N,Row).
flatmap_nohof([],R,_,_) -> R;
flatmap_nohof([H|T],R,N,Row) ->
	flatmap_nohof(T,solve_nohof(N,Row+1,H) ++ R,N,Row).

concat_to([],R) -> R;
concat_to([H|T],R) -> concat_to(T,[H|R]).

% N queens with arrays (not fix sized)

queens_array(N) when N > 0 -> 
	solve_array(N,1,array:new()).

legal_array(Queen,IQ,Q) ->
	Size_Q = array:size(Q),
	if  Size_Q == IQ -> true;
	true ->
		B = attacks(Queen,array:get(IQ,Q)),
		if B -> false;
			true -> legal_array(Queen,IQ+1,Q)
		end
	end.

get_arrays(_,N,_,Col,R) when Col > N -> R;
get_arrays(Q,N,Row,Col,R) ->
	B = legal_array({Col,Row},0,Q),
	if B 	-> get_arrays(Q,N,Row,Col+1,array:set(array:size(R),array:set(array:size(Q),{Col,Row},Q),R));
	   true	-> get_arrays(Q,N,Row,Col+1,R)
	end.


solve_array(N, Row, Queens) when Row > N -> array:set(0,Queens,array:new());
solve_array(N, Row, Queens) ->
	flatmap_array(
		get_arrays(Queens,N,Row,1,array:new()),N,Row
	).



flatmap_array(Queens,N,Row) -> flatmap_array(0,Queens,array:new(),N,Row).
flatmap_array(IQ,Q,R,N,Row) ->
	Size_Q = array:size(Q),
	if  Size_Q == IQ -> R;
	true ->
		flatmap_array(IQ+1,Q,concat_to_array(0, solve_array(N,Row+1,array:get(IQ,Q)),R),N,Row)
	end.

concat_to_array(IA,A,R) -> 
	Size_A = array:size(A),
	if  Size_A == IA -> R;
	true ->
		concat_to_array(IA+1, A, array:set(array:size(R),array:get(IA,A),R))
	end.

% N queens with arrays (fix sized)

queens_array_fix(N) when N > 0 -> 
	solve_array_fix(N,1,array:new(N)).

legal_array_fix({_,Row} = Queen,IQ,Q) ->
	if  Row-1 == IQ -> true;
	true ->
		B = attacks(Queen,array:get(IQ,Q)),
		if B -> false;
			true -> legal_array_fix(Queen,IQ+1,Q)
		end
	end.

get_arrays_fix(_,N,_,Col,R) when Col > N -> R;
get_arrays_fix(Q,N,Row,Col,R) ->
	B = legal_array_fix({Col,Row},0,Q),
	if B 	-> get_arrays_fix(Q,N,Row,Col+1,array:set(array:size(R),array:set(Row-1,{Col,Row},Q),R));
	   true	-> get_arrays_fix(Q,N,Row,Col+1,R)
	end.


solve_array_fix(N, Row, Queens) when Row > N -> array:set(0,Queens,array:new());
solve_array_fix(N, Row, Queens) ->
	flatmap_array_fix(
		get_arrays_fix(Queens,N,Row,1,array:new()),N,Row
	).



flatmap_array_fix(Queens,N,Row) -> flatmap_array_fix(0,Queens,array:new(),N,Row).
flatmap_array_fix(IQ,Q,R,N,Row) ->
	SQ = array:size(Q),
	if  SQ == IQ -> R;
	true ->
		flatmap_array_fix(IQ+1,Q,concat_to_array_fix(0, solve_array_fix(N,Row+1,array:get(IQ,Q)),R),N,Row)
	end.

concat_to_array_fix(IA,A,R) -> 
	Size_A = array:size(A),
	if  Size_A == IA -> R;
	true ->
		concat_to_array_fix(IA+1, A, array:set(array:size(R),array:get(IA,A),R))
	end.


% Paralell --- 12 es  folotte fel kell venni a max processek szamat!

queens_par(N) ->
		solutions_par(N, N, []).
	
solutions_par(0, _Rows, Xs) ->
	[Xs];
solutions_par(N, Rows, Xs) ->
	Next = [ [Row | Xs] || Row <- lists:seq(1, Rows),
							legal_par(Row, Xs)],
	lists:append(par_map(fun(Xs2) -> solutions_par(N - 1, Rows, Xs2) end, Next)).

legal_par(Row, Queens) ->
	not lists:member(true, [attacks_par(Row, Queens, I) || I <- lists:seq(1, length(Queens))]).

attacks_par(Q, Queens, I) ->
	B = lists:nth(I, Queens),
	Q == B orelse abs(Q - B) == I.

par_map(F, Xs) ->
	Me = self(),
	[spawn(fun() -> Me ! F(X) end) || X<-Xs],
	[receive Res -> Res end || _ <- Xs].


%Parallel with dispatcher and collector		

queens_par_dis(N) ->
		solutions_par_dis(N, N, []).
	
solutions_par_dis(0, _Rows, Xs) ->
	[Xs];
solutions_par_dis(N, Rows, Xs) ->
	Next = [ [Row | Xs] || Row <- lists:seq(1, Rows), legal_par(Row, Xs)],
	lists:append(parmap(fun(Xs2) -> solutions_par_dis(N - 1, Rows, Xs2) end, Next, 8)).

