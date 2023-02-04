-module(sparse).
-compile(export_all).

-import(lists,[filter/2]).


% "sparse.dets"-ben tárolva vannak a tesztesetek
% "sparse100.dets"-ben a 100%-osak
% {{Meret,list/array},Rows,Cols} formában
% [{type, set}] -tipusu a dets

insert_into(FileName,Size,Sparsity,ListA,ListB) ->
	dets:open_file(FileName,[{type, set}]),
	dets:insert(FileName,{{Size,list,Sparsity},ListA,ListB}),
	ListAArr = sparse_list_to_array(ListA,array:new()),
	ListBArr = sparse_list_to_array(ListB,array:new()),
	dets:insert(FileName,{{Size,array,Sparsity},ListAArr,ListBArr}),
	ListAF = sparse_list_to_array_fix(ListA,array:new(Size),Size),
	ListBF = sparse_list_to_array_fix(ListB,array:new(Size),Size),
	dets:insert(FileName,{{Size,array_fix,Sparsity},ListAF,ListBF}),
	dets:close(FileName).


measure({{Size,list,Sparsity},Rows,Cols})->
	Sp = lists:flatten(io_lib:format("~p", [Sparsity])),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_list, [Rows,Cols], Size},10,"results/Geri/sparse/sparse_res_list_"++Sp,"results/Geri/sparse/sparse_avg_list_"++Sp,"results/Geri/sparse/sparse_result_list_"++Sp++".txt"),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_nohof, [Rows,Cols], Size},10,"results/Geri/sparse/sparse_res_nohof_"++Sp,"results/Geri/sparse/sparse_avg_nohof_"++Sp,"results/Geri/sparse/sparse_result_nohof_"++Sp++".txt"),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_par, [Rows,Cols], Size},10,"results/Geri/sparse/sparse_res_par_"++Sp,"results/Geri/sparse/sparse_avg_par_"++Sp,"results/Geri/sparse/sparse_result_par_"++Sp++".txt"),

	continue;
		
measure({{Size,array,Sparsity},Rows,Cols})->	
	Sp = lists:flatten(io_lib:format("~p", [Sparsity])),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_array, [Rows,Cols],Size},10,"results/Geri/sparse/sparse_res_arr_"++Sp,"results/Geri/sparse/sparse_avg_arr_"++Sp,"results/Geri/sparse/sparse_result_arr_"++Sp++".txt"),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_array_nohof, [Rows,Cols],Size},10,"results/Geri/sparse/sparse_res_arr_nohof_"++Sp,"results/Geri/sparse/sparse_avg_arr_nofoh_"++Sp,"results/Geri/sparse/sparse_result_arr_nohof_"++Sp++".txt"),
	continue;	

measure({{Size,array_fix,Sparsity},Rows,Cols})->	
	Sp = lists:flatten(io_lib:format("~p", [Sparsity])),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_array_fix, [Rows,Cols],Size},10,"results/Geri/sparse/sparse_res_arr_fix_"++Sp,"results/Geri/sparse/sparse_avg_arr_fix_"++Sp,"results/Geri/sparse/sparse_result_arr_fix_"++Sp++".txt"),
	energy_consumption:measure("./rapl-read.out",{sparse, mxm_array_nohof_fix, [Rows,Cols],Size},10,"results/Geri/sparse/sparse_res_arr_nohof_fix_"++Sp,"results/Geri/sparse/sparse_avg_arr_nohof_fix_"++Sp,"results/Geri/sparse/sparse_result_arr_nohof_fix_"++Sp++".txt"),
	continue;

measure(_) -> continue.

measure_all(FileName) ->
	{ok,File} = dets:open_file(FileName,[{type,set}]),
	%{ok,File} = dets:open_file(FileName,[{type, duplicate_bag}]),
	%dets:traverse(File, fun(A) -> io:format("~p~n",[A]), continue end),
	%io:format("~w~n",[length(dets:match(FileName,{{'$1','_','_'},'_','_'}))]),
	dets:traverse(FileName, fun measure/1),
	dets:close(File).



%LISTS WITH HOFS

% Az azonos indexeket osszeszorozza, eredmenye egy skalar,
% Be: [{Int,Int}],[{Int,Int}] a tuple-k elso eleme az index, novekvo sorrendben!, a masodik az ertek
vxv_list(Row,Col) -> vxv_acc_list(Row,Col,0).
vxv_acc_list([],_,Acc) -> Acc;
vxv_acc_list(_,[],Acc) -> Acc;
vxv_acc_list([{I,R}|Row],[{I,C}|Col],Acc) ->
	vxv_acc_list(Row,Col,Acc+R*C);
vxv_acc_list([{I,R}|Row],[{J,C}|Col],Acc) ->
	if I < J -> vxv_acc_list(Row,[{J,C}|Col],Acc);
	   true  -> vxv_acc_list([{I,R}|Row],Col,Acc)
	end.

%Egy matrixot szoroz egy vektorral,
%Be: Rows: [{Int,[{Int,Int}]}], az elso int a sor szama, benne a tuple elso intje az oszlop szama
%    Col: [{Int,Int}], az elso int a sorszam, a masodik az ertek 
mxv_list( Rows, Col ) ->
	Product = [ {I,vxv_list(Row,Col)} || {I,Row} <- Rows ],
	filter( fun({_,V}) -> V /= 0 end, Product ).

%Ket matrixot szoroz ossze,
%Be: Rows: [{Int,[{Int,Int}]}], az elso int a sor szama, benne a tuple elso intje az oszlop szama
%    Cols: [{Int,[{Int,Int}]}], az elso int az oszlop szama, benne a tuple elso intje a sor szama
%Ki: az eredmeny, hasonloan Cols-hoz! (oszloponkent)
mxm_list(Rows, Cols) ->
	Product = [{I,mxv_list(Rows,Col)} || {I,Col} <- Cols],
	filter( fun({_,V}) -> V /= [] end, Product).


%NOHOF VERSION
vxv_nohof(Row,Col) -> vxv_acc_nohof(Row,Col,0).
vxv_acc_nohof([],_,Acc) -> Acc;
vxv_acc_nohof(_,[],Acc) -> Acc;
vxv_acc_nohof([{I,R}|Row],[{I,C}|Col],Acc) ->
	vxv_acc_nohof(Row,Col,Acc+R*C);
vxv_acc_nohof([{I,R}|Row],[{J,C}|Col],Acc) ->
	if I < J -> vxv_acc_nohof(Row,[{J,C}|Col],Acc);
	   true  -> vxv_acc_nohof([{I,R}|Row],Col,Acc)
	end.

mxv_nohof( Rows, Col ) ->
	Product = [ {I,vxv_nohof(Row,Col)} || {I,Row} <- Rows ],
	filter_zeros_nohof(Product,[]).

mxm_nohof(Rows, Cols) ->
	Product = [{I,mxv_nohof(Rows,Col)} || {I,Col} <- Cols],
	filter_empty_nohof(Product,[]).

filter_zeros_nohof([],R) -> lists:reverse(R);
filter_zeros_nohof([{_,0}|P],R) -> filter_zeros_nohof(P,R);
filter_zeros_nohof([H|P],R) -> filter_zeros_nohof(P,[H|R]).

filter_empty_nohof([],R) -> lists:reverse(R);
filter_empty_nohof([{_,[]}|P],R) -> filter_empty_nohof(P,R);
filter_empty_nohof([H|P],R) -> filter_empty_nohof(P,[H|R]).

%PARALLEL VERSION

%This uses recursive vxv from list version!!!
vxv_par(Pid, Row, Col) ->
	Pid ! {self(), vxv_list(Row, Col)}.

mxv_par(Parent, Rows, Col ) ->
	IPids = [{I, spawn(?MODULE, vxv_par, [self(), Row, Col])} || {I,Row} <- Rows ],
	Product = [receive 
	   		{Pid, Res} -> {I, Res}
	 	   end || {I, Pid} <- IPids],
	Parent ! {self(),filter( fun({_,V}) -> V /= 0 end, Product )}.

mxm_par(Rows, Cols) ->
	IPids = [{I, spawn(?MODULE, mxv_par, [self(), Rows, Col])} || {I,Col} <- Cols],
	Product = [receive 
			{Pid, Res} -> {I, Res}
			end || {I, Pid} <- IPids],
	filter( fun({_,V}) -> V /= [] end, Product).


%PARALLEL VERSION WITH MAP
vxv_parwithmap(Pid, Row, Col) ->
	Pid ! {self(), vxv_list(Row, Col)}.

mxv_parwithmap(Parent, Rows, Col ) ->
	IPids = lists:map(fun({I,Row}) -> {I, spawn(?MODULE, vxv_parwithmap, [self(), Row, Col])} end,Rows),		
	%IPids = [{I, spawn(?MODULE, vxv_parwithmap, [self(), Row, Col])} || {I,Row} <- Rows ],
	Product = [receive 
	   		{Pid, Res} -> {I, Res}
	 	   end || {I, Pid} <- IPids],
	Parent ! {self(),filter( fun({_,V}) -> V /= 0 end, Product )}.

mxm_parwithmap(Rows, Cols) ->
	IPids = lists:map(fun({I,Col}) -> {I, spawn(?MODULE, mxv_parwithmap, [self(), Rows, Col])} end,Cols),
	%IPids = [{I, spawn(?MODULE, mxv_parwithmap, [self(), Rows, Col])} || {I,Col} <- Cols],
	Product = [receive 
			{Pid, Res} -> {I, Res}
			end || {I, Pid} <- IPids],
	filter( fun({_,V}) -> V /= [] end, Product).

%PARALLEL VERSION WITH PAR_MAP

vxv_parmap(Pid, Row, Col) ->
	Pid ! vxv_list(Row, Col).

mxv_parmap(Parent, Rows, Col ) ->
	Parent ! filter( fun({_,V}) -> V /= 0 end, ord_par_map(fun({I,Row}) -> {I,vxv_parmap(self(),Row,Col)} end, Rows) ).
	
mxm_parmap(Rows, Cols) ->
	filter( fun({_,V}) -> V /= [] end, ord_par_map(fun({I,Col}) -> {I,mxv_parmap(self(),Rows,Col)} end, Cols)).

ord_par_map(F, Xs) ->
	Me = self(),
	Pids = [spawn(fun() -> Me ! {self(), F(X)} end) || X<-Xs],
	[receive {Pid, Res} -> Res end || Pid <- Pids].

%PARALLEL VERSION WITH PROCESS POOL

mxv_parmap_pp(Parent, Rows, Col, P) ->
	Parent ! filter( fun({_,V}) -> V /= 0 end, ordmap(fun({I,Row}) -> {I,vxv_parmap(self(),Row,Col)} end, Rows,P) ).
	
mxm_parmap_pp(Rows, Cols,P) ->
	filter( fun({_,V}) -> V /= [] end, ordmap(fun({I,Col}) -> {I,mxv_parmap_pp(self(),Rows,Col,P)} end, Cols,P)).

%PARALLEL -> SEQUENTIAL VERSION: PROCESS POOL 

mxm_parmap_ps(Rows, Cols, P) ->
	filter( fun({_,V}) -> V /= [] end, ordmap(fun({I,Col}) -> {I,mxv_list(Rows,Col)} end, Cols,P)).

%PROCESS POOL
ordmap(F, L, N) ->
	Main = self(),
	Index = lists:seq(1,length(L)),
	DPid = spawn(fun() -> dispatcher(lists:zip(Index, L)) end),
	CPid = spawn(fun() -> collector(0, length(L), [], Main) end),
	Workers = [spawn(fun() -> worker(F, DPid, CPid) end) || _<- lists:seq(1, N)],
	[W ! init || W <- Workers],
	receive 
		{value, CPid, Data} ->
			Data
	after 5000 -> Data = []
	end,
	[W ! stop || W <- Workers],
	Data.
	
worker_init(F, DPid, CPid) ->
	DPid ! {done, self()},
	worker_(F, DPid, CPid).

worker_(F, DPid, CPid)->
	receive 
		{data, D} -> 
			CPid ! {ready, F(D)},
			DPid ! {done, self()},
			worker_(F, DPid, CPid);
		stop ->
			ok
	end.

worker(F, DPid, CPid)->
	receive 
		{data, {I, D}} -> 
			CPid ! {ready, {I, F(D)}},
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		init ->
			DPid ! {done, self()},
			worker(F, DPid, CPid);
		stop ->
			ok
	end.

collector(N, N, Acc, Main) ->
	Main ! {value, self(), lists:reverse(Acc)};
collector(C, N, Acc, Main) ->
	receive 
		{ready, {I, Data}} when I == C+1 ->
			collector(C+1, N, [Data | Acc], Main)
	end.

dispatcher([]) ->
	ok;
dispatcher([H|T]) ->
	receive 
		{done, W} ->
			W ! {data, H},
			dispatcher(T)
	end.

%Array implementation

%Transform list input to array -- just for testing! call with A = array:new()
sparse_list_to_array([],A) -> A;
sparse_list_to_array([{Row,Elems}|L],A) ->
	sparse_list_to_array(L,array:set(Row-1,get_sparse_row(Elems,array:new()),A)).

sparse_list_to_array_fix([],A,_) -> A;
sparse_list_to_array_fix([{Row,Elems}|L],A,Size) ->
	sparse_list_to_array_fix(L,array:set(Row-1,get_sparse_row(Elems,array:new(Size)),A),Size).


get_sparse_row([],A) -> A;
get_sparse_row([{Col,Val}|L],A) -> 
	get_sparse_row(L,array:set(Col-1,Val,A)).

vxv_array(Row,Col) ->
	A = array:sparse_foldr(
			fun(_,Val,Acc)->
				Acc + Val
			end,
			0,
			array:sparse_map(
				fun(Index,Elem) -> 
					C = array:get(Index,Col),
					if 	C == undefined -> undefined;
						true -> Elem*C
					end
				end, 
			Row)),
	
	if 	A == 0 -> undefined;
		true -> A
	end.

mxv_array(Rows,Col) -> 
	A = array:sparse_map(
		fun(_,Row) -> 
			if 	Row == undefined -> undefined;
				true -> vxv_array(Row,Col) 
			end 
		end, 
	Rows),
	S = array:sparse_size(A),
	if 	S==0 -> undefined;
		true -> A
	end.

mxm_array(Rows,Cols) ->
	array:sparse_map(
		fun(_,Col) ->
			if 	Col == undefined -> undefined;
				true -> mxv_array(Rows,Col)
			end
		end,
	Cols).

%Fix sized arrays, same code as not fix, but called with different input!!
mxm_array_fix(Rows,Cols) ->
	array:sparse_map(
		fun(_,Col) ->
			if 	Col == undefined -> undefined;
				true -> mxv_array(Rows,Col)
			end
		end,
	Cols).

%Array with no HOFs
vxv_array_nohof(Row,Col) ->
	T = vxv_array_nohof_map(
		0,
		array:size(Row),
		Col,
		Row),
	A = vxv_array_nohof_foldr(
		0, 
		array:size(T),
		T,
		0),
	if 	A == 0 -> undefined;
		true -> A
	end.

vxv_array_nohof_foldr(Index,Size,_,Acc) when Index == Size -> Acc;
vxv_array_nohof_foldr(Index,Size,Array,Acc) ->
	Elem = array:get(Index,Array),
	if 	Elem == undefined -> vxv_array_nohof_foldr(Index+1,Size,Array,Acc);
		true -> vxv_array_nohof_foldr(Index+1,Size,Array,Acc + Elem)
	end.

vxv_array_nohof_map(Index,Size,_,Row) when Index == Size -> Row;
vxv_array_nohof_map(Index,Size,Col,Row) ->
	ElemR = array:get(Index,Row),
	ElemC = array:get(Index,Col),
	if  ElemR == undefined -> vxv_array_nohof_map(Index+1,Size,Col,Row);
		ElemC == undefined -> vxv_array_nohof_map(Index+1,Size,Col,array:set(Index, undefined, Row));
		true -> vxv_array_nohof_map(Index+1,Size,Col,array:set(Index, ElemC*ElemR, Row))
	end.

mxv_array_nohof(Rows,Col) -> 
	{S,A} = mxv_array_nohof_map(0,array:size(Rows),Rows,Col,0),
	if 	S==0 -> undefined;
		true -> A
	end.


mxv_array_nohof_map(Index,Size,Rows,_,S) when Index == Size -> {S,Rows};
mxv_array_nohof_map(Index,Size,Rows,Col,S) ->
	Row = array:get(Index,Rows),
	if 	Row == undefined -> mxv_array_nohof_map(Index+1,Size,Rows,Col,S);
		true -> Res = vxv_array_nohof(Row,Col),
				if 	Res == undefined -> mxv_array_nohof_map(Index+1,Size,array:set(Index,Res,Rows),Col,S);
					true -> mxv_array_nohof_map(Index+1,Size,array:set(Index,Res,Rows),Col,S+1)
				end
	end.

mxm_array_nohof(Rows,Cols) ->
	mxm_array_nofof_map(0,array:size(Cols),Rows,Cols).

mxm_array_nofof_map(Index,Size,_,Cols) when Index == Size -> Cols;
mxm_array_nofof_map(Index,Size,Rows,Cols) ->
	Col = array:get(Index,Cols),
	if 	Col == undefined -> mxm_array_nofof_map(Index+1,Size,Rows,Cols);
		true -> mxm_array_nofof_map(Index+1,Size,Rows,array:set(Index,mxv_array_nohof(Rows,Col),Cols))
	end.

%Fix array with no HOFs
%same code as not fix, different arguments
mxm_array_nohof_fix(Rows,Cols) ->
		mxm_array_nofof_map(0,array:size(Cols),Rows,Cols).
