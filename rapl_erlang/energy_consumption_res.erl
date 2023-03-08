-module(energy_consumption_res).
-export([measure/3, detsToFile/1, detsToFiles/1]).

measure({Module, Functions, [InputDesc|InputDescs]}, Count, ResultPath) -> 
    IsInputDescList = isList(InputDesc),
    if
        IsInputDescList -> 
            IsGenerateInputAvailable = lists:member({generate_input,length(InputDesc)},
                                        apply(Module, module_info, [exports])),
            if
                IsGenerateInputAvailable ->
                    io:format("~nGenerating input for description ~p~n", [InputDesc]),
                    ArgList = apply(Module, generate_input, InputDesc),
                    measureFunctions({Module, Functions, ArgList, InputDesc}, Count, ResultPath);
                true ->
                    io:format("~nNo generate_input/~p function available~n", [length(InputDesc)]),
                    ArgList = InputDesc,
                    measureFunctions({Module, Functions, ArgList}, Count, ResultPath)
            end;
        true ->
            IsGenerateInputAvailable = lists:member({generate_input,1}, apply(Module, module_info, [exports])),
            if
                IsGenerateInputAvailable ->
                    io:format("~nGenerating input for description ~p~n", [InputDesc]),
                    ArgList = apply(Module, generate_input, [InputDesc]),
                    measureFunctions({Module, Functions, ArgList, InputDesc}, Count, ResultPath);
                true ->
                    io:format("~nNo generate_input/1 function available~n", []),
                    ArgList = [InputDesc],
                    measureFunctions({Module, Functions, ArgList}, Count, ResultPath)
            end
    end,
    measure({Module, Functions, InputDescs}, Count, ResultPath);
measure({_, _, []}, _, _) -> 
    io:format("~nMeasurements finished succesfully~n", []),
    ok.

isList([]) -> true;
isList([_|_]) -> true;
isList(_) -> false.

measureFunctions({Module, Functions, Attributes=[H|_]}, Count, ResultPath) -> 
    measureFunctions( {Module, Functions, Attributes, H}, Count, ResultPath);
measureFunctions({Module, all, Attributes, InputDesc}, Count, ResultPath) ->
    Functions = lists:map(fun({FunctionName, _}) -> 
                    FunctionName end, 
    lists:filter(fun({FunctionName, Arity}) ->
                Arity == length(Attributes) 
                            andalso FunctionName /= module_info 
                            andalso FunctionName /= generate_input end,
        apply(Module, module_info, [exports]))),   
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions({Module, [Function|Functions], Attributes, InputDesc}, Count, ResultPath) ->
    ResultOutput = ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_result",
    AvgOutput    = ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_avg",
    LogFileName  = ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_log.csv",
    io:format("~nCurrently measuring functions with input desctription ~p~n",[InputDesc]),
    measureFunction({Module,Function,Attributes,InputDesc}, Count , ResultOutput, AvgOutput, LogFileName),
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions({_, [], _, _}, _, _) -> ok.



measureFunction({M,F,A=[H|_]}, 0, ResultOutput, AvgOutput, LogFileName) -> 
    measureFunction({M,F,A,H}, 0, ResultOutput, AvgOutput, LogFileName);
    
measureFunction({M,F,A=[H|_]}, Count, ResultOutput, AvgOutput, LogFileName) -> 
    measureFunction({M,F,A,H}, Count, ResultOutput, AvgOutput, LogFileName);


measureFunction({M,F,A,O},0,ResultOutput,AvgOutput,LogFileName) ->
    calculateAverage({M,F,A,O},ResultOutput,AvgOutput),
    averageToFile(AvgOutput,LogFileName);
% measureFunction({_,_,_,_}, 0, _, _, _) -> ok;

measureFunction({M,F,A,O}, Count, ResultOutput, AvgOutput, LogFileName) ->
    % process_flag(trap_exit, true),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
    apply(M, F, A),
    measureFunction({M,F,A,O},Count-1, ResultOutput, AvgOutput, LogFileName).


calculateAverage(T,ResultOutput,AvgOutput) ->
    dets:open_file(AvgOutput,[{type,set}]),
    dets:open_file(ResultOutput,[{type,duplicate_bag}]),
    calcAverageMethod(T,ResultOutput,AvgOutput,"sysfs"),
    calcAverageMethod(T,ResultOutput,AvgOutput,"perf_event"),
    calcAverageMethod(T,ResultOutput,AvgOutput,"msr"),
    calcAverageMethod(T,ResultOutput,AvgOutput,"time"),
    dets:close(ResultOutput),
    dets:close(AvgOutput). 


calcAverageMethod(T,ResultOutput,AvgOutput,"time") ->
    calcAverageType(T,ResultOutput,AvgOutput,"time","time");
calcAverageMethod(T,ResultOutput,AvgOutput,Method) ->
    calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-pkg"),
    calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-cores"),
    calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-gpu"),
    calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-ram").

calcAverageType({M,F,_,O},ResultOutput,AvgOutput,Method,Type) ->
    A = average(lists:flatten(dets:match(ResultOutput,{{M,F,O},Method,Type,'$1'})),1), 
    if
        A < 0 -> nok;
        true -> dets:insert(AvgOutput,{{{M,F,O},Method,Type},A})
    end.

average(L,K) when length(L) > 2*K -> %Second parameter: How many best and worst to disregard
    S = lists:nthtail(K,lists:reverse(lists:nthtail(K,lists:sort(L)))),
    lists:sum(S)/length(S);
average(_,_) -> -1.

averageToFile(FileName,LogFileName) ->
    dets:open_file(FileName,[{type,set}]),
    {ok, File} = file:open(LogFileName,[write]),
    Fun = fun({{{M,F,O},Method,Type},Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Value]), continue end,
    dets:traverse(FileName, Fun),
    dets:close(FileName).

detsToFiles([FileName | FileNames]) ->
    detsToFile(FileName),
    detsToFiles(FileNames);

detsToFiles([]) -> ok.

detsToFile(FileName) ->
    dets:open_file(FileName,[{type,duplicate_bag}]),
    LogFileName = FileName ++ ".csv",
    {ok, File} = file:open(LogFileName,[write]),
    Fun = fun({{M,F,O},Method,Type,Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Value]), continue end,
    dets:traverse(FileName, Fun),
    dets:close(FileName).