-module(energy_consumption_res).
-export([measure/3]).

measure({Module, Functions, [_={V,C}|InputDescs]}, Count, ResultPath) ->
    measure({Module, Functions, [V]}, C, ResultPath),
    measure({Module, Functions, InputDescs}, Count, ResultPath);
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
    filelib:ensure_dir(ResultPath),
    io:format("~n Path: ~p~n",[ResultPath]),
    InputDesFile = "\"" ++ ResultPath  ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++
                    "_" ++ integer_to_list(InputDesc) ++ ".json"++ "\"",
   io:format("~nCurrently measuring functions with input desctription ~p~n",[InputDesc]),
    Command = "scaphandre json -s 0 -n 100000000 -f " ++ InputDesFile,
    Output = os:cmd("wmic process call create \""++ Command ++"\" | find \"ProcessId\""),
    {match, [PidString]} = re:run(Output, "ProcessId = ([0-9]+)", [{capture, all_but_first, list}]),
    Pid = list_to_integer(PidString),
    io:format("OS PID: ~p~n", [Pid]),
    % Sleep for 1 sec the time needed for scaphandre to start dumping values into the json files
    % If not, we will not be able to measure the values for small input size
    timer:sleep(1000),
    Me = self(),
    FileName = atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ ".csv",
    Val = atom_to_list(Module) ++ ";" ++ atom_to_list(Function)++ ";" ++ integer_to_list(InputDesc) ++ ";" ++
    "time" ++ ";" ++ "time",
    Time = measureFunction({Module,Function,Attributes,InputDesc}, Count , Me, InputDesFile, 0),
    receive
        stop ->  
            timer:sleep(1000),
            Command1 = "taskkill /F /PID " ++ integer_to_list(Pid),
            os:cmd(Command1),
            Data = Val ++ ";" ++ float_to_list(Time/1000000),
            dumpTime(ResultPath,FileName,Data),
            io:format("Time elapsed: ~p~n", [Time/1000000]),
            io:format("~n killed ~n"),
            timer:sleep(1000)
        % after 
        %     60000 -> % wait for 60 seconds
    %     exit(Pid,kill)
    end,
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions({_, [], _, _}, _, _) -> ok.



measureFunction({M,F,A=[H|_]}, 0, Me, InputDesFile, Time) -> 
    measureFunction({M,F,A,H}, 0, Me, InputDesFile, Time); 
measureFunction({M,F,A=[H|_]}, Count, Me, InputDesFile,Time) -> 
    measureFunction({M,F,A,H}, Count, Me, InputDesFile, Time);

measureFunction({_,_,_,_}, 0, Me, _, Time) -> 
    io:format("~nSending stop~n"),
    Me ! stop,
    Time;
measureFunction({M,F,A,O}, Count, Me, InputDesFile, Time) ->
    process_flag(trap_exit, true),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
    {NewTime,_} = timer:tc(erlang,apply,[M, F, A]), 
    measureFunction({M,F,A,O},Count-1, Me, InputDesFile, Time+NewTime).

dumpTime(FilePath, FileName, Data) ->
    FullPath = filename:join(FilePath, FileName),     
    {ok, File} = file:open(FullPath, [append]),        
    io:format(File, "~s~n", [Data]),                  
    file:close(File).