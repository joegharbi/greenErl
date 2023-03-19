-module(energy_consumption_res).
-export([measure/3]).

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
    % InputDesFile = "\"" ++ ResultPath ++ "M_" ++ atom_to_list(Module) ++ "_F_" ++ atom_to_list(Function) ++
    %                 "_I_" ++ integer_to_list(InputDesc) ++ "_C_" ++ integer_to_list(Count) ++ ".json"++ "\"",
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
    % JsonFile = atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ ".json",
    % {ok, _} = file:open(JsonFile, [write]),
    % FileName = atom_to_list(Module) ++ atom_to_list(Function) ++ integer_to_list(InputDesc) ++ ".csv",
    Val = atom_to_list(Module) ++ ";" ++ atom_to_list(Function)++ ";" ++ integer_to_list(InputDesc) ++ ";" ++
    "time" ++ ";" ++ "time",
    % Started_At = erlang:system_time(seconds),
    % io:format("Time started at: ~p~n", [Started_At]),
    Before = erlang:system_time(1000000),
    measureFunction({Module,Function,Attributes,InputDesc}, Count , Me, InputDesFile),
    receive
        stop ->  
            timer:sleep(1000),
            Command1 = "taskkill /F /PID " ++ integer_to_list(Pid),
            os:cmd(Command1),
            After = erlang:system_time(1000000)-(1000000),
            Time = (After - Before)/1000000,
            Data = Val ++ ";" ++ float_to_list(Time),
            dumpTime(ResultPath,FileName,Data),
            io:format("Time elapsed: ~p~n", [Time]),
            io:format("~n killed ~n"),
            timer:sleep(1000)
        % after 
        %     60000 -> % wait for 60 seconds
    %     exit(Pid,kill)
    end,
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions({_, [], _, _}, _, _) -> ok.



measureFunction({M,F,A=[H|_]}, 0, Me, InputDesFile) -> 
    measureFunction({M,F,A,H}, 0, Me, InputDesFile);
    
measureFunction({M,F,A=[H|_]}, Count, Me, InputDesFile) -> 
    measureFunction({M,F,A,H}, Count, Me, InputDesFile);


% measureFunction({M,F,A,O},0,ResultOutput, InputDesFile) ->
    % calculateAverage({M,F,A,O},ResultOutput,AvgOutput),
    % averageToFile(AvgOutput,LogFileName);
measureFunction({_,_,_,_}, 0, Me, _) -> 
    io:format("~nSending stop~n"),
    Me ! stop;

measureFunction({M,F,A,O}, Count, Me, InputDesFile) ->
    process_flag(trap_exit, true),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
    apply(M, F, A),
    measureFunction({M,F,A,O},Count-1, Me, InputDesFile).


% dumpTime(FileName,Data) ->
%     {ok, File} = file:open(FileName,[append]),
%     io:format(File, "~s~n", [Data]),
%     file:close(File).

dumpTime(FilePath, FileName, Data) ->
    FullPath = filename:join(FilePath, FileName),     
    {ok, File} = file:open(FullPath, [append]),        
    io:format(File, "~s~n", [Data]),                  
    file:close(File).                                 
    
    % Fun = fun({{M,F,O},Method,Type,Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Data]), continue end,
    % Fun = fun({{{M,F,O},Method,Type},Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Value]), continue end,
    % dets:close(FileName).


% dumpTime(FileName, Rows) ->
%     {ok, File} = file:open(FileName, [write]),  % open the file for writing
%     write_rows(File, Rows),                     % write the rows to the file
%     file:close(File).                           % close the file

% write_rows(_, []) ->
%     ok;
% write_rows(File, [Row | Rows]) ->
%     RowStr = string:join([io_lib:format("\"~s\"", [Value]) || Value <- Row], ";"),  % format the row as a CSV string
%     ok = file:write(File, RowStr ++ "\n"),                                          % write the row to the file
%     write_rows(File, Rows).  