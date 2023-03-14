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
    InputDesFile = "\"" ++ ResultPath ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++
                    "_" ++ integer_to_list(InputDesc) ++ "_" ++ integer_to_list(Count) ++ ".json"++ "\"",
    io:format("~nCurrently measuring functions with input desctription ~p~n",[InputDesc]),
    Command = "scaphandre json -s 0 -n 100000000 -f " ++ InputDesFile,
    Output = os:cmd("wmic process call create \"cmd /C start /B"++ Command ++"\" | find \"ProcessId\""),
    {match, [PidString]} = re:run(Output, "ProcessId = ([0-9]+)", [{capture, all_but_first, list}]),
    Pid = list_to_integer(PidString),
    io:format("OS PID: ~p~n", [Pid]),
    % io:format("~nCommand: ~p~n",[Command]),
    % Pid = spawn(fun() -> os:cmd(Command) end),
    % Info = erlang:process_info(Pid),
    % io:format("Info: ~p~n", [Info]),
    % ProcessId = element(2, Info),
    % io:format("Process Id: ~p~n", [ProcessId]),
    % Pid = spawn(fun() -> os:cmd(Command) end),
    % timer:sleep(1000),
    Started_At = erlang:system_time(seconds),
    io:format("Time started at: ~p~n", [Started_At]),
    Before = erlang:system_time(1000000),
    Me = self(),
    measureFunction({Module,Function,Attributes,InputDesc}, Count , Me, InputDesFile),
    receive
        stop ->  
            Command1 = "taskkill /F /PID " ++ integer_to_list(Pid),
            os:cmd(Command1),
        % stop -> erlang:exit(Pid,kill), 
                % ProcessId = <process id>,
                % Command_kill = "taskkill /F /PID " ++ (ProcessId),
                % Command_kill = "taskkill /F /PID " ++ (Pid),
                % os:cmd(Command_kill),
        After = erlang:system_time(1000000),
        Time = (After - Before)/1000000,
        % Finished_At = erlang:system_time(seconds),
        % io:format("Time finished: ~p~n", [Finished_At]),
        io:format("Time elapsed: ~p~n", [Time]),
        % io:format("Is the process with PID ~p alive? ~p~n", [Pid, erlang:is_process_alive(Pid)]),
        % Ns = erlang:monotonic_time(),
        % io:format("Current time in nanoseconds: ~p~n", [Ns]),
        io:format("~n killed ~n")
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
    % Ns = erlang:monotonic_time(),
    % io:format("Finished time in nanoseconds: ~p~n", [Ns]),
    % timer:sleep(1000),
    Me ! stop;
    % timer:sleep(2000);

measureFunction({M,F,A,O}, Count, Me, InputDesFile) ->
    process_flag(trap_exit, true),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
    apply(M, F, A),
    measureFunction({M,F,A,O},Count-1, Me, InputDesFile).
% measureFunction({M,F,A=[H|_]}, 0, ResultOutput, InputDesFile) -> 
%     measureFunction({M,F,A,H}, 0, ResultOutput, InputDesFile);
    
% measureFunction({M,F,A=[H|_]}, Count, ResultOutput, InputDesFile) -> 
%     measureFunction({M,F,A,H}, Count, ResultOutput, InputDesFile);


% % measureFunction({M,F,A,O},0,ResultOutput, InputDesFile) ->
%     % calculateAverage({M,F,A,O},ResultOutput,AvgOutput),
%     % averageToFile(AvgOutput,LogFileName);
% measureFunction({_,_,_,_}, 0, _, _) -> 
%     ok;

% measureFunction({M,F,A,O}, Count, ResultOutput, InputDesFile) ->
%     % process_flag(trap_exit, true),
%     io:format("~n----------------------------------------------------~n"),
%     io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
%     apply(M, F, A),
%     measureFunction({M,F,A,O},Count-1, ResultOutput, InputDesFile).


% calculateAverage(T,ResultOutput,AvgOutput) ->
%     dets:open_file(AvgOutput,[{type,set}]),
%     dets:open_file(ResultOutput,[{type,duplicate_bag}]),
%     calcAverageMethod(T,ResultOutput,AvgOutput,"sysfs"),
%     calcAverageMethod(T,ResultOutput,AvgOutput,"perf_event"),
%     calcAverageMethod(T,ResultOutput,AvgOutput,"msr"),
%     calcAverageMethod(T,ResultOutput,AvgOutput,"time"),
%     dets:close(ResultOutput),
%     dets:close(AvgOutput). 


% calcAverageMethod(T,ResultOutput,AvgOutput,"time") ->
%     calcAverageType(T,ResultOutput,AvgOutput,"time","time");
% calcAverageMethod(T,ResultOutput,AvgOutput,Method) ->
%     calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-pkg"),
%     calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-cores"),
%     calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-gpu"),
%     calcAverageType(T,ResultOutput,AvgOutput,Method,"energy-ram").

% calcAverageType({M,F,_,O},ResultOutput,AvgOutput,Method,Type) ->
%     A = average(lists:flatten(dets:match(ResultOutput,{{M,F,O},Method,Type,'$1'})),1), 
%     if
%         A < 0 -> nok;
%         true -> dets:insert(AvgOutput,{{{M,F,O},Method,Type},A})
%     end.

% average(L,K) when length(L) > 2*K -> %Second parameter: How many best and worst to disregard
%     S = lists:nthtail(K,lists:reverse(lists:nthtail(K,lists:sort(L)))),
%     lists:sum(S)/length(S);
% average(_,_) -> -1.

% averageToFile(FileName,LogFileName) ->
%     dets:open_file(FileName,[{type,set}]),
%     {ok, File} = file:open(LogFileName,[write]),
%     Fun = fun({{{M,F,O},Method,Type},Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Value]), continue end,
%     dets:traverse(FileName, Fun),
%     dets:close(FileName).

% detsToFiles([FileName | FileNames]) ->
%     detsToFile(FileName),
%     detsToFiles(FileNames);

% detsToFiles([]) -> ok.

% detsToFile(FileName) ->
%     dets:open_file(FileName,[{type,duplicate_bag}]),
%     LogFileName = FileName ++ ".csv",
%     {ok, File} = file:open(LogFileName,[write]),
%     Fun = fun({{M,F,O},Method,Type,Value}) -> io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M,F,O,Method,Type,Value]), continue end,
%     dets:traverse(FileName, Fun),
%     dets:close(FileName).