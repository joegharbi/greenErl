-module(energy_consumption).

-export([measure/4, detsToFile/1, detsToFiles/1, measureFunction/6]).

% Measure multiple functions for multiple inputs
% If generate_input is available in the measured module, use it to generate input argument lists,
% Otherwise InputDesc is treated as an argument list.
measure(Prog, {Module, Functions, [InputDesc | InputDescs]}, Count, ResultPath) ->
    IsInputDescList = isList(InputDesc),
    if IsInputDescList ->
           IsGenerateInputAvailable =
               lists:member({generate_input, length(InputDesc)},
                            apply(Module, module_info, [exports])),
           if IsGenerateInputAvailable ->
                  io:format("~nGenerating input for description ~p~n", [InputDesc]),
                  ArgList = apply(Module, generate_input, InputDesc),
                  measureFunctions(Prog,
                                   {Module, Functions, ArgList, InputDesc},
                                   Count,
                                   ResultPath);
              true ->
                  io:format("~nNo generate_input/~p function available~n", [length(InputDesc)]),
                  ArgList = InputDesc,
                  measureFunctions(Prog, {Module, Functions, ArgList}, Count, ResultPath)
           end;
       true ->
           IsGenerateInputAvailable =
               lists:member({generate_input, 1}, apply(Module, module_info, [exports])),
           if IsGenerateInputAvailable ->
                  io:format("~nGenerating input for description ~p~n", [InputDesc]),
                  ArgList = apply(Module, generate_input, [InputDesc]),
                  measureFunctions(Prog,
                                   {Module, Functions, ArgList, InputDesc},
                                   Count,
                                   ResultPath);
              true ->
                  io:format("~nNo generate_input/1 function available~n", []),
                  ArgList = [InputDesc],
                  measureFunctions(Prog, {Module, Functions, ArgList}, Count, ResultPath)
           end
    end,
    measure(Prog, {Module, Functions, InputDescs}, Count, ResultPath);
measure(_, {_, _, []}, _, _) ->
    io:format("~nMeasurements finished succesfully~n", []),
    ok.

isList([]) ->
    true;
isList([_ | _]) ->
    true;
isList(_) ->
    false.

% This function is only for convenience, if no InputDesc is given, we take the head of Attributes and use it as one, for logging.
measureFunctions(Prog, {Module, Functions, Attributes = [H | _]}, Count, ResultPath) ->
    measureFunctions(Prog, {Module, Functions, Attributes, H}, Count, ResultPath);
% Measure multiple functions from the same module, with a given input, and InputDesc
measureFunctions(Prog, {Module, all, Attributes, InputDesc}, Count, ResultPath) ->
    Functions =
        lists:map(fun({FunctionName, _}) -> FunctionName end,
                  lists:filter(fun({FunctionName, Arity}) ->
                                  Arity == length(Attributes)
                                  andalso FunctionName /= module_info
                                  andalso FunctionName /= generate_input
                               end,
                               apply(Module, module_info, [exports]))),
    measureFunctions(Prog, {Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions(Prog,
                 {Module, [Function | Functions], Attributes, InputDesc},
                 Count,
                 ResultPath) ->
    ResultOutput =
        ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_result",
    AvgOutput =
        ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_avg",
    LogFileName =
        ResultPath ++ "/" ++ atom_to_list(Module) ++ "_" ++ atom_to_list(Function) ++ "_log.csv",
    io:format("~nCurrently measuring functions with input desctription ~p~n", [InputDesc]),
    measureFunction(Prog,
                    {Module, Function, Attributes, InputDesc},
                    Count,
                    ResultOutput,
                    AvgOutput,
                    LogFileName),
    measureFunctions(Prog, {Module, Functions, Attributes, InputDesc}, Count, ResultPath);
measureFunctions(_, {_, [], _, _}, _, _) ->
    ok.

% Measure function for measurement of a single function, with a single input, multiple times
% Parameters:
%  Prog: path to rapl-read.out program
%  {M, F, A}: Module, Function and Attributes to be measured
%   if a 4th element is given in the tuple it will be used to describe the size of the input, otherwise
%   the head of the argument list is used
%  Count: how many times to measure
%  ResultOutput: path to the dets file where we want to store the raw results
%  AvgOutput: path to the dets file where we want to store the averege of the results
%  LogFileName: path to the .csv file where results are logged
measureFunction(Prog, {M, F, A = [H | _]}, 0, ResultOutput, AvgOutput, LogFileName) ->
    measureFunction(Prog, {M, F, A, H}, 0, ResultOutput, AvgOutput, LogFileName);
measureFunction(Prog, {M, F, A = [H | _]}, Count, ResultOutput, AvgOutput, LogFileName) ->
    measureFunction(Prog, {M, F, A, H}, Count, ResultOutput, AvgOutput, LogFileName);
measureFunction(_, {M, F, A, O}, 0, ResultOutput, AvgOutput, LogFileName) ->
    calculateAverage({M, F, A, O}, ResultOutput, AvgOutput),
    averageToFile(AvgOutput, LogFileName);
measureFunction(Prog, {M, F, A, O}, Count, ResultOutput, AvgOutput, LogFileName) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, Prog}, [{packet, 2}]),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this "
              "funcion for this input: ~p~n",
              [M, F, Count]),
    io:format("Port: ~p~n", [Port]),
    test(Port, {M, F, A, O}, ResultOutput),
    measureFunction(Prog, {M, F, A, O}, Count - 1, ResultOutput, AvgOutput, LogFileName).

test(Port, {M, F, A, O}, Output) ->
    receive
        {Port, {data, [1]}} -> %Rapl-read is ready to initialize measurement
            Port ! {self(), {command, [1]}}, %Rapl-read starts measuring
            try test1(Port, {M, F, A, O}, Output) of
                _ ->
                    ok
            catch
                no_init ->
                    io:format("Failed: not ready initializing~n");
                not_received ->
                    io:format("Failed: measurement not received~n")
            end;
        X ->
            io:format("~p~n", [X])
    after 5000 ->
        io:format("Failed: not ready to measure~n")
    end,
    close(Port).

close(Port) ->
    Port ! {self(), close},
    receive
        {Port, closed} ->
            io:format("~p~n", [{Port, closed}]),
            ok;
        {'EXIT', Port, Reason} ->
            io:format("~p~n", [{'EXIT', Port, Reason}])
    after 2000 ->
        nok
    end,
    flush().

test1(Port, T = {M, F, A, _}, FileName) ->
    receive
        {Port, {data, [2]}} -> %Rapl-read is ready initializing measurement
            Before = erlang:system_time(1000000),
            apply(M, F, A),
            After = erlang:system_time(1000000),
            Port ! {self(), {command, [2]}}, %Rapl-read stops measuring.
            Time = (After - Before) / 1000000,
            receive_measurement(Port, FileName, T, Time);
        X ->
            io:format("~p~n", [X])
    after 5000 ->
        throw(no_init)
    end.

flush() ->
    receive
        A ->
            io:format("Messages: ~p~n", [A]),
            flush()
    after 0 ->
        ok
    end.

receive_measurement(Port, FileName, Input = {M, F, _, O}, Time) ->
    dets:open_file(FileName, [{type, duplicate_bag}]),
    dets:insert(FileName, {{M, F, O}, "time", "time", Time}),
    receive
        {Port, {data, [N]}} ->
            receive_measurement_method(Port, FileName, Input, N) %Receive number of measurements
    after 5000 ->
        throw(not_received)
    end,
    dets:close(FileName).

receive_measurement_method(_, _, _, 0) ->
    ok;
receive_measurement_method(Port, FileName, Input, N) ->
    receive
        {Port, {data, Method}} ->  %Receive method of measurement, eg.: msr, sysfs...
            receive_measurement_type_count(Port, FileName, Input, Method),
            receive_measurement_method(Port, FileName, Input, N - 1)
    after 5000 ->
        throw(not_received)
    end.

receive_measurement_type_count(Port, FileName, Input, Method) ->
    receive
        {Port, {data, [N]}} ->
            receive_measurement_type(Port, FileName, Input, Method, N)
    after 5000 ->
        throw(not_received)
    end.

receive_measurement_type(_, _, _, _, 0) ->
    ok;
receive_measurement_type(Port, FileName, Input, Method, N) ->
    receive
        {Port, {data, Case}} ->  %Receive case of measurement, eg.: uncore, dram...
            receive_measurement_value(Port, FileName, Input, Method, Case),
            receive_measurement_type(Port, FileName, Input, Method, N - 1)
    after 5000 ->
        throw(not_received)
    end.

receive_measurement_value(Port, FileName, {M, F, _, O}, Method, Case) ->
    receive
        {Port, {data, Value}} ->  %Receive value of measurement
            <<X/float>> =
                erlang:list_to_binary(
                    lists:reverse(Value)),
            dets:insert(FileName, {{M, F, O}, Method, Case, X})
    after 5000 ->
        throw(not_received)
    end.

calculateAverage(T, ResultOutput, AvgOutput) ->
    dets:open_file(AvgOutput, [{type, set}]),
    dets:open_file(ResultOutput, [{type, duplicate_bag}]),
    calcAverageMethod(T, ResultOutput, AvgOutput, "sysfs"),
    calcAverageMethod(T, ResultOutput, AvgOutput, "perf_event"),
    calcAverageMethod(T, ResultOutput, AvgOutput, "msr"),
    calcAverageMethod(T, ResultOutput, AvgOutput, "time"),
    dets:close(ResultOutput),
    dets:close(AvgOutput).

calcAverageMethod(T, ResultOutput, AvgOutput, "time") ->
    calcAverageType(T, ResultOutput, AvgOutput, "time", "time");
calcAverageMethod(T, ResultOutput, AvgOutput, Method) ->
    calcAverageType(T, ResultOutput, AvgOutput, Method, "energy-pkg"),
    calcAverageType(T, ResultOutput, AvgOutput, Method, "energy-cores"),
    calcAverageType(T, ResultOutput, AvgOutput, Method, "energy-gpu"),
    calcAverageType(T, ResultOutput, AvgOutput, Method, "energy-ram").

calcAverageType({M, F, _, O}, ResultOutput, AvgOutput, Method, Type) ->
    A = average(lists:flatten(
                    dets:match(ResultOutput, {{M, F, O}, Method, Type, '$1'})),
                1),
    if A < 0 ->
           nok;
       true ->
           dets:insert(AvgOutput, {{{M, F, O}, Method, Type}, A})
    end.

average(L, K)
    when length(L) > 2 * K -> %Second parameter: How many best and worst to disregard
    S = lists:nthtail(K,
                      lists:reverse(
                          lists:nthtail(K, lists:sort(L)))),
    lists:sum(S) / length(S);
average(_, _) ->
    -1.

averageToFile(FileName, LogFileName) ->
    dets:open_file(FileName, [{type, set}]),
    {ok, File} = file:open(LogFileName, [write]),
    Fun = fun({{{M, F, O}, Method, Type}, Value}) ->
             io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M, F, O, Method, Type, Value]),
             continue
          end,
    dets:traverse(FileName, Fun),
    dets:close(FileName).

detsToFiles([FileName | FileNames]) ->
    detsToFile(FileName),
    detsToFiles(FileNames);
detsToFiles([]) ->
    ok.

detsToFile(FileName) ->
    dets:open_file(FileName, [{type, duplicate_bag}]),
    LogFileName = FileName ++ ".csv",
    {ok, File} = file:open(LogFileName, [write]),
    Fun = fun({{M, F, O}, Method, Type, Value}) ->
             io:format(File, "~p;~p;~w;~s;~s;~.10f~n", [M, F, O, Method, Type, Value]),
             continue
          end,
    dets:traverse(FileName, Fun),
    dets:close(FileName).
