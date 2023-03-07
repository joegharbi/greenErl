-module(energy_consumption_res).
-export([measure/2]).

measure({Module, Functions, [InputDesc|InputDescs]}, Count) -> 
    IsInputDescList = isList(InputDesc),
    if
        IsInputDescList -> 
            IsGenerateInputAvailable = lists:member({generate_input,length(InputDesc)},
                                        apply(Module, module_info, [exports])),
            if
                IsGenerateInputAvailable ->
                    io:format("~nGenerating input for description ~p~n", [InputDesc]),
                    ArgList = apply(Module, generate_input, InputDesc),
                    measureFunctions({Module, Functions, ArgList, InputDesc}, Count);
                true ->
                    io:format("~nNo generate_input/~p function available~n", [length(InputDesc)]),
                    ArgList = InputDesc,
                    measureFunctions({Module, Functions, ArgList}, Count)
            end;
        true ->
            IsGenerateInputAvailable = lists:member({generate_input,1}, apply(Module, module_info, [exports])),
            if
                IsGenerateInputAvailable ->
                    io:format("~nGenerating input for description ~p~n", [InputDesc]),
                    ArgList = apply(Module, generate_input, [InputDesc]),
                    measureFunctions({Module, Functions, ArgList, InputDesc}, Count);
                true ->
                    io:format("~nNo generate_input/1 function available~n", []),
                    ArgList = [InputDesc],
                    measureFunctions({Module, Functions, ArgList}, Count)
            end
    end,
    measure({Module, Functions, InputDescs}, Count);
measure({_, _, []}, _) -> 
    io:format("~nMeasurements finished succesfully~n", []),
    ok.

isList([]) -> true;
isList([_|_]) -> true;
isList(_) -> false.

measureFunctions({Module, Functions, Attributes=[H|_]}, Count) -> 
    measureFunctions( {Module, Functions, Attributes, H}, Count);
measureFunctions({Module, all, Attributes, InputDesc}, Count) ->
    Functions = lists:map(fun({FunctionName, _}) -> 
                    FunctionName end, 
    lists:filter(fun({FunctionName, Arity}) ->
                Arity == length(Attributes) 
                            andalso FunctionName /= module_info 
                            andalso FunctionName /= generate_input end,
        apply(Module, module_info, [exports]))),   
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count);
measureFunctions({Module, [Function|Functions], Attributes, InputDesc}, Count) ->
    io:format("~nCurrently measuring functions with input desctription ~p~n",[InputDesc]),
    measureFunction({Module,Function,Attributes,InputDesc}, Count),
    measureFunctions({Module, Functions, Attributes, InputDesc}, Count);
measureFunctions({_, [], _, _}, _) -> ok.

measureFunction({_,_,_,_}, 0) -> ok;
measureFunction({M,F,A=[H|_]}, 0) -> 
    measureFunction({M,F,A,H}, 0);
measureFunction({M,F,A=[H|_]}, Count ) -> 
    measureFunction({M,F,A,H}, Count);
measureFunction({M,F,A,O}, Count ) ->
    process_flag(trap_exit, true),
    io:format("~n----------------------------------------------------~n"),
    io:format("Starting measurement for ~p:~p~nMeasurements left for this funcion for this input: ~p~n", [M, F, Count]),
    apply(M, F, A),
    measureFunction({M,F,A,O},Count-1).