-module(nbody_cpu).

-compile(export_all).

list_from_binary(<<>>) ->
    [];
list_from_binary(Binary) ->
    <<X/float>> = binary:part(Binary, {0,8}),
    [X | list_from_binary(binary:part(Binary, {8, byte_size(Binary)-8}))].

get_tuple_list([]) ->
    [];
get_tuple_list(Data) ->
    {Raw, Rest} = lists:split(8, Data),
    Comp = list_to_tuple(Raw),
    [Comp | get_tuple_list(Rest)].

add_to_acc_list (OrigPart, Sum, Part) ->
    {Sum1,Sum2,Sum3} = Sum,
    {X1,Y1,Z1,_,_,_,_,_} = OrigPart,
    {X2,Y2,Z2,M,_,_,_,_} = Part,
    Diff = {X2-X1,Y2-Y1,Z2-Z1},
    {Dx,Dy,Dz} = Diff,
    Invr = 1 / math:sqrt(Dx*Dx+Dy*Dy*+Dz*Dz+0.0001),
    Invr3 = M*Invr*Invr*Invr,
    {Sum1+Invr3*Dx, Sum2+Invr3*Dy, Sum3+Invr3*Dz}.

calc_acc_vector_list (Part,Particles) ->
    lists:foldl(fun(X,Sum) -> add_to_acc_list(X,Sum,Part) end, {0,0,0}, Particles).

calc_nbody_list(Part, Particles,Dt) ->
    {Ax,Ay,Az} = calc_acc_vector_list (Part, Particles),
    {X,Y,Z,M,Vx,Vy,Vz,_} = Part,
    Xnew = X + Dt*Vx + 0.5*Dt*Dt*Ax,
    Ynew = Y + Dt*Vy + 0.5*Dt*Dt*Ay,
    Znew = Z + Dt*Vz + 0.5*Dt*Dt*Az,
    Vxnew = Vx + Dt*Ax,
    Vynew = Vy + Dt*Ay,
    Vznew = Vz + Dt*Az,
    {Xnew,Ynew,Znew,M,Vxnew,Vynew,Vznew,0}.
    
nbody_cpu(Chunk, Particles,Dt) ->
    lists:map (fun(X) -> calc_nbody_list (X,Particles,Dt) end, Chunk).

