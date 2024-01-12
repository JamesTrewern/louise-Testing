:- use_module(lib(mathemancy/mathemancy))

can_move(_,[]).
can_move(D1,[D2|_]):- D1 < D2.

% Move top disk from pole 1
move_12(s([D|T1],T2,T3),s(T1,[D|T2],T3)):- can_move(D,T2).
move_13(s([D|T1],T2,T3),s(T1,T2,[D|T3])):- can_move(D,T3).
% Move top disk from pole 2
move_21(s(T1,[D|T2],T3),s([D|T1],T2,T3)):- can_move(D,T1).
move_23(s(T1,[D|T2],T3),s(T1,T2,[D|T3])):- can_move(D,T3).
% Move top disk from pole 3
move_31(s(T1,T2,[D|T3]),s([D|T1],T2,T3)):- can_move(D,T1).
move_32(s(T1,T2,[D|T3]),s(T1,[D|T2],T3)):- can_move(D,T2).

:-table toh/2.

toh(A,B):-move_12(A,C),move_23(C,B).
toh(A,B):-move_13(A,C),toh(C,B).
toh(A,B):-move_21(A,C),toh(C,B).
toh(A,B):-move_32(A,C),toh(C,B).

test:-
    