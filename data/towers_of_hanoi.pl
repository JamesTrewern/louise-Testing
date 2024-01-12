:-module(hanoi, [background_knowledge/2
                ,metarules/2
                ,positive_example/2
                ,negative_example/2
                ,move_12/2
                ,move_13/2
                ,move_21/2
                ,move_23/2
                ,move_31/2
                ,move_32/2
                ,can_move/2]).


:- auxiliaries:set_configuration_option(clause_limit, [4]).
:- auxiliaries:set_configuration_option(max_invented, [0]).
configuration:metarule_constraints(M,fail):- M =.. [m,_,P,P|_Ps].

background_knowledge(toh/2, [move_12/2
                            ,move_13/2
                            ,move_21/2
                            ,move_23/2
                            ,move_31/2
                            ,move_32/2
                            ]).

metarules(toh/2,[chain,identity]).

positive_example(toh/2, toh(s([1,2,3],[],[]),s([],[],[1,2,3])) ).

negative_example(toh/2,_):- false.


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

% toh(A,B):-move_12(A,C),move_23(C,B).
% toh(A,B):-move_12(A,C),toh(C,B).
% toh(A,B):-move_23(A,C),toh(C,B).
% toh(A,B):-move_31(A,C),toh(C,B).

% toh(A,B):-move_13(A,C),toh(C,B).
% toh(A,B):-move_21(A,C),toh(C,B).
% toh(A,B):-move_31(A,C),move_13(C,B).
% toh(A,B):-move_32(A,C),toh(C,B).

% toh(A,B):-move_12(A,C),toh(C,B).
% toh(A,B):-move_23(A,C),toh(C,B).
% toh(A,B):-move_31(A,C),toh(C,B).
% toh(A,B):-move_32(A,C),move_23(C,B).

% toh(A,B):-move_13(A,C),toh(C,B).
% toh(A,B):-move_21(A,C),toh(C,B).
% toh(A,B):-move_31(A,C),move_13(C,B).
% toh(A,B):-move_32(A,C),toh(C,B).