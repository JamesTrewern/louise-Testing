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

:- use_module(lib(mathemancy/mathemancy)).
:- auxiliaries:set_configuration_option(clause_limit, [7]).
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
                                                                            
positive_example(toh/2, Example):-
    member(N,[3,4]),
    example(N,Example).

negative_example(toh/2,_):- false.


can_move(_,[]).
can_move(D1,[D2|_]):- D1 < D2.

% Move top disk from pole 1
move_12(s(t([D|T1],T2,T3),M),s(t(T1,[D|T2],T3),[[1,2]|M])):- can_move(D,T2).
move_13(s(t([D|T1],T2,T3),M),s(t(T1,T2,[D|T3]),[[1,3]|M])):- can_move(D,T3).
% Move top disk from pole 2
move_21(s(t(T1,[D|T2],T3),M),s(t([D|T1],T2,T3),[[2,1]|M])):- can_move(D,T1).
move_23(s(t(T1,[D|T2],T3),M),s(t(T1,T2,[D|T3]),[[2,3]|M])):- can_move(D,T3).
% Move top disk from pole 3
move_31(s(t(T1,T2,[D|T3]),M),s(t([D|T1],T2,T3),[[3,1]|M])):- can_move(D,T1).
move_32(s(t(T1,T2,[D|T3]),M),s(t(T1,[D|T2],T3),[[3,2]|M])):- can_move(D,T2).

:-table toh/2.

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

moves(N,Moves):-
    move(N,1,3,2,[],Moves).
move(1,X,Y,_,M1,[[X,Y]|M1]).
move(N,X,Y,Z,M1,M4) :-
   N>1,
   M is N-1,
   move(M,X,Z,Y,M1,M2),
   move(1,X,Y,_,M2,M3),
   move(M,Z,Y,X,M3,M4).

example(N, toh(s(t(T,[],[]),[]), s(t([],[],T),Moves))):-
    moves(N,Moves),
    series(1, N, T).