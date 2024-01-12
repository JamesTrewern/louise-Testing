:-module(toh, [background_knowledge/2
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

:-use_module(configuration).
:- auxiliaries:set_configuration_option(clause_limit, [3]).
:- auxiliaries:set_configuration_option(max_invented, [0]).

background_knowledge(toh/2,[
    move_12/2
    ,move_13/2
    ,move_21/2
    ,move_23/2
    ,move_31/2
    ,move_32/2
]).

metarules(toh/2,[chain,identity]).


toh(([1,2,3],[],[]),[[],[],[1,2,3]]).


positive_example(toh/2, toh(s([1,2,3],[],[]),s([],[],[1,2,3])) ).
negative_example(toh/2,_):- false.

replace( I, X, [H|T],[H|R]):- 
    I > 0,
    I1 is I-1,
    replace(I1, X, T, R).
replace(0, X, [_|T], [X|T]).

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

% toh(X,Y):-
%     move02(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move01(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move21(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move02(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move10(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move12(X,Z),
%     toh(Z,Y).
% toh(X,Y):-
%     move02(X,Z),

% move(Towers1,I1,I2,Towers3):-
%     number(I1),
%     number(I2),
%     nth0(I1,Towers1, Tower1),
%     nth0(I2,Towers1, Tower2),
%     can_move(Tower1,Tower2),
%     [Ring|Tower1_2] = Tower1,
%     Tower2_2 = [Ring|Tower2],
%     replace(I1,Tower1_2,Towers1,Towers2),
%     replace(I2,Tower2_2, Towers2,Towers3).


% move01(T1,T2):-
%     move(T1,0,1,T2).
% move02(T1,T2):-
%     move(T1,0,2,T2).
% move10(T1,T2):-
%     move(T1,1,0,T2).
% move12(T1,T2):-
%     move(T1,1,2,T2).
% move20(T1,T2):-
%     move(T1,2,0,T2).
% move21(T1,T2):-
%     move(T1,2,1,T2).

% debug(learn), debug(top_program)