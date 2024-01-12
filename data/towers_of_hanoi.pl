:-module(toh, [
    background_knowledge/2,
    metarules/2,
    positive_example/2,
    negative_example/2,
    toh/2,
    move01/2,
    move02/2,
    move10/2,
    move12/2,
    move20/2,
    move21/2
]).

:-use_module(configuration).

background_knowledge(toh/2,[
    move01/2,
    move02/2,
    move10/2,
    move12/2,
    move20/2,
    move21/2
    % unify/2
    ]).

configuration:toh_1 metarule 'P(x,y):- Q(x,z), R(z,y)'.
configuration:toh_2 metarule 'P(x,y):- R(x,y)'.
metarules(toh/2,[toh_1,toh_2]).


toh([[1,2,3],[],[]],[[],[],[1,2,3]]).
positive_example(toh/2,toh(A,B)):-
	toh(A,B).

negative_example(toh/2,toh(A,B)):-
	toh(B,A).

replace( I, X, [H|T],[H|R]):- 
    I > 0,
    I1 is I-1,
    replace(I1, X, T, R).
replace(0, X, [_|T], [X|T]).

can_move([_|_],[]).
can_move([H1|_],[H2,_]):- H1 < H2.
move(Towers1,I1,I2,Towers3):-
    number(I1),
    number(I2),
    nth0(I1,Towers1, Tower1),
    nth0(I2,Towers1, Tower2),
    can_move(Tower1,Tower2),
    [Ring|Tower1_2] = Tower1,
    Tower2_2 = [Ring|Tower2],
    replace(I1,Tower1_2,Towers1,Towers2),
    replace(I2,Tower2_2, Towers2,Towers3).
move01(T1,T2):-
    move(T1,0,1,T2).
move02(T1,T2):-
    move(T1,0,2,T2).
move10(T1,T2):-
    move(T1,1,0,T2).
move12(T1,T2):-
    move(T1,1,2,T2).
move20(T1,T2):-
    move(T1,2,0,T2).
move21(T1,T2):-
    move(T1,2,1,T2).

% solve(Towers1,Towers2):-
%     solve1(Towers1,Towers2)
% %%multiple clauses
% solve1(T1,T2):-
%     analyse_towers_n(T1),
%     solve_n(T1,T2),
% solve_n(T1,T3):-
%     move(T1,i,i,T2),
%     solve1(T2,T3),
% solve_last_n(T1,T2).