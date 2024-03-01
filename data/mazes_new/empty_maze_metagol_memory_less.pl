% Should both be in the same directory as this file.
:-use_module(metagol).
:-use_module(primitives_memory_less).

metagol:min_clauses(8).
metagol:max_clauses(8).
metagol:max_inv_preds(0).

% Identity
metarule([P,Q], [P,X,Y], [[Q,X,Y]]).
% Tailrec
metarule([P,Q], [P,X,Y], [[Q,X,Z],[P,Z,Y]]).

% Move primitives
body_pred(move_right/2).
body_pred(move_left/2).
body_pred(move_up/2).
body_pred(move_down/2).

% Learn from a generalised example.
% Gets stuck after a while.
learn_maze_solver_non_ground:-
        Pos = [solve([Id,_X1/_Y1,_T1],[Id,_X2/_Y2,_T2])]
        ,Neg = []
        ,time(learn(Pos,Neg)).

% Learn from ground examples.
% Gets stuck after a while.
learn_maze_solver_ground:-
        Pos = [solve([zero, 0/1, f], [zero, 0/0, f])
              ,solve([zero, 1/1, f], [zero, 1/0, f])
              ,solve([zero, 1/0, f], [zero, 0/0, f])
              ,solve([zero, 1/1, f], [zero, 0/1, f])
              ,solve([zero, 0/0, f], [zero, 1/0, f])
              ,solve([zero, 0/1, f], [zero, 1/1, f])
              ,solve([zero, 0/0, f], [zero, 0/1, f])
              ,solve([zero, 1/0, f], [zero, 1/1, f])
              ]
        ,Neg = []
        ,time(learn(Pos,Neg)).


%!      move_down(+State1,-State2) is det.
%
%       Move the robot one step down.
%
%
move_down(S1,S2):-
    step_down(S1,S2).


%!      move_left(+State1,-State2) is det.
%
%       Move the robot one step left.
%
%
move_left(S1,S2):-
    step_left(S1,S2).


%!      move_right(+State1,-State2) is det.
%
%       Move the robot one step right.
%
%
move_right(S1,S2):-
    step_right(S1,S2).


%!      move_up(+State1,-State2) is det.
%
%       Move the robot one step up.
%
%
move_up(S1,S2):-
    step_up(S1,S2).
