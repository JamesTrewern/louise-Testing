% Should both be in the same directory as this file.
:-use_module(metagol).
:-use_module(primitives).

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
        Pos = [solve([Id,_X1/_Y1,_T1,[]],[Id,_X2/_Y2,_T2,_Vs])]
        ,Neg = []
        ,time(learn(Pos,Neg)).

% Learn from ground examples.
% Gets stuck after a while.
learn_maze_solver_ground:-
        Pos = [solve([zero, 0/1, f, []], [zero, 0/0, f, _])
              ,solve([zero, 1/1, f, []], [zero, 1/0, f, _])
              ,solve([zero, 1/0, f, []], [zero, 0/0, f, _])
              ,solve([zero, 1/1, f, []], [zero, 0/1, f, _])
              ,solve([zero, 0/0, f, []], [zero, 1/0, f, _])
              ,solve([zero, 0/1, f, []], [zero, 1/1, f, _])
              ,solve([zero, 0/0, f, []], [zero, 0/1, f, _])
              ,solve([zero, 1/0, f, []], [zero, 1/1, f, _])
              ]
        ,Neg = []
        ,time(learn(Pos,Neg)).


%!      move_down(+State1,-State2) is det.
%
%       Move the robot one step down.
%
%
move_down([Id,X/Y,T,[]],S2):-
    step_down([Id,X/Y,T,[]],S2).
move_down([Id,X/Y,T,[V|Vs]],S2):-
    V \== u,
    step_down([Id,X/Y,T,[V|Vs]],S2).


%!      move_left(+State1,-State2) is det.
%
%       Move the robot one step left.
%
%
move_left([Id,X/Y,T,[]],S2):-
    step_left([Id,X/Y,T,[]],S2).
move_left([Id,X/Y,T,[V|Vs]],S2):-
    V \== r,
    step_left([Id,X/Y,T,[V|Vs]],S2).


%!      move_right(+State1,-State2) is det.
%
%       Move the robot one step right.
%
%
move_right([Id,X/Y,T,[]],S2):-
    step_right([Id,X/Y,T,[]],S2).
move_right([Id,X/Y,T,[V|Vs]],S2):-
    V \== l,
    step_right([Id,X/Y,T,[V|Vs]],S2).


%!      move_up(+State1,-State2) is det.
%
%       Move the robot one step up.
%
%
move_up([Id,X/Y,T,[]],S2):-
    step_up([Id,X/Y,T,[]],S2).
move_up([Id,X/Y,T,[V|Vs]],S2):-
    V \== d,
    step_up([Id,X/Y,T,[V|Vs]],S2).
