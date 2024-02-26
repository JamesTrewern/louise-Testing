:- module(primitives,
          [ maze/3,
            step_down/2,
            step_left/2,
            step_right/2,
            step_up/2
          ]).

:- (discontiguous maze/3).
:- (discontiguous step_down/2).
:- (discontiguous step_up/2).
:- (discontiguous step_left/2).
:- (discontiguous step_right/2).

:- (multifile maze/3).

%Maze zero
 maze(zero, 2-2, [[f, f], [f, f]]).

%Primitive moves for maze zero
step_down([zero, 0/1, f], [zero, 0/0, f]).
step_down([zero, 1/1, f], [zero, 1/0, f]).
step_left([zero, 1/0, f], [zero, 0/0, f]).
step_left([zero, 1/1, f], [zero, 0/1, f]).
step_right([zero, 0/0, f], [zero, 1/0, f]).
step_right([zero, 0/1, f], [zero, 1/1, f]).
step_up([zero, 0/0, f], [zero, 0/1, f]).
step_up([zero, 1/0, f], [zero, 1/1, f]).

