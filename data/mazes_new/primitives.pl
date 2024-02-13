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
step_down([zero, 0/1, f, Vs], [zero, 0/0, f, [d|Vs]]).
step_down([zero, 1/1, f, Vs], [zero, 1/0, f, [d|Vs]]).
step_left([zero, 1/0, f, Vs], [zero, 0/0, f, [l|Vs]]).
step_left([zero, 1/1, f, Vs], [zero, 0/1, f, [l|Vs]]).
step_right([zero, 0/0, f, Vs], [zero, 1/0, f, [r|Vs]]).
step_right([zero, 0/1, f, Vs], [zero, 1/1, f, [r|Vs]]).
step_up([zero, 0/0, f, Vs], [zero, 0/1, f, [u|Vs]]).
step_up([zero, 1/0, f, Vs], [zero, 1/1, f, [u|Vs]]).

%Maze tessera_1
 maze(tessera_1,
      7-7,
      [ [s, f, f, w, f, f, f],
        [w, w, f, w, f, w, f],
        [f, w, f, w, f, w, f],
        [f, w, f, w, w, w, f],
        [f, w, f, w, f, f, f],
        [f, w, f, w, f, w, f],
        [f, f, f, f, f, w, e]
      ]).

%Primitive moves for maze tessera_1
step_down([tessera_1, 0/1, f, Vs], [tessera_1, 0/0, f, [d|Vs]]).
step_down([tessera_1, 0/2, f, Vs], [tessera_1, 0/1, f, [d|Vs]]).
step_down([tessera_1, 0/3, f, Vs], [tessera_1, 0/2, f, [d|Vs]]).
step_down([tessera_1, 0/4, f, Vs], [tessera_1, 0/3, f, [d|Vs]]).
step_down([tessera_1, 2/1, f, Vs], [tessera_1, 2/0, f, [d|Vs]]).
step_down([tessera_1, 2/2, f, Vs], [tessera_1, 2/1, f, [d|Vs]]).
step_down([tessera_1, 2/3, f, Vs], [tessera_1, 2/2, f, [d|Vs]]).
step_down([tessera_1, 2/4, f, Vs], [tessera_1, 2/3, f, [d|Vs]]).
step_down([tessera_1, 2/5, f, Vs], [tessera_1, 2/4, f, [d|Vs]]).
step_down([tessera_1, 2/6, f, Vs], [tessera_1, 2/5, f, [d|Vs]]).
step_down([tessera_1, 4/1, f, Vs], [tessera_1, 4/0, f, [d|Vs]]).
step_down([tessera_1, 4/2, f, Vs], [tessera_1, 4/1, f, [d|Vs]]).
step_down([tessera_1, 4/5, f, Vs], [tessera_1, 4/4, f, [d|Vs]]).
step_down([tessera_1, 4/6, f, Vs], [tessera_1, 4/5, f, [d|Vs]]).
step_down([tessera_1, 6/1, f, Vs], [tessera_1, 6/0, e, [d|Vs]]).
step_down([tessera_1, 6/2, f, Vs], [tessera_1, 6/1, f, [d|Vs]]).
step_down([tessera_1, 6/3, f, Vs], [tessera_1, 6/2, f, [d|Vs]]).
step_down([tessera_1, 6/4, f, Vs], [tessera_1, 6/3, f, [d|Vs]]).
step_down([tessera_1, 6/5, f, Vs], [tessera_1, 6/4, f, [d|Vs]]).
step_down([tessera_1, 6/6, f, Vs], [tessera_1, 6/5, f, [d|Vs]]).
step_left([tessera_1, 1/0, f, Vs], [tessera_1, 0/0, f, [l|Vs]]).
step_left([tessera_1, 1/6, f, Vs], [tessera_1, 0/6, s, [l|Vs]]).
step_left([tessera_1, 2/0, f, Vs], [tessera_1, 1/0, f, [l|Vs]]).
step_left([tessera_1, 2/6, f, Vs], [tessera_1, 1/6, f, [l|Vs]]).
step_left([tessera_1, 3/0, f, Vs], [tessera_1, 2/0, f, [l|Vs]]).
step_left([tessera_1, 4/0, f, Vs], [tessera_1, 3/0, f, [l|Vs]]).
step_left([tessera_1, 5/2, f, Vs], [tessera_1, 4/2, f, [l|Vs]]).
step_left([tessera_1, 5/6, f, Vs], [tessera_1, 4/6, f, [l|Vs]]).
step_left([tessera_1, 6/2, f, Vs], [tessera_1, 5/2, f, [l|Vs]]).
step_left([tessera_1, 6/6, f, Vs], [tessera_1, 5/6, f, [l|Vs]]).
step_right([tessera_1, 0/0, f, Vs], [tessera_1, 1/0, f, [r|Vs]]).
step_right([tessera_1, 0/6, s, Vs], [tessera_1, 1/6, f, [r|Vs]]).
step_right([tessera_1, 1/0, f, Vs], [tessera_1, 2/0, f, [r|Vs]]).
step_right([tessera_1, 1/6, f, Vs], [tessera_1, 2/6, f, [r|Vs]]).
step_right([tessera_1, 2/0, f, Vs], [tessera_1, 3/0, f, [r|Vs]]).
step_right([tessera_1, 3/0, f, Vs], [tessera_1, 4/0, f, [r|Vs]]).
step_right([tessera_1, 4/2, f, Vs], [tessera_1, 5/2, f, [r|Vs]]).
step_right([tessera_1, 4/6, f, Vs], [tessera_1, 5/6, f, [r|Vs]]).
step_right([tessera_1, 5/2, f, Vs], [tessera_1, 6/2, f, [r|Vs]]).
step_right([tessera_1, 5/6, f, Vs], [tessera_1, 6/6, f, [r|Vs]]).
step_up([tessera_1, 0/0, f, Vs], [tessera_1, 0/1, f, [u|Vs]]).
step_up([tessera_1, 0/1, f, Vs], [tessera_1, 0/2, f, [u|Vs]]).
step_up([tessera_1, 0/2, f, Vs], [tessera_1, 0/3, f, [u|Vs]]).
step_up([tessera_1, 0/3, f, Vs], [tessera_1, 0/4, f, [u|Vs]]).
step_up([tessera_1, 2/0, f, Vs], [tessera_1, 2/1, f, [u|Vs]]).
step_up([tessera_1, 2/1, f, Vs], [tessera_1, 2/2, f, [u|Vs]]).
step_up([tessera_1, 2/2, f, Vs], [tessera_1, 2/3, f, [u|Vs]]).
step_up([tessera_1, 2/3, f, Vs], [tessera_1, 2/4, f, [u|Vs]]).
step_up([tessera_1, 2/4, f, Vs], [tessera_1, 2/5, f, [u|Vs]]).
step_up([tessera_1, 2/5, f, Vs], [tessera_1, 2/6, f, [u|Vs]]).
step_up([tessera_1, 4/0, f, Vs], [tessera_1, 4/1, f, [u|Vs]]).
step_up([tessera_1, 4/1, f, Vs], [tessera_1, 4/2, f, [u|Vs]]).
step_up([tessera_1, 4/4, f, Vs], [tessera_1, 4/5, f, [u|Vs]]).
step_up([tessera_1, 4/5, f, Vs], [tessera_1, 4/6, f, [u|Vs]]).
step_up([tessera_1, 6/0, e, Vs], [tessera_1, 6/1, f, [u|Vs]]).
step_up([tessera_1, 6/1, f, Vs], [tessera_1, 6/2, f, [u|Vs]]).
step_up([tessera_1, 6/2, f, Vs], [tessera_1, 6/3, f, [u|Vs]]).
step_up([tessera_1, 6/3, f, Vs], [tessera_1, 6/4, f, [u|Vs]]).
step_up([tessera_1, 6/4, f, Vs], [tessera_1, 6/5, f, [u|Vs]]).
step_up([tessera_1, 6/5, f, Vs], [tessera_1, 6/6, f, [u|Vs]]).

%Maze tessera_2
 maze(tessera_2,
      7-7,
      [ [s, f, f, w, f, f, f],
        [w, w, f, w, f, w, f],
        [f, w, f, w, f, w, f],
        [f, w, f, w, w, w, f],
        [f, w, f, w, f, f, f],
        [f, w, f, w, f, w, f],
        [e, f, f, f, f, w, f]
      ]).

%Primitive moves for maze tessera_2
step_down([tessera_2, 0/1, f, Vs], [tessera_2, 0/0, e, [d|Vs]]).
step_down([tessera_2, 0/2, f, Vs], [tessera_2, 0/1, f, [d|Vs]]).
step_down([tessera_2, 0/3, f, Vs], [tessera_2, 0/2, f, [d|Vs]]).
step_down([tessera_2, 0/4, f, Vs], [tessera_2, 0/3, f, [d|Vs]]).
step_down([tessera_2, 2/1, f, Vs], [tessera_2, 2/0, f, [d|Vs]]).
step_down([tessera_2, 2/2, f, Vs], [tessera_2, 2/1, f, [d|Vs]]).
step_down([tessera_2, 2/3, f, Vs], [tessera_2, 2/2, f, [d|Vs]]).
step_down([tessera_2, 2/4, f, Vs], [tessera_2, 2/3, f, [d|Vs]]).
step_down([tessera_2, 2/5, f, Vs], [tessera_2, 2/4, f, [d|Vs]]).
step_down([tessera_2, 2/6, f, Vs], [tessera_2, 2/5, f, [d|Vs]]).
step_down([tessera_2, 4/1, f, Vs], [tessera_2, 4/0, f, [d|Vs]]).
step_down([tessera_2, 4/2, f, Vs], [tessera_2, 4/1, f, [d|Vs]]).
step_down([tessera_2, 4/5, f, Vs], [tessera_2, 4/4, f, [d|Vs]]).
step_down([tessera_2, 4/6, f, Vs], [tessera_2, 4/5, f, [d|Vs]]).
step_down([tessera_2, 6/1, f, Vs], [tessera_2, 6/0, f, [d|Vs]]).
step_down([tessera_2, 6/2, f, Vs], [tessera_2, 6/1, f, [d|Vs]]).
step_down([tessera_2, 6/3, f, Vs], [tessera_2, 6/2, f, [d|Vs]]).
step_down([tessera_2, 6/4, f, Vs], [tessera_2, 6/3, f, [d|Vs]]).
step_down([tessera_2, 6/5, f, Vs], [tessera_2, 6/4, f, [d|Vs]]).
step_down([tessera_2, 6/6, f, Vs], [tessera_2, 6/5, f, [d|Vs]]).
step_left([tessera_2, 1/0, f, Vs], [tessera_2, 0/0, e, [l|Vs]]).
step_left([tessera_2, 1/6, f, Vs], [tessera_2, 0/6, s, [l|Vs]]).
step_left([tessera_2, 2/0, f, Vs], [tessera_2, 1/0, f, [l|Vs]]).
step_left([tessera_2, 2/6, f, Vs], [tessera_2, 1/6, f, [l|Vs]]).
step_left([tessera_2, 3/0, f, Vs], [tessera_2, 2/0, f, [l|Vs]]).
step_left([tessera_2, 4/0, f, Vs], [tessera_2, 3/0, f, [l|Vs]]).
step_left([tessera_2, 5/2, f, Vs], [tessera_2, 4/2, f, [l|Vs]]).
step_left([tessera_2, 5/6, f, Vs], [tessera_2, 4/6, f, [l|Vs]]).
step_left([tessera_2, 6/2, f, Vs], [tessera_2, 5/2, f, [l|Vs]]).
step_left([tessera_2, 6/6, f, Vs], [tessera_2, 5/6, f, [l|Vs]]).
step_right([tessera_2, 0/0, e, Vs], [tessera_2, 1/0, f, [r|Vs]]).
step_right([tessera_2, 0/6, s, Vs], [tessera_2, 1/6, f, [r|Vs]]).
step_right([tessera_2, 1/0, f, Vs], [tessera_2, 2/0, f, [r|Vs]]).
step_right([tessera_2, 1/6, f, Vs], [tessera_2, 2/6, f, [r|Vs]]).
step_right([tessera_2, 2/0, f, Vs], [tessera_2, 3/0, f, [r|Vs]]).
step_right([tessera_2, 3/0, f, Vs], [tessera_2, 4/0, f, [r|Vs]]).
step_right([tessera_2, 4/2, f, Vs], [tessera_2, 5/2, f, [r|Vs]]).
step_right([tessera_2, 4/6, f, Vs], [tessera_2, 5/6, f, [r|Vs]]).
step_right([tessera_2, 5/2, f, Vs], [tessera_2, 6/2, f, [r|Vs]]).
step_right([tessera_2, 5/6, f, Vs], [tessera_2, 6/6, f, [r|Vs]]).
step_up([tessera_2, 0/0, e, Vs], [tessera_2, 0/1, f, [u|Vs]]).
step_up([tessera_2, 0/1, f, Vs], [tessera_2, 0/2, f, [u|Vs]]).
step_up([tessera_2, 0/2, f, Vs], [tessera_2, 0/3, f, [u|Vs]]).
step_up([tessera_2, 0/3, f, Vs], [tessera_2, 0/4, f, [u|Vs]]).
step_up([tessera_2, 2/0, f, Vs], [tessera_2, 2/1, f, [u|Vs]]).
step_up([tessera_2, 2/1, f, Vs], [tessera_2, 2/2, f, [u|Vs]]).
step_up([tessera_2, 2/2, f, Vs], [tessera_2, 2/3, f, [u|Vs]]).
step_up([tessera_2, 2/3, f, Vs], [tessera_2, 2/4, f, [u|Vs]]).
step_up([tessera_2, 2/4, f, Vs], [tessera_2, 2/5, f, [u|Vs]]).
step_up([tessera_2, 2/5, f, Vs], [tessera_2, 2/6, f, [u|Vs]]).
step_up([tessera_2, 4/0, f, Vs], [tessera_2, 4/1, f, [u|Vs]]).
step_up([tessera_2, 4/1, f, Vs], [tessera_2, 4/2, f, [u|Vs]]).
step_up([tessera_2, 4/4, f, Vs], [tessera_2, 4/5, f, [u|Vs]]).
step_up([tessera_2, 4/5, f, Vs], [tessera_2, 4/6, f, [u|Vs]]).
step_up([tessera_2, 6/0, f, Vs], [tessera_2, 6/1, f, [u|Vs]]).
step_up([tessera_2, 6/1, f, Vs], [tessera_2, 6/2, f, [u|Vs]]).
step_up([tessera_2, 6/2, f, Vs], [tessera_2, 6/3, f, [u|Vs]]).
step_up([tessera_2, 6/3, f, Vs], [tessera_2, 6/4, f, [u|Vs]]).
step_up([tessera_2, 6/4, f, Vs], [tessera_2, 6/5, f, [u|Vs]]).
step_up([tessera_2, 6/5, f, Vs], [tessera_2, 6/6, f, [u|Vs]]).

%Maze tessera_3
 maze(tessera_3,
      7-7,
      [ [s, f, f, w, f, f, f],
        [w, w, f, w, f, w, f],
        [f, w, f, w, f, w, f],
        [f, w, f, w, w, w, f],
        [f, w, f, w, f, f, e],
        [f, w, f, w, f, w, f],
        [f, f, f, f, f, w, f]
      ]).

%Primitive moves for maze tessera_3
step_down([tessera_3, 0/1, f, Vs], [tessera_3, 0/0, f, [d|Vs]]).
step_down([tessera_3, 0/2, f, Vs], [tessera_3, 0/1, f, [d|Vs]]).
step_down([tessera_3, 0/3, f, Vs], [tessera_3, 0/2, f, [d|Vs]]).
step_down([tessera_3, 0/4, f, Vs], [tessera_3, 0/3, f, [d|Vs]]).
step_down([tessera_3, 2/1, f, Vs], [tessera_3, 2/0, f, [d|Vs]]).
step_down([tessera_3, 2/2, f, Vs], [tessera_3, 2/1, f, [d|Vs]]).
step_down([tessera_3, 2/3, f, Vs], [tessera_3, 2/2, f, [d|Vs]]).
step_down([tessera_3, 2/4, f, Vs], [tessera_3, 2/3, f, [d|Vs]]).
step_down([tessera_3, 2/5, f, Vs], [tessera_3, 2/4, f, [d|Vs]]).
step_down([tessera_3, 2/6, f, Vs], [tessera_3, 2/5, f, [d|Vs]]).
step_down([tessera_3, 4/1, f, Vs], [tessera_3, 4/0, f, [d|Vs]]).
step_down([tessera_3, 4/2, f, Vs], [tessera_3, 4/1, f, [d|Vs]]).
step_down([tessera_3, 4/5, f, Vs], [tessera_3, 4/4, f, [d|Vs]]).
step_down([tessera_3, 4/6, f, Vs], [tessera_3, 4/5, f, [d|Vs]]).
step_down([tessera_3, 6/1, f, Vs], [tessera_3, 6/0, f, [d|Vs]]).
step_down([tessera_3, 6/2, e, Vs], [tessera_3, 6/1, f, [d|Vs]]).
step_down([tessera_3, 6/3, f, Vs], [tessera_3, 6/2, e, [d|Vs]]).
step_down([tessera_3, 6/4, f, Vs], [tessera_3, 6/3, f, [d|Vs]]).
step_down([tessera_3, 6/5, f, Vs], [tessera_3, 6/4, f, [d|Vs]]).
step_down([tessera_3, 6/6, f, Vs], [tessera_3, 6/5, f, [d|Vs]]).
step_left([tessera_3, 1/0, f, Vs], [tessera_3, 0/0, f, [l|Vs]]).
step_left([tessera_3, 1/6, f, Vs], [tessera_3, 0/6, s, [l|Vs]]).
step_left([tessera_3, 2/0, f, Vs], [tessera_3, 1/0, f, [l|Vs]]).
step_left([tessera_3, 2/6, f, Vs], [tessera_3, 1/6, f, [l|Vs]]).
step_left([tessera_3, 3/0, f, Vs], [tessera_3, 2/0, f, [l|Vs]]).
step_left([tessera_3, 4/0, f, Vs], [tessera_3, 3/0, f, [l|Vs]]).
step_left([tessera_3, 5/2, f, Vs], [tessera_3, 4/2, f, [l|Vs]]).
step_left([tessera_3, 5/6, f, Vs], [tessera_3, 4/6, f, [l|Vs]]).
step_left([tessera_3, 6/2, e, Vs], [tessera_3, 5/2, f, [l|Vs]]).
step_left([tessera_3, 6/6, f, Vs], [tessera_3, 5/6, f, [l|Vs]]).
step_right([tessera_3, 0/0, f, Vs], [tessera_3, 1/0, f, [r|Vs]]).
step_right([tessera_3, 0/6, s, Vs], [tessera_3, 1/6, f, [r|Vs]]).
step_right([tessera_3, 1/0, f, Vs], [tessera_3, 2/0, f, [r|Vs]]).
step_right([tessera_3, 1/6, f, Vs], [tessera_3, 2/6, f, [r|Vs]]).
step_right([tessera_3, 2/0, f, Vs], [tessera_3, 3/0, f, [r|Vs]]).
step_right([tessera_3, 3/0, f, Vs], [tessera_3, 4/0, f, [r|Vs]]).
step_right([tessera_3, 4/2, f, Vs], [tessera_3, 5/2, f, [r|Vs]]).
step_right([tessera_3, 4/6, f, Vs], [tessera_3, 5/6, f, [r|Vs]]).
step_right([tessera_3, 5/2, f, Vs], [tessera_3, 6/2, e, [r|Vs]]).
step_right([tessera_3, 5/6, f, Vs], [tessera_3, 6/6, f, [r|Vs]]).
step_up([tessera_3, 0/0, f, Vs], [tessera_3, 0/1, f, [u|Vs]]).
step_up([tessera_3, 0/1, f, Vs], [tessera_3, 0/2, f, [u|Vs]]).
step_up([tessera_3, 0/2, f, Vs], [tessera_3, 0/3, f, [u|Vs]]).
step_up([tessera_3, 0/3, f, Vs], [tessera_3, 0/4, f, [u|Vs]]).
step_up([tessera_3, 2/0, f, Vs], [tessera_3, 2/1, f, [u|Vs]]).
step_up([tessera_3, 2/1, f, Vs], [tessera_3, 2/2, f, [u|Vs]]).
step_up([tessera_3, 2/2, f, Vs], [tessera_3, 2/3, f, [u|Vs]]).
step_up([tessera_3, 2/3, f, Vs], [tessera_3, 2/4, f, [u|Vs]]).
step_up([tessera_3, 2/4, f, Vs], [tessera_3, 2/5, f, [u|Vs]]).
step_up([tessera_3, 2/5, f, Vs], [tessera_3, 2/6, f, [u|Vs]]).
step_up([tessera_3, 4/0, f, Vs], [tessera_3, 4/1, f, [u|Vs]]).
step_up([tessera_3, 4/1, f, Vs], [tessera_3, 4/2, f, [u|Vs]]).
step_up([tessera_3, 4/4, f, Vs], [tessera_3, 4/5, f, [u|Vs]]).
step_up([tessera_3, 4/5, f, Vs], [tessera_3, 4/6, f, [u|Vs]]).
step_up([tessera_3, 6/0, f, Vs], [tessera_3, 6/1, f, [u|Vs]]).
step_up([tessera_3, 6/1, f, Vs], [tessera_3, 6/2, e, [u|Vs]]).
step_up([tessera_3, 6/2, e, Vs], [tessera_3, 6/3, f, [u|Vs]]).
step_up([tessera_3, 6/3, f, Vs], [tessera_3, 6/4, f, [u|Vs]]).
step_up([tessera_3, 6/4, f, Vs], [tessera_3, 6/5, f, [u|Vs]]).
step_up([tessera_3, 6/5, f, Vs], [tessera_3, 6/6, f, [u|Vs]]).

%Maze tessera_4
 maze(tessera_4,
      7-7,
      [ [s, f, f, w, f, f, e],
        [w, w, f, w, f, w, f],
        [f, w, f, w, f, w, f],
        [f, w, f, w, w, w, f],
        [f, w, f, w, f, f, f],
        [f, w, f, w, f, w, f],
        [f, f, f, f, f, w, f]
      ]).

%Primitive moves for maze tessera_4
step_down([tessera_4, 0/1, f, Vs], [tessera_4, 0/0, f, [d|Vs]]).
step_down([tessera_4, 0/2, f, Vs], [tessera_4, 0/1, f, [d|Vs]]).
step_down([tessera_4, 0/3, f, Vs], [tessera_4, 0/2, f, [d|Vs]]).
step_down([tessera_4, 0/4, f, Vs], [tessera_4, 0/3, f, [d|Vs]]).
step_down([tessera_4, 2/1, f, Vs], [tessera_4, 2/0, f, [d|Vs]]).
step_down([tessera_4, 2/2, f, Vs], [tessera_4, 2/1, f, [d|Vs]]).
step_down([tessera_4, 2/3, f, Vs], [tessera_4, 2/2, f, [d|Vs]]).
step_down([tessera_4, 2/4, f, Vs], [tessera_4, 2/3, f, [d|Vs]]).
step_down([tessera_4, 2/5, f, Vs], [tessera_4, 2/4, f, [d|Vs]]).
step_down([tessera_4, 2/6, f, Vs], [tessera_4, 2/5, f, [d|Vs]]).
step_down([tessera_4, 4/1, f, Vs], [tessera_4, 4/0, f, [d|Vs]]).
step_down([tessera_4, 4/2, f, Vs], [tessera_4, 4/1, f, [d|Vs]]).
step_down([tessera_4, 4/5, f, Vs], [tessera_4, 4/4, f, [d|Vs]]).
step_down([tessera_4, 4/6, f, Vs], [tessera_4, 4/5, f, [d|Vs]]).
step_down([tessera_4, 6/1, f, Vs], [tessera_4, 6/0, f, [d|Vs]]).
step_down([tessera_4, 6/2, f, Vs], [tessera_4, 6/1, f, [d|Vs]]).
step_down([tessera_4, 6/3, f, Vs], [tessera_4, 6/2, f, [d|Vs]]).
step_down([tessera_4, 6/4, f, Vs], [tessera_4, 6/3, f, [d|Vs]]).
step_down([tessera_4, 6/5, f, Vs], [tessera_4, 6/4, f, [d|Vs]]).
step_down([tessera_4, 6/6, e, Vs], [tessera_4, 6/5, f, [d|Vs]]).
step_left([tessera_4, 1/0, f, Vs], [tessera_4, 0/0, f, [l|Vs]]).
step_left([tessera_4, 1/6, f, Vs], [tessera_4, 0/6, s, [l|Vs]]).
step_left([tessera_4, 2/0, f, Vs], [tessera_4, 1/0, f, [l|Vs]]).
step_left([tessera_4, 2/6, f, Vs], [tessera_4, 1/6, f, [l|Vs]]).
step_left([tessera_4, 3/0, f, Vs], [tessera_4, 2/0, f, [l|Vs]]).
step_left([tessera_4, 4/0, f, Vs], [tessera_4, 3/0, f, [l|Vs]]).
step_left([tessera_4, 5/2, f, Vs], [tessera_4, 4/2, f, [l|Vs]]).
step_left([tessera_4, 5/6, f, Vs], [tessera_4, 4/6, f, [l|Vs]]).
step_left([tessera_4, 6/2, f, Vs], [tessera_4, 5/2, f, [l|Vs]]).
step_left([tessera_4, 6/6, e, Vs], [tessera_4, 5/6, f, [l|Vs]]).
step_right([tessera_4, 0/0, f, Vs], [tessera_4, 1/0, f, [r|Vs]]).
step_right([tessera_4, 0/6, s, Vs], [tessera_4, 1/6, f, [r|Vs]]).
step_right([tessera_4, 1/0, f, Vs], [tessera_4, 2/0, f, [r|Vs]]).
step_right([tessera_4, 1/6, f, Vs], [tessera_4, 2/6, f, [r|Vs]]).
step_right([tessera_4, 2/0, f, Vs], [tessera_4, 3/0, f, [r|Vs]]).
step_right([tessera_4, 3/0, f, Vs], [tessera_4, 4/0, f, [r|Vs]]).
step_right([tessera_4, 4/2, f, Vs], [tessera_4, 5/2, f, [r|Vs]]).
step_right([tessera_4, 4/6, f, Vs], [tessera_4, 5/6, f, [r|Vs]]).
step_right([tessera_4, 5/2, f, Vs], [tessera_4, 6/2, f, [r|Vs]]).
step_right([tessera_4, 5/6, f, Vs], [tessera_4, 6/6, e, [r|Vs]]).
step_up([tessera_4, 0/0, f, Vs], [tessera_4, 0/1, f, [u|Vs]]).
step_up([tessera_4, 0/1, f, Vs], [tessera_4, 0/2, f, [u|Vs]]).
step_up([tessera_4, 0/2, f, Vs], [tessera_4, 0/3, f, [u|Vs]]).
step_up([tessera_4, 0/3, f, Vs], [tessera_4, 0/4, f, [u|Vs]]).
step_up([tessera_4, 2/0, f, Vs], [tessera_4, 2/1, f, [u|Vs]]).
step_up([tessera_4, 2/1, f, Vs], [tessera_4, 2/2, f, [u|Vs]]).
step_up([tessera_4, 2/2, f, Vs], [tessera_4, 2/3, f, [u|Vs]]).
step_up([tessera_4, 2/3, f, Vs], [tessera_4, 2/4, f, [u|Vs]]).
step_up([tessera_4, 2/4, f, Vs], [tessera_4, 2/5, f, [u|Vs]]).
step_up([tessera_4, 2/5, f, Vs], [tessera_4, 2/6, f, [u|Vs]]).
step_up([tessera_4, 4/0, f, Vs], [tessera_4, 4/1, f, [u|Vs]]).
step_up([tessera_4, 4/1, f, Vs], [tessera_4, 4/2, f, [u|Vs]]).
step_up([tessera_4, 4/4, f, Vs], [tessera_4, 4/5, f, [u|Vs]]).
step_up([tessera_4, 4/5, f, Vs], [tessera_4, 4/6, f, [u|Vs]]).
step_up([tessera_4, 6/0, f, Vs], [tessera_4, 6/1, f, [u|Vs]]).
step_up([tessera_4, 6/1, f, Vs], [tessera_4, 6/2, f, [u|Vs]]).
step_up([tessera_4, 6/2, f, Vs], [tessera_4, 6/3, f, [u|Vs]]).
step_up([tessera_4, 6/3, f, Vs], [tessera_4, 6/4, f, [u|Vs]]).
step_up([tessera_4, 6/4, f, Vs], [tessera_4, 6/5, f, [u|Vs]]).
step_up([tessera_4, 6/5, f, Vs], [tessera_4, 6/6, e, [u|Vs]]).

