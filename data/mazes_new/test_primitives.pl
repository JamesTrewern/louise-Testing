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

%Maze 0
 maze(0,
      7-7,
      [ [s, f, f, w, f, f, f],
        [w, w, f, w, f, w, f],
        [f, w, f, w, f, w, f],
        [f, w, f, w, w, w, f],
        [f, w, f, w, f, f, f],
        [f, w, f, w, f, w, f],
        [e, f, f, f, f, w, f]
      ]).

%Primitive moves for maze 0
step_down([0, 0/1, f], [0, 0/0, e]).
step_down([0, 0/2, f], [0, 0/1, f]).
step_down([0, 0/3, f], [0, 0/2, f]).
step_down([0, 0/4, f], [0, 0/3, f]).
step_down([0, 2/1, f], [0, 2/0, f]).
step_down([0, 2/2, f], [0, 2/1, f]).
step_down([0, 2/3, f], [0, 2/2, f]).
step_down([0, 2/4, f], [0, 2/3, f]).
step_down([0, 2/5, f], [0, 2/4, f]).
step_down([0, 2/6, f], [0, 2/5, f]).
step_down([0, 4/1, f], [0, 4/0, f]).
step_down([0, 4/2, f], [0, 4/1, f]).
step_down([0, 4/5, f], [0, 4/4, f]).
step_down([0, 4/6, f], [0, 4/5, f]).
step_down([0, 6/1, f], [0, 6/0, f]).
step_down([0, 6/2, f], [0, 6/1, f]).
step_down([0, 6/3, f], [0, 6/2, f]).
step_down([0, 6/4, f], [0, 6/3, f]).
step_down([0, 6/5, f], [0, 6/4, f]).
step_down([0, 6/6, f], [0, 6/5, f]).
step_left([0, 1/0, f], [0, 0/0, e]).
step_left([0, 1/6, f], [0, 0/6, s]).
step_left([0, 2/0, f], [0, 1/0, f]).
step_left([0, 2/6, f], [0, 1/6, f]).
step_left([0, 3/0, f], [0, 2/0, f]).
step_left([0, 4/0, f], [0, 3/0, f]).
step_left([0, 5/2, f], [0, 4/2, f]).
step_left([0, 5/6, f], [0, 4/6, f]).
step_left([0, 6/2, f], [0, 5/2, f]).
step_left([0, 6/6, f], [0, 5/6, f]).
step_right([0, 0/0, e], [0, 1/0, f]).
step_right([0, 0/6, s], [0, 1/6, f]).
step_right([0, 1/0, f], [0, 2/0, f]).
step_right([0, 1/6, f], [0, 2/6, f]).
step_right([0, 2/0, f], [0, 3/0, f]).
step_right([0, 3/0, f], [0, 4/0, f]).
step_right([0, 4/2, f], [0, 5/2, f]).
step_right([0, 4/6, f], [0, 5/6, f]).
step_right([0, 5/2, f], [0, 6/2, f]).
step_right([0, 5/6, f], [0, 6/6, f]).
step_up([0, 0/0, e], [0, 0/1, f]).
step_up([0, 0/1, f], [0, 0/2, f]).
step_up([0, 0/2, f], [0, 0/3, f]).
step_up([0, 0/3, f], [0, 0/4, f]).
step_up([0, 2/0, f], [0, 2/1, f]).
step_up([0, 2/1, f], [0, 2/2, f]).
step_up([0, 2/2, f], [0, 2/3, f]).
step_up([0, 2/3, f], [0, 2/4, f]).
step_up([0, 2/4, f], [0, 2/5, f]).
step_up([0, 2/5, f], [0, 2/6, f]).
step_up([0, 4/0, f], [0, 4/1, f]).
step_up([0, 4/1, f], [0, 4/2, f]).
step_up([0, 4/4, f], [0, 4/5, f]).
step_up([0, 4/5, f], [0, 4/6, f]).
step_up([0, 6/0, f], [0, 6/1, f]).
step_up([0, 6/1, f], [0, 6/2, f]).
step_up([0, 6/2, f], [0, 6/3, f]).
step_up([0, 6/3, f], [0, 6/4, f]).
step_up([0, 6/4, f], [0, 6/5, f]).
step_up([0, 6/5, f], [0, 6/6, f]).

%Maze 1
 maze(1,
      19-19,
      [ [e, w, f, f, f, f, f, f, f, f, f, w, f, w, f, f, f, f, f],
        [f, w, f, w, f, w, w, w, w, w, f, w, f, w, f, w, w, w, f],
        [f, w, f, w, f, w, f, f, f, w, f, w, f, f, f, f, f, w, f],
        [f, w, f, w, f, w, f, w, f, w, f, w, w, w, w, w, f, w, f],
        [f, w, f, w, f, w, f, w, f, f, f, f, f, f, f, w, f, w, f],
        [f, w, f, w, f, w, w, w, w, w, w, w, w, w, f, w, w, w, f],
        [f, w, f, w, f, f, f, f, f, w, f, f, f, f, f, w, f, f, f],
        [f, w, f, w, w, w, w, w, f, w, w, w, w, w, f, w, f, w, f],
        [f, w, f, w, f, f, f, w, f, w, f, f, f, w, f, w, f, w, f],
        [f, w, w, w, f, w, f, w, f, w, f, w, f, w, f, w, f, w, w],
        [f, f, f, f, f, w, f, w, f, f, f, w, f, w, f, w, f, f, f],
        [w, w, w, w, w, w, f, w, f, w, w, w, f, w, f, w, w, w, f],
        [f, f, f, w, f, f, f, w, f, f, f, w, f, w, f, f, f, f, f],
        [f, w, w, w, f, w, w, w, w, w, f, w, f, w, w, w, w, w, f],
        [f, f, f, w, f, f, f, w, f, f, f, w, f, w, f, f, f, w, f],
        [w, w, f, w, w, w, f, w, w, w, w, w, f, w, f, w, f, w, f],
        [f, f, f, f, f, w, f, w, f, f, f, f, f, w, f, w, f, w, f],
        [f, w, w, w, w, w, f, w, f, w, w, w, w, w, f, w, w, w, f],
        [f, f, f, f, f, f, f, f, f, w, f, f, f, f, f, f, f, f, s]
      ]).

%Primitive moves for maze 1
step_down([1, 0/1, f], [1, 0/0, f]).
step_down([1, 0/2, f], [1, 0/1, f]).
step_down([1, 0/5, f], [1, 0/4, f]).
step_down([1, 0/6, f], [1, 0/5, f]).
step_down([1, 0/9, f], [1, 0/8, f]).
step_down([1, 0/10, f], [1, 0/9, f]).
step_down([1, 0/11, f], [1, 0/10, f]).
step_down([1, 0/12, f], [1, 0/11, f]).
step_down([1, 0/13, f], [1, 0/12, f]).
step_down([1, 0/14, f], [1, 0/13, f]).
step_down([1, 0/15, f], [1, 0/14, f]).
step_down([1, 0/16, f], [1, 0/15, f]).
step_down([1, 0/17, f], [1, 0/16, f]).
step_down([1, 0/18, e], [1, 0/17, f]).
step_down([1, 2/3, f], [1, 2/2, f]).
step_down([1, 2/4, f], [1, 2/3, f]).
step_down([1, 2/11, f], [1, 2/10, f]).
step_down([1, 2/12, f], [1, 2/11, f]).
step_down([1, 2/13, f], [1, 2/12, f]).
step_down([1, 2/14, f], [1, 2/13, f]).
step_down([1, 2/15, f], [1, 2/14, f]).
step_down([1, 2/16, f], [1, 2/15, f]).
step_down([1, 2/17, f], [1, 2/16, f]).
step_down([1, 2/18, f], [1, 2/17, f]).
step_down([1, 4/5, f], [1, 4/4, f]).
step_down([1, 4/6, f], [1, 4/5, f]).
step_down([1, 4/9, f], [1, 4/8, f]).
step_down([1, 4/10, f], [1, 4/9, f]).
step_down([1, 4/13, f], [1, 4/12, f]).
step_down([1, 4/14, f], [1, 4/13, f]).
step_down([1, 4/15, f], [1, 4/14, f]).
step_down([1, 4/16, f], [1, 4/15, f]).
step_down([1, 4/17, f], [1, 4/16, f]).
step_down([1, 4/18, f], [1, 4/17, f]).
step_down([1, 6/1, f], [1, 6/0, f]).
step_down([1, 6/2, f], [1, 6/1, f]).
step_down([1, 6/3, f], [1, 6/2, f]).
step_down([1, 6/4, f], [1, 6/3, f]).
step_down([1, 6/7, f], [1, 6/6, f]).
step_down([1, 6/8, f], [1, 6/7, f]).
step_down([1, 6/9, f], [1, 6/8, f]).
step_down([1, 6/10, f], [1, 6/9, f]).
step_down([1, 6/15, f], [1, 6/14, f]).
step_down([1, 6/16, f], [1, 6/15, f]).
step_down([1, 8/1, f], [1, 8/0, f]).
step_down([1, 8/2, f], [1, 8/1, f]).
step_down([1, 8/7, f], [1, 8/6, f]).
step_down([1, 8/8, f], [1, 8/7, f]).
step_down([1, 8/9, f], [1, 8/8, f]).
step_down([1, 8/10, f], [1, 8/9, f]).
step_down([1, 8/11, f], [1, 8/10, f]).
step_down([1, 8/12, f], [1, 8/11, f]).
step_down([1, 8/15, f], [1, 8/14, f]).
step_down([1, 8/16, f], [1, 8/15, f]).
step_down([1, 10/5, f], [1, 10/4, f]).
step_down([1, 10/6, f], [1, 10/5, f]).
step_down([1, 10/9, f], [1, 10/8, f]).
step_down([1, 10/10, f], [1, 10/9, f]).
step_down([1, 10/15, f], [1, 10/14, f]).
step_down([1, 10/16, f], [1, 10/15, f]).
step_down([1, 10/17, f], [1, 10/16, f]).
step_down([1, 10/18, f], [1, 10/17, f]).
step_down([1, 12/3, f], [1, 12/2, f]).
step_down([1, 12/4, f], [1, 12/3, f]).
step_down([1, 12/5, f], [1, 12/4, f]).
step_down([1, 12/6, f], [1, 12/5, f]).
step_down([1, 12/7, f], [1, 12/6, f]).
step_down([1, 12/8, f], [1, 12/7, f]).
step_down([1, 12/9, f], [1, 12/8, f]).
step_down([1, 12/10, f], [1, 12/9, f]).
step_down([1, 12/17, f], [1, 12/16, f]).
step_down([1, 12/18, f], [1, 12/17, f]).
step_down([1, 14/1, f], [1, 14/0, f]).
step_down([1, 14/2, f], [1, 14/1, f]).
step_down([1, 14/3, f], [1, 14/2, f]).
step_down([1, 14/4, f], [1, 14/3, f]).
step_down([1, 14/7, f], [1, 14/6, f]).
step_down([1, 14/8, f], [1, 14/7, f]).
step_down([1, 14/9, f], [1, 14/8, f]).
step_down([1, 14/10, f], [1, 14/9, f]).
step_down([1, 14/11, f], [1, 14/10, f]).
step_down([1, 14/12, f], [1, 14/11, f]).
step_down([1, 14/13, f], [1, 14/12, f]).
step_down([1, 14/14, f], [1, 14/13, f]).
step_down([1, 14/17, f], [1, 14/16, f]).
step_down([1, 14/18, f], [1, 14/17, f]).
step_down([1, 16/3, f], [1, 16/2, f]).
step_down([1, 16/4, f], [1, 16/3, f]).
step_down([1, 16/9, f], [1, 16/8, f]).
step_down([1, 16/10, f], [1, 16/9, f]).
step_down([1, 16/11, f], [1, 16/10, f]).
step_down([1, 16/12, f], [1, 16/11, f]).
step_down([1, 16/15, f], [1, 16/14, f]).
step_down([1, 16/16, f], [1, 16/15, f]).
step_down([1, 18/1, f], [1, 18/0, s]).
step_down([1, 18/2, f], [1, 18/1, f]).
step_down([1, 18/3, f], [1, 18/2, f]).
step_down([1, 18/4, f], [1, 18/3, f]).
step_down([1, 18/5, f], [1, 18/4, f]).
step_down([1, 18/6, f], [1, 18/5, f]).
step_down([1, 18/7, f], [1, 18/6, f]).
step_down([1, 18/8, f], [1, 18/7, f]).
step_down([1, 18/11, f], [1, 18/10, f]).
step_down([1, 18/12, f], [1, 18/11, f]).
step_down([1, 18/13, f], [1, 18/12, f]).
step_down([1, 18/14, f], [1, 18/13, f]).
step_down([1, 18/15, f], [1, 18/14, f]).
step_down([1, 18/16, f], [1, 18/15, f]).
step_down([1, 18/17, f], [1, 18/16, f]).
step_down([1, 18/18, f], [1, 18/17, f]).
step_left([1, 1/0, f], [1, 0/0, f]).
step_left([1, 1/2, f], [1, 0/2, f]).
step_left([1, 1/4, f], [1, 0/4, f]).
step_left([1, 1/6, f], [1, 0/6, f]).
step_left([1, 1/8, f], [1, 0/8, f]).
step_left([1, 2/0, f], [1, 1/0, f]).
step_left([1, 2/2, f], [1, 1/2, f]).
step_left([1, 2/4, f], [1, 1/4, f]).
step_left([1, 2/6, f], [1, 1/6, f]).
step_left([1, 2/8, f], [1, 1/8, f]).
step_left([1, 3/0, f], [1, 2/0, f]).
step_left([1, 3/2, f], [1, 2/2, f]).
step_left([1, 3/8, f], [1, 2/8, f]).
step_left([1, 3/18, f], [1, 2/18, f]).
step_left([1, 4/0, f], [1, 3/0, f]).
step_left([1, 4/2, f], [1, 3/2, f]).
step_left([1, 4/8, f], [1, 3/8, f]).
step_left([1, 4/18, f], [1, 3/18, f]).
step_left([1, 5/0, f], [1, 4/0, f]).
step_left([1, 5/4, f], [1, 4/4, f]).
step_left([1, 5/6, f], [1, 4/6, f]).
step_left([1, 5/10, f], [1, 4/10, f]).
step_left([1, 5/12, f], [1, 4/12, f]).
step_left([1, 5/18, f], [1, 4/18, f]).
step_left([1, 6/0, f], [1, 5/0, f]).
step_left([1, 6/4, f], [1, 5/4, f]).
step_left([1, 6/6, f], [1, 5/6, f]).
step_left([1, 6/10, f], [1, 5/10, f]).
step_left([1, 6/12, f], [1, 5/12, f]).
step_left([1, 6/18, f], [1, 5/18, f]).
step_left([1, 7/0, f], [1, 6/0, f]).
step_left([1, 7/12, f], [1, 6/12, f]).
step_left([1, 7/16, f], [1, 6/16, f]).
step_left([1, 7/18, f], [1, 6/18, f]).
step_left([1, 8/0, f], [1, 7/0, f]).
step_left([1, 8/12, f], [1, 7/12, f]).
step_left([1, 8/16, f], [1, 7/16, f]).
step_left([1, 8/18, f], [1, 7/18, f]).
step_left([1, 9/2, f], [1, 8/2, f]).
step_left([1, 9/4, f], [1, 8/4, f]).
step_left([1, 9/6, f], [1, 8/6, f]).
step_left([1, 9/8, f], [1, 8/8, f]).
step_left([1, 9/14, f], [1, 8/14, f]).
step_left([1, 9/18, f], [1, 8/18, f]).
step_left([1, 10/2, f], [1, 9/2, f]).
step_left([1, 10/4, f], [1, 9/4, f]).
step_left([1, 10/6, f], [1, 9/6, f]).
step_left([1, 10/8, f], [1, 9/8, f]).
step_left([1, 10/14, f], [1, 9/14, f]).
step_left([1, 10/18, f], [1, 9/18, f]).
step_left([1, 11/0, f], [1, 10/0, f]).
step_left([1, 11/2, f], [1, 10/2, f]).
step_left([1, 11/10, f], [1, 10/10, f]).
step_left([1, 11/12, f], [1, 10/12, f]).
step_left([1, 11/14, f], [1, 10/14, f]).
step_left([1, 12/0, f], [1, 11/0, f]).
step_left([1, 12/2, f], [1, 11/2, f]).
step_left([1, 12/10, f], [1, 11/10, f]).
step_left([1, 12/12, f], [1, 11/12, f]).
step_left([1, 12/14, f], [1, 11/14, f]).
step_left([1, 13/0, f], [1, 12/0, f]).
step_left([1, 13/12, f], [1, 12/12, f]).
step_left([1, 13/14, f], [1, 12/14, f]).
step_left([1, 13/16, f], [1, 12/16, f]).
step_left([1, 14/0, f], [1, 13/0, f]).
step_left([1, 14/12, f], [1, 13/12, f]).
step_left([1, 14/14, f], [1, 13/14, f]).
step_left([1, 14/16, f], [1, 13/16, f]).
step_left([1, 15/0, f], [1, 14/0, f]).
step_left([1, 15/4, f], [1, 14/4, f]).
step_left([1, 15/6, f], [1, 14/6, f]).
step_left([1, 15/16, f], [1, 14/16, f]).
step_left([1, 15/18, f], [1, 14/18, f]).
step_left([1, 16/0, f], [1, 15/0, f]).
step_left([1, 16/4, f], [1, 15/4, f]).
step_left([1, 16/6, f], [1, 15/6, f]).
step_left([1, 16/16, f], [1, 15/16, f]).
step_left([1, 16/18, f], [1, 15/18, f]).
step_left([1, 17/0, f], [1, 16/0, f]).
step_left([1, 17/6, f], [1, 16/6, f]).
step_left([1, 17/8, f], [1, 16/8, f]).
step_left([1, 17/12, f], [1, 16/12, f]).
step_left([1, 17/18, f], [1, 16/18, f]).
step_left([1, 18/0, s], [1, 17/0, f]).
step_left([1, 18/6, f], [1, 17/6, f]).
step_left([1, 18/8, f], [1, 17/8, f]).
step_left([1, 18/12, f], [1, 17/12, f]).
step_left([1, 18/18, f], [1, 17/18, f]).
step_right([1, 0/0, f], [1, 1/0, f]).
step_right([1, 0/2, f], [1, 1/2, f]).
step_right([1, 0/4, f], [1, 1/4, f]).
step_right([1, 0/6, f], [1, 1/6, f]).
step_right([1, 0/8, f], [1, 1/8, f]).
step_right([1, 1/0, f], [1, 2/0, f]).
step_right([1, 1/2, f], [1, 2/2, f]).
step_right([1, 1/4, f], [1, 2/4, f]).
step_right([1, 1/6, f], [1, 2/6, f]).
step_right([1, 1/8, f], [1, 2/8, f]).
step_right([1, 2/0, f], [1, 3/0, f]).
step_right([1, 2/2, f], [1, 3/2, f]).
step_right([1, 2/8, f], [1, 3/8, f]).
step_right([1, 2/18, f], [1, 3/18, f]).
step_right([1, 3/0, f], [1, 4/0, f]).
step_right([1, 3/2, f], [1, 4/2, f]).
step_right([1, 3/8, f], [1, 4/8, f]).
step_right([1, 3/18, f], [1, 4/18, f]).
step_right([1, 4/0, f], [1, 5/0, f]).
step_right([1, 4/4, f], [1, 5/4, f]).
step_right([1, 4/6, f], [1, 5/6, f]).
step_right([1, 4/10, f], [1, 5/10, f]).
step_right([1, 4/12, f], [1, 5/12, f]).
step_right([1, 4/18, f], [1, 5/18, f]).
step_right([1, 5/0, f], [1, 6/0, f]).
step_right([1, 5/4, f], [1, 6/4, f]).
step_right([1, 5/6, f], [1, 6/6, f]).
step_right([1, 5/10, f], [1, 6/10, f]).
step_right([1, 5/12, f], [1, 6/12, f]).
step_right([1, 5/18, f], [1, 6/18, f]).
step_right([1, 6/0, f], [1, 7/0, f]).
step_right([1, 6/12, f], [1, 7/12, f]).
step_right([1, 6/16, f], [1, 7/16, f]).
step_right([1, 6/18, f], [1, 7/18, f]).
step_right([1, 7/0, f], [1, 8/0, f]).
step_right([1, 7/12, f], [1, 8/12, f]).
step_right([1, 7/16, f], [1, 8/16, f]).
step_right([1, 7/18, f], [1, 8/18, f]).
step_right([1, 8/2, f], [1, 9/2, f]).
step_right([1, 8/4, f], [1, 9/4, f]).
step_right([1, 8/6, f], [1, 9/6, f]).
step_right([1, 8/8, f], [1, 9/8, f]).
step_right([1, 8/14, f], [1, 9/14, f]).
step_right([1, 8/18, f], [1, 9/18, f]).
step_right([1, 9/2, f], [1, 10/2, f]).
step_right([1, 9/4, f], [1, 10/4, f]).
step_right([1, 9/6, f], [1, 10/6, f]).
step_right([1, 9/8, f], [1, 10/8, f]).
step_right([1, 9/14, f], [1, 10/14, f]).
step_right([1, 9/18, f], [1, 10/18, f]).
step_right([1, 10/0, f], [1, 11/0, f]).
step_right([1, 10/2, f], [1, 11/2, f]).
step_right([1, 10/10, f], [1, 11/10, f]).
step_right([1, 10/12, f], [1, 11/12, f]).
step_right([1, 10/14, f], [1, 11/14, f]).
step_right([1, 11/0, f], [1, 12/0, f]).
step_right([1, 11/2, f], [1, 12/2, f]).
step_right([1, 11/10, f], [1, 12/10, f]).
step_right([1, 11/12, f], [1, 12/12, f]).
step_right([1, 11/14, f], [1, 12/14, f]).
step_right([1, 12/0, f], [1, 13/0, f]).
step_right([1, 12/12, f], [1, 13/12, f]).
step_right([1, 12/14, f], [1, 13/14, f]).
step_right([1, 12/16, f], [1, 13/16, f]).
step_right([1, 13/0, f], [1, 14/0, f]).
step_right([1, 13/12, f], [1, 14/12, f]).
step_right([1, 13/14, f], [1, 14/14, f]).
step_right([1, 13/16, f], [1, 14/16, f]).
step_right([1, 14/0, f], [1, 15/0, f]).
step_right([1, 14/4, f], [1, 15/4, f]).
step_right([1, 14/6, f], [1, 15/6, f]).
step_right([1, 14/16, f], [1, 15/16, f]).
step_right([1, 14/18, f], [1, 15/18, f]).
step_right([1, 15/0, f], [1, 16/0, f]).
step_right([1, 15/4, f], [1, 16/4, f]).
step_right([1, 15/6, f], [1, 16/6, f]).
step_right([1, 15/16, f], [1, 16/16, f]).
step_right([1, 15/18, f], [1, 16/18, f]).
step_right([1, 16/0, f], [1, 17/0, f]).
step_right([1, 16/6, f], [1, 17/6, f]).
step_right([1, 16/8, f], [1, 17/8, f]).
step_right([1, 16/12, f], [1, 17/12, f]).
step_right([1, 16/18, f], [1, 17/18, f]).
step_right([1, 17/0, f], [1, 18/0, s]).
step_right([1, 17/6, f], [1, 18/6, f]).
step_right([1, 17/8, f], [1, 18/8, f]).
step_right([1, 17/12, f], [1, 18/12, f]).
step_right([1, 17/18, f], [1, 18/18, f]).
step_up([1, 0/0, f], [1, 0/1, f]).
step_up([1, 0/1, f], [1, 0/2, f]).
step_up([1, 0/4, f], [1, 0/5, f]).
step_up([1, 0/5, f], [1, 0/6, f]).
step_up([1, 0/8, f], [1, 0/9, f]).
step_up([1, 0/9, f], [1, 0/10, f]).
step_up([1, 0/10, f], [1, 0/11, f]).
step_up([1, 0/11, f], [1, 0/12, f]).
step_up([1, 0/12, f], [1, 0/13, f]).
step_up([1, 0/13, f], [1, 0/14, f]).
step_up([1, 0/14, f], [1, 0/15, f]).
step_up([1, 0/15, f], [1, 0/16, f]).
step_up([1, 0/16, f], [1, 0/17, f]).
step_up([1, 0/17, f], [1, 0/18, e]).
step_up([1, 2/2, f], [1, 2/3, f]).
step_up([1, 2/3, f], [1, 2/4, f]).
step_up([1, 2/10, f], [1, 2/11, f]).
step_up([1, 2/11, f], [1, 2/12, f]).
step_up([1, 2/12, f], [1, 2/13, f]).
step_up([1, 2/13, f], [1, 2/14, f]).
step_up([1, 2/14, f], [1, 2/15, f]).
step_up([1, 2/15, f], [1, 2/16, f]).
step_up([1, 2/16, f], [1, 2/17, f]).
step_up([1, 2/17, f], [1, 2/18, f]).
step_up([1, 4/4, f], [1, 4/5, f]).
step_up([1, 4/5, f], [1, 4/6, f]).
step_up([1, 4/8, f], [1, 4/9, f]).
step_up([1, 4/9, f], [1, 4/10, f]).
step_up([1, 4/12, f], [1, 4/13, f]).
step_up([1, 4/13, f], [1, 4/14, f]).
step_up([1, 4/14, f], [1, 4/15, f]).
step_up([1, 4/15, f], [1, 4/16, f]).
step_up([1, 4/16, f], [1, 4/17, f]).
step_up([1, 4/17, f], [1, 4/18, f]).
step_up([1, 6/0, f], [1, 6/1, f]).
step_up([1, 6/1, f], [1, 6/2, f]).
step_up([1, 6/2, f], [1, 6/3, f]).
step_up([1, 6/3, f], [1, 6/4, f]).
step_up([1, 6/6, f], [1, 6/7, f]).
step_up([1, 6/7, f], [1, 6/8, f]).
step_up([1, 6/8, f], [1, 6/9, f]).
step_up([1, 6/9, f], [1, 6/10, f]).
step_up([1, 6/14, f], [1, 6/15, f]).
step_up([1, 6/15, f], [1, 6/16, f]).
step_up([1, 8/0, f], [1, 8/1, f]).
step_up([1, 8/1, f], [1, 8/2, f]).
step_up([1, 8/6, f], [1, 8/7, f]).
step_up([1, 8/7, f], [1, 8/8, f]).
step_up([1, 8/8, f], [1, 8/9, f]).
step_up([1, 8/9, f], [1, 8/10, f]).
step_up([1, 8/10, f], [1, 8/11, f]).
step_up([1, 8/11, f], [1, 8/12, f]).
step_up([1, 8/14, f], [1, 8/15, f]).
step_up([1, 8/15, f], [1, 8/16, f]).
step_up([1, 10/4, f], [1, 10/5, f]).
step_up([1, 10/5, f], [1, 10/6, f]).
step_up([1, 10/8, f], [1, 10/9, f]).
step_up([1, 10/9, f], [1, 10/10, f]).
step_up([1, 10/14, f], [1, 10/15, f]).
step_up([1, 10/15, f], [1, 10/16, f]).
step_up([1, 10/16, f], [1, 10/17, f]).
step_up([1, 10/17, f], [1, 10/18, f]).
step_up([1, 12/2, f], [1, 12/3, f]).
step_up([1, 12/3, f], [1, 12/4, f]).
step_up([1, 12/4, f], [1, 12/5, f]).
step_up([1, 12/5, f], [1, 12/6, f]).
step_up([1, 12/6, f], [1, 12/7, f]).
step_up([1, 12/7, f], [1, 12/8, f]).
step_up([1, 12/8, f], [1, 12/9, f]).
step_up([1, 12/9, f], [1, 12/10, f]).
step_up([1, 12/16, f], [1, 12/17, f]).
step_up([1, 12/17, f], [1, 12/18, f]).
step_up([1, 14/0, f], [1, 14/1, f]).
step_up([1, 14/1, f], [1, 14/2, f]).
step_up([1, 14/2, f], [1, 14/3, f]).
step_up([1, 14/3, f], [1, 14/4, f]).
step_up([1, 14/6, f], [1, 14/7, f]).
step_up([1, 14/7, f], [1, 14/8, f]).
step_up([1, 14/8, f], [1, 14/9, f]).
step_up([1, 14/9, f], [1, 14/10, f]).
step_up([1, 14/10, f], [1, 14/11, f]).
step_up([1, 14/11, f], [1, 14/12, f]).
step_up([1, 14/12, f], [1, 14/13, f]).
step_up([1, 14/13, f], [1, 14/14, f]).
step_up([1, 14/16, f], [1, 14/17, f]).
step_up([1, 14/17, f], [1, 14/18, f]).
step_up([1, 16/2, f], [1, 16/3, f]).
step_up([1, 16/3, f], [1, 16/4, f]).
step_up([1, 16/8, f], [1, 16/9, f]).
step_up([1, 16/9, f], [1, 16/10, f]).
step_up([1, 16/10, f], [1, 16/11, f]).
step_up([1, 16/11, f], [1, 16/12, f]).
step_up([1, 16/14, f], [1, 16/15, f]).
step_up([1, 16/15, f], [1, 16/16, f]).
step_up([1, 18/0, s], [1, 18/1, f]).
step_up([1, 18/1, f], [1, 18/2, f]).
step_up([1, 18/2, f], [1, 18/3, f]).
step_up([1, 18/3, f], [1, 18/4, f]).
step_up([1, 18/4, f], [1, 18/5, f]).
step_up([1, 18/5, f], [1, 18/6, f]).
step_up([1, 18/6, f], [1, 18/7, f]).
step_up([1, 18/7, f], [1, 18/8, f]).
step_up([1, 18/10, f], [1, 18/11, f]).
step_up([1, 18/11, f], [1, 18/12, f]).
step_up([1, 18/12, f], [1, 18/13, f]).
step_up([1, 18/13, f], [1, 18/14, f]).
step_up([1, 18/14, f], [1, 18/15, f]).
step_up([1, 18/15, f], [1, 18/16, f]).
step_up([1, 18/16, f], [1, 18/17, f]).
step_up([1, 18/17, f], [1, 18/18, f]).

%Maze 2
 maze(2,
      119-119,
      [ [ s,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f
        ],
        [ w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f
        ],
        [ w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f
        ],
        [ w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f
        ],
        [ f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f
        ],
        [ f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w
        ],
        [ f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f
        ],
        [ f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          f,
          w,
          f,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          w,
          f,
          w,
          f
        ],
        [ f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          f,
          w,
          e
        ]
      ]).

%Primitive moves for maze 2
step_down([2, 0/1, f], [2, 0/0, f]).
step_down([2, 0/2, f], [2, 0/1, f]).
step_down([2, 0/3, f], [2, 0/2, f]).
step_down([2, 0/4, f], [2, 0/3, f]).
step_down([2, 0/5, f], [2, 0/4, f]).
step_down([2, 0/6, f], [2, 0/5, f]).
step_down([2, 0/7, f], [2, 0/6, f]).
step_down([2, 0/8, f], [2, 0/7, f]).
step_down([2, 0/9, f], [2, 0/8, f]).
step_down([2, 0/10, f], [2, 0/9, f]).
step_down([2, 0/13, f], [2, 0/12, f]).
step_down([2, 0/14, f], [2, 0/13, f]).
step_down([2, 0/15, f], [2, 0/14, f]).
step_down([2, 0/16, f], [2, 0/15, f]).
step_down([2, 0/17, f], [2, 0/16, f]).
step_down([2, 0/18, f], [2, 0/17, f]).
step_down([2, 0/21, f], [2, 0/20, f]).
step_down([2, 0/22, f], [2, 0/21, f]).
step_down([2, 0/25, f], [2, 0/24, f]).
step_down([2, 0/26, f], [2, 0/25, f]).
step_down([2, 0/27, f], [2, 0/26, f]).
step_down([2, 0/28, f], [2, 0/27, f]).
step_down([2, 0/29, f], [2, 0/28, f]).
step_down([2, 0/30, f], [2, 0/29, f]).
step_down([2, 0/31, f], [2, 0/30, f]).
step_down([2, 0/32, f], [2, 0/31, f]).
step_down([2, 0/33, f], [2, 0/32, f]).
step_down([2, 0/34, f], [2, 0/33, f]).
step_down([2, 0/35, f], [2, 0/34, f]).
step_down([2, 0/36, f], [2, 0/35, f]).
step_down([2, 0/39, f], [2, 0/38, f]).
step_down([2, 0/40, f], [2, 0/39, f]).
step_down([2, 0/43, f], [2, 0/42, f]).
step_down([2, 0/44, f], [2, 0/43, f]).
step_down([2, 0/45, f], [2, 0/44, f]).
step_down([2, 0/46, f], [2, 0/45, f]).
step_down([2, 0/47, f], [2, 0/46, f]).
step_down([2, 0/48, f], [2, 0/47, f]).
step_down([2, 0/51, f], [2, 0/50, f]).
step_down([2, 0/52, f], [2, 0/51, f]).
step_down([2, 0/53, f], [2, 0/52, f]).
step_down([2, 0/54, f], [2, 0/53, f]).
step_down([2, 0/55, f], [2, 0/54, f]).
step_down([2, 0/56, f], [2, 0/55, f]).
step_down([2, 0/59, f], [2, 0/58, f]).
step_down([2, 0/60, f], [2, 0/59, f]).
step_down([2, 0/61, f], [2, 0/60, f]).
step_down([2, 0/62, f], [2, 0/61, f]).
step_down([2, 0/63, f], [2, 0/62, f]).
step_down([2, 0/64, f], [2, 0/63, f]).
step_down([2, 0/65, f], [2, 0/64, f]).
step_down([2, 0/66, f], [2, 0/65, f]).
step_down([2, 0/67, f], [2, 0/66, f]).
step_down([2, 0/68, f], [2, 0/67, f]).
step_down([2, 0/69, f], [2, 0/68, f]).
step_down([2, 0/70, f], [2, 0/69, f]).
step_down([2, 0/73, f], [2, 0/72, f]).
step_down([2, 0/74, f], [2, 0/73, f]).
step_down([2, 0/75, f], [2, 0/74, f]).
step_down([2, 0/76, f], [2, 0/75, f]).
step_down([2, 0/77, f], [2, 0/76, f]).
step_down([2, 0/78, f], [2, 0/77, f]).
step_down([2, 0/79, f], [2, 0/78, f]).
step_down([2, 0/80, f], [2, 0/79, f]).
step_down([2, 0/85, f], [2, 0/84, f]).
step_down([2, 0/86, f], [2, 0/85, f]).
step_down([2, 0/87, f], [2, 0/86, f]).
step_down([2, 0/88, f], [2, 0/87, f]).
step_down([2, 0/89, f], [2, 0/88, f]).
step_down([2, 0/90, f], [2, 0/89, f]).
step_down([2, 0/91, f], [2, 0/90, f]).
step_down([2, 0/92, f], [2, 0/91, f]).
step_down([2, 0/93, f], [2, 0/92, f]).
step_down([2, 0/94, f], [2, 0/93, f]).
step_down([2, 0/95, f], [2, 0/94, f]).
step_down([2, 0/96, f], [2, 0/95, f]).
step_down([2, 0/97, f], [2, 0/96, f]).
step_down([2, 0/98, f], [2, 0/97, f]).
step_down([2, 0/101, f], [2, 0/100, f]).
step_down([2, 0/102, f], [2, 0/101, f]).
step_down([2, 0/103, f], [2, 0/102, f]).
step_down([2, 0/104, f], [2, 0/103, f]).
step_down([2, 0/105, f], [2, 0/104, f]).
step_down([2, 0/106, f], [2, 0/105, f]).
step_down([2, 0/107, f], [2, 0/106, f]).
step_down([2, 0/108, f], [2, 0/107, f]).
step_down([2, 0/109, f], [2, 0/108, f]).
step_down([2, 0/110, f], [2, 0/109, f]).
step_down([2, 0/113, f], [2, 0/112, f]).
step_down([2, 0/114, f], [2, 0/113, f]).
step_down([2, 0/115, f], [2, 0/114, f]).
step_down([2, 0/116, f], [2, 0/115, f]).
step_down([2, 0/117, f], [2, 0/116, f]).
step_down([2, 0/118, s], [2, 0/117, f]).
step_down([2, 2/1, f], [2, 2/0, f]).
step_down([2, 2/2, f], [2, 2/1, f]).
step_down([2, 2/3, f], [2, 2/2, f]).
step_down([2, 2/4, f], [2, 2/3, f]).
step_down([2, 2/7, f], [2, 2/6, f]).
step_down([2, 2/8, f], [2, 2/7, f]).
step_down([2, 2/9, f], [2, 2/8, f]).
step_down([2, 2/10, f], [2, 2/9, f]).
step_down([2, 2/17, f], [2, 2/16, f]).
step_down([2, 2/18, f], [2, 2/17, f]).
step_down([2, 2/19, f], [2, 2/18, f]).
step_down([2, 2/20, f], [2, 2/19, f]).
step_down([2, 2/25, f], [2, 2/24, f]).
step_down([2, 2/26, f], [2, 2/25, f]).
step_down([2, 2/27, f], [2, 2/26, f]).
step_down([2, 2/28, f], [2, 2/27, f]).
step_down([2, 2/33, f], [2, 2/32, f]).
step_down([2, 2/34, f], [2, 2/33, f]).
step_down([2, 2/37, f], [2, 2/36, f]).
step_down([2, 2/38, f], [2, 2/37, f]).
step_down([2, 2/41, f], [2, 2/40, f]).
step_down([2, 2/42, f], [2, 2/41, f]).
step_down([2, 2/43, f], [2, 2/42, f]).
step_down([2, 2/44, f], [2, 2/43, f]).
step_down([2, 2/53, f], [2, 2/52, f]).
step_down([2, 2/54, f], [2, 2/53, f]).
step_down([2, 2/55, f], [2, 2/54, f]).
step_down([2, 2/56, f], [2, 2/55, f]).
step_down([2, 2/61, f], [2, 2/60, f]).
step_down([2, 2/62, f], [2, 2/61, f]).
step_down([2, 2/67, f], [2, 2/66, f]).
step_down([2, 2/68, f], [2, 2/67, f]).
step_down([2, 2/69, f], [2, 2/68, f]).
step_down([2, 2/70, f], [2, 2/69, f]).
step_down([2, 2/71, f], [2, 2/70, f]).
step_down([2, 2/72, f], [2, 2/71, f]).
step_down([2, 2/75, f], [2, 2/74, f]).
step_down([2, 2/76, f], [2, 2/75, f]).
step_down([2, 2/79, f], [2, 2/78, f]).
step_down([2, 2/80, f], [2, 2/79, f]).
step_down([2, 2/85, f], [2, 2/84, f]).
step_down([2, 2/86, f], [2, 2/85, f]).
step_down([2, 2/87, f], [2, 2/86, f]).
step_down([2, 2/88, f], [2, 2/87, f]).
step_down([2, 2/91, f], [2, 2/90, f]).
step_down([2, 2/92, f], [2, 2/91, f]).
step_down([2, 2/93, f], [2, 2/92, f]).
step_down([2, 2/94, f], [2, 2/93, f]).
step_down([2, 2/97, f], [2, 2/96, f]).
step_down([2, 2/98, f], [2, 2/97, f]).
step_down([2, 2/105, f], [2, 2/104, f]).
step_down([2, 2/106, f], [2, 2/105, f]).
step_down([2, 2/109, f], [2, 2/108, f]).
step_down([2, 2/110, f], [2, 2/109, f]).
step_down([2, 2/111, f], [2, 2/110, f]).
step_down([2, 2/112, f], [2, 2/111, f]).
step_down([2, 2/115, f], [2, 2/114, f]).
step_down([2, 2/116, f], [2, 2/115, f]).
step_down([2, 2/117, f], [2, 2/116, f]).
step_down([2, 2/118, f], [2, 2/117, f]).
step_down([2, 4/3, f], [2, 4/2, f]).
step_down([2, 4/4, f], [2, 4/3, f]).
step_down([2, 4/9, f], [2, 4/8, f]).
step_down([2, 4/10, f], [2, 4/9, f]).
step_down([2, 4/15, f], [2, 4/14, f]).
step_down([2, 4/16, f], [2, 4/15, f]).
step_down([2, 4/17, f], [2, 4/16, f]).
step_down([2, 4/18, f], [2, 4/17, f]).
step_down([2, 4/23, f], [2, 4/22, f]).
step_down([2, 4/24, f], [2, 4/23, f]).
step_down([2, 4/25, f], [2, 4/24, f]).
step_down([2, 4/26, f], [2, 4/25, f]).
step_down([2, 4/33, f], [2, 4/32, f]).
step_down([2, 4/34, f], [2, 4/33, f]).
step_down([2, 4/35, f], [2, 4/34, f]).
step_down([2, 4/36, f], [2, 4/35, f]).
step_down([2, 4/37, f], [2, 4/36, f]).
step_down([2, 4/38, f], [2, 4/37, f]).
step_down([2, 4/39, f], [2, 4/38, f]).
step_down([2, 4/40, f], [2, 4/39, f]).
step_down([2, 4/41, f], [2, 4/40, f]).
step_down([2, 4/42, f], [2, 4/41, f]).
step_down([2, 4/43, f], [2, 4/42, f]).
step_down([2, 4/44, f], [2, 4/43, f]).
step_down([2, 4/49, f], [2, 4/48, f]).
step_down([2, 4/50, f], [2, 4/49, f]).
step_down([2, 4/55, f], [2, 4/54, f]).
step_down([2, 4/56, f], [2, 4/55, f]).
step_down([2, 4/57, f], [2, 4/56, f]).
step_down([2, 4/58, f], [2, 4/57, f]).
step_down([2, 4/69, f], [2, 4/68, f]).
step_down([2, 4/70, f], [2, 4/69, f]).
step_down([2, 4/71, f], [2, 4/70, f]).
step_down([2, 4/72, f], [2, 4/71, f]).
step_down([2, 4/73, f], [2, 4/72, f]).
step_down([2, 4/74, f], [2, 4/73, f]).
step_down([2, 4/77, f], [2, 4/76, f]).
step_down([2, 4/78, f], [2, 4/77, f]).
step_down([2, 4/85, f], [2, 4/84, f]).
step_down([2, 4/86, f], [2, 4/85, f]).
step_down([2, 4/91, f], [2, 4/90, f]).
step_down([2, 4/92, f], [2, 4/91, f]).
step_down([2, 4/95, f], [2, 4/94, f]).
step_down([2, 4/96, f], [2, 4/95, f]).
step_down([2, 4/99, f], [2, 4/98, f]).
step_down([2, 4/100, f], [2, 4/99, f]).
step_down([2, 4/105, f], [2, 4/104, f]).
step_down([2, 4/106, f], [2, 4/105, f]).
step_down([2, 4/109, f], [2, 4/108, f]).
step_down([2, 4/110, f], [2, 4/109, f]).
step_down([2, 4/113, f], [2, 4/112, f]).
step_down([2, 4/114, f], [2, 4/113, f]).
step_down([2, 6/3, f], [2, 6/2, f]).
step_down([2, 6/4, f], [2, 6/3, f]).
step_down([2, 6/7, f], [2, 6/6, f]).
step_down([2, 6/8, f], [2, 6/7, f]).
step_down([2, 6/11, f], [2, 6/10, f]).
step_down([2, 6/12, f], [2, 6/11, f]).
step_down([2, 6/15, f], [2, 6/14, f]).
step_down([2, 6/16, f], [2, 6/15, f]).
step_down([2, 6/19, f], [2, 6/18, f]).
step_down([2, 6/20, f], [2, 6/19, f]).
step_down([2, 6/21, f], [2, 6/20, f]).
step_down([2, 6/22, f], [2, 6/21, f]).
step_down([2, 6/23, f], [2, 6/22, f]).
step_down([2, 6/24, f], [2, 6/23, f]).
step_down([2, 6/27, f], [2, 6/26, f]).
step_down([2, 6/28, f], [2, 6/27, f]).
step_down([2, 6/33, f], [2, 6/32, f]).
step_down([2, 6/34, f], [2, 6/33, f]).
step_down([2, 6/35, f], [2, 6/34, f]).
step_down([2, 6/36, f], [2, 6/35, f]).
step_down([2, 6/39, f], [2, 6/38, f]).
step_down([2, 6/40, f], [2, 6/39, f]).
step_down([2, 6/43, f], [2, 6/42, f]).
step_down([2, 6/44, f], [2, 6/43, f]).
step_down([2, 6/47, f], [2, 6/46, f]).
step_down([2, 6/48, f], [2, 6/47, f]).
step_down([2, 6/49, f], [2, 6/48, f]).
step_down([2, 6/50, f], [2, 6/49, f]).
step_down([2, 6/53, f], [2, 6/52, f]).
step_down([2, 6/54, f], [2, 6/53, f]).
step_down([2, 6/59, f], [2, 6/58, f]).
step_down([2, 6/60, f], [2, 6/59, f]).
step_down([2, 6/63, f], [2, 6/62, f]).
step_down([2, 6/64, f], [2, 6/63, f]).
step_down([2, 6/65, f], [2, 6/64, f]).
step_down([2, 6/66, f], [2, 6/65, f]).
step_down([2, 6/69, f], [2, 6/68, f]).
step_down([2, 6/70, f], [2, 6/69, f]).
step_down([2, 6/71, f], [2, 6/70, f]).
step_down([2, 6/72, f], [2, 6/71, f]).
step_down([2, 6/73, f], [2, 6/72, f]).
step_down([2, 6/74, f], [2, 6/73, f]).
step_down([2, 6/75, f], [2, 6/74, f]).
step_down([2, 6/76, f], [2, 6/75, f]).
step_down([2, 6/81, f], [2, 6/80, f]).
step_down([2, 6/82, f], [2, 6/81, f]).
step_down([2, 6/91, f], [2, 6/90, f]).
step_down([2, 6/92, f], [2, 6/91, f]).
step_down([2, 6/95, f], [2, 6/94, f]).
step_down([2, 6/96, f], [2, 6/95, f]).
step_down([2, 6/97, f], [2, 6/96, f]).
step_down([2, 6/98, f], [2, 6/97, f]).
step_down([2, 6/99, f], [2, 6/98, f]).
step_down([2, 6/100, f], [2, 6/99, f]).
step_down([2, 6/101, f], [2, 6/100, f]).
step_down([2, 6/102, f], [2, 6/101, f]).
step_down([2, 6/109, f], [2, 6/108, f]).
step_down([2, 6/110, f], [2, 6/109, f]).
step_down([2, 6/115, f], [2, 6/114, f]).
step_down([2, 6/116, f], [2, 6/115, f]).
step_down([2, 6/117, f], [2, 6/116, f]).
step_down([2, 6/118, f], [2, 6/117, f]).
step_down([2, 8/3, f], [2, 8/2, f]).
step_down([2, 8/4, f], [2, 8/3, f]).
step_down([2, 8/7, f], [2, 8/6, f]).
step_down([2, 8/8, f], [2, 8/7, f]).
step_down([2, 8/11, f], [2, 8/10, f]).
step_down([2, 8/12, f], [2, 8/11, f]).
step_down([2, 8/13, f], [2, 8/12, f]).
step_down([2, 8/14, f], [2, 8/13, f]).
step_down([2, 8/19, f], [2, 8/18, f]).
step_down([2, 8/20, f], [2, 8/19, f]).
step_down([2, 8/23, f], [2, 8/22, f]).
step_down([2, 8/24, f], [2, 8/23, f]).
step_down([2, 8/25, f], [2, 8/24, f]).
step_down([2, 8/26, f], [2, 8/25, f]).
step_down([2, 8/27, f], [2, 8/26, f]).
step_down([2, 8/28, f], [2, 8/27, f]).
step_down([2, 8/29, f], [2, 8/28, f]).
step_down([2, 8/30, f], [2, 8/29, f]).
step_down([2, 8/33, f], [2, 8/32, f]).
step_down([2, 8/34, f], [2, 8/33, f]).
step_down([2, 8/37, f], [2, 8/36, f]).
step_down([2, 8/38, f], [2, 8/37, f]).
step_down([2, 8/43, f], [2, 8/42, f]).
step_down([2, 8/44, f], [2, 8/43, f]).
step_down([2, 8/49, f], [2, 8/48, f]).
step_down([2, 8/50, f], [2, 8/49, f]).
step_down([2, 8/51, f], [2, 8/50, f]).
step_down([2, 8/52, f], [2, 8/51, f]).
step_down([2, 8/55, f], [2, 8/54, f]).
step_down([2, 8/56, f], [2, 8/55, f]).
step_down([2, 8/61, f], [2, 8/60, f]).
step_down([2, 8/62, f], [2, 8/61, f]).
step_down([2, 8/65, f], [2, 8/64, f]).
step_down([2, 8/66, f], [2, 8/65, f]).
step_down([2, 8/67, f], [2, 8/66, f]).
step_down([2, 8/68, f], [2, 8/67, f]).
step_down([2, 8/69, f], [2, 8/68, f]).
step_down([2, 8/70, f], [2, 8/69, f]).
step_down([2, 8/71, f], [2, 8/70, f]).
step_down([2, 8/72, f], [2, 8/71, f]).
step_down([2, 8/73, f], [2, 8/72, f]).
step_down([2, 8/74, f], [2, 8/73, f]).
step_down([2, 8/75, f], [2, 8/74, f]).
step_down([2, 8/76, f], [2, 8/75, f]).
step_down([2, 8/81, f], [2, 8/80, f]).
step_down([2, 8/82, f], [2, 8/81, f]).
step_down([2, 8/91, f], [2, 8/90, f]).
step_down([2, 8/92, f], [2, 8/91, f]).
step_down([2, 8/93, f], [2, 8/92, f]).
step_down([2, 8/94, f], [2, 8/93, f]).
step_down([2, 8/95, f], [2, 8/94, f]).
step_down([2, 8/96, f], [2, 8/95, f]).
step_down([2, 8/97, f], [2, 8/96, f]).
step_down([2, 8/98, f], [2, 8/97, f]).
step_down([2, 8/103, f], [2, 8/102, f]).
step_down([2, 8/104, f], [2, 8/103, f]).
step_down([2, 8/105, f], [2, 8/104, f]).
step_down([2, 8/106, f], [2, 8/105, f]).
step_down([2, 8/111, f], [2, 8/110, f]).
step_down([2, 8/112, f], [2, 8/111, f]).
step_down([2, 8/115, f], [2, 8/114, f]).
step_down([2, 8/116, f], [2, 8/115, f]).
step_down([2, 8/117, f], [2, 8/116, f]).
step_down([2, 8/118, f], [2, 8/117, f]).
step_down([2, 10/1, f], [2, 10/0, f]).
step_down([2, 10/2, f], [2, 10/1, f]).
step_down([2, 10/5, f], [2, 10/4, f]).
step_down([2, 10/6, f], [2, 10/5, f]).
step_down([2, 10/11, f], [2, 10/10, f]).
step_down([2, 10/12, f], [2, 10/11, f]).
step_down([2, 10/15, f], [2, 10/14, f]).
step_down([2, 10/16, f], [2, 10/15, f]).
step_down([2, 10/21, f], [2, 10/20, f]).
step_down([2, 10/22, f], [2, 10/21, f]).
step_down([2, 10/25, f], [2, 10/24, f]).
step_down([2, 10/26, f], [2, 10/25, f]).
step_down([2, 10/29, f], [2, 10/28, f]).
step_down([2, 10/30, f], [2, 10/29, f]).
step_down([2, 10/31, f], [2, 10/30, f]).
step_down([2, 10/32, f], [2, 10/31, f]).
step_down([2, 10/37, f], [2, 10/36, f]).
step_down([2, 10/38, f], [2, 10/37, f]).
step_down([2, 10/41, f], [2, 10/40, f]).
step_down([2, 10/42, f], [2, 10/41, f]).
step_down([2, 10/43, f], [2, 10/42, f]).
step_down([2, 10/44, f], [2, 10/43, f]).
step_down([2, 10/47, f], [2, 10/46, f]).
step_down([2, 10/48, f], [2, 10/47, f]).
step_down([2, 10/49, f], [2, 10/48, f]).
step_down([2, 10/50, f], [2, 10/49, f]).
step_down([2, 10/53, f], [2, 10/52, f]).
step_down([2, 10/54, f], [2, 10/53, f]).
step_down([2, 10/59, f], [2, 10/58, f]).
step_down([2, 10/60, f], [2, 10/59, f]).
step_down([2, 10/65, f], [2, 10/64, f]).
step_down([2, 10/66, f], [2, 10/65, f]).
step_down([2, 10/67, f], [2, 10/66, f]).
step_down([2, 10/68, f], [2, 10/67, f]).
step_down([2, 10/69, f], [2, 10/68, f]).
step_down([2, 10/70, f], [2, 10/69, f]).
step_down([2, 10/71, f], [2, 10/70, f]).
step_down([2, 10/72, f], [2, 10/71, f]).
step_down([2, 10/73, f], [2, 10/72, f]).
step_down([2, 10/74, f], [2, 10/73, f]).
step_down([2, 10/79, f], [2, 10/78, f]).
step_down([2, 10/80, f], [2, 10/79, f]).
step_down([2, 10/81, f], [2, 10/80, f]).
step_down([2, 10/82, f], [2, 10/81, f]).
step_down([2, 10/87, f], [2, 10/86, f]).
step_down([2, 10/88, f], [2, 10/87, f]).
step_down([2, 10/91, f], [2, 10/90, f]).
step_down([2, 10/92, f], [2, 10/91, f]).
step_down([2, 10/97, f], [2, 10/96, f]).
step_down([2, 10/98, f], [2, 10/97, f]).
step_down([2, 10/101, f], [2, 10/100, f]).
step_down([2, 10/102, f], [2, 10/101, f]).
step_down([2, 10/107, f], [2, 10/106, f]).
step_down([2, 10/108, f], [2, 10/107, f]).
step_down([2, 10/111, f], [2, 10/110, f]).
step_down([2, 10/112, f], [2, 10/111, f]).
step_down([2, 10/115, f], [2, 10/114, f]).
step_down([2, 10/116, f], [2, 10/115, f]).
step_down([2, 10/117, f], [2, 10/116, f]).
step_down([2, 10/118, f], [2, 10/117, f]).
step_down([2, 12/3, f], [2, 12/2, f]).
step_down([2, 12/4, f], [2, 12/3, f]).
step_down([2, 12/5, f], [2, 12/4, f]).
step_down([2, 12/6, f], [2, 12/5, f]).
step_down([2, 12/9, f], [2, 12/8, f]).
step_down([2, 12/10, f], [2, 12/9, f]).
step_down([2, 12/15, f], [2, 12/14, f]).
step_down([2, 12/16, f], [2, 12/15, f]).
step_down([2, 12/21, f], [2, 12/20, f]).
step_down([2, 12/22, f], [2, 12/21, f]).
step_down([2, 12/31, f], [2, 12/30, f]).
step_down([2, 12/32, f], [2, 12/31, f]).
step_down([2, 12/35, f], [2, 12/34, f]).
step_down([2, 12/36, f], [2, 12/35, f]).
step_down([2, 12/39, f], [2, 12/38, f]).
step_down([2, 12/40, f], [2, 12/39, f]).
step_down([2, 12/45, f], [2, 12/44, f]).
step_down([2, 12/46, f], [2, 12/45, f]).
step_down([2, 12/47, f], [2, 12/46, f]).
step_down([2, 12/48, f], [2, 12/47, f]).
step_down([2, 12/51, f], [2, 12/50, f]).
step_down([2, 12/52, f], [2, 12/51, f]).
step_down([2, 12/53, f], [2, 12/52, f]).
step_down([2, 12/54, f], [2, 12/53, f]).
step_down([2, 12/59, f], [2, 12/58, f]).
step_down([2, 12/60, f], [2, 12/59, f]).
step_down([2, 12/63, f], [2, 12/62, f]).
step_down([2, 12/64, f], [2, 12/63, f]).
step_down([2, 12/69, f], [2, 12/68, f]).
step_down([2, 12/70, f], [2, 12/69, f]).
step_down([2, 12/71, f], [2, 12/70, f]).
step_down([2, 12/72, f], [2, 12/71, f]).
step_down([2, 12/75, f], [2, 12/74, f]).
step_down([2, 12/76, f], [2, 12/75, f]).
step_down([2, 12/79, f], [2, 12/78, f]).
step_down([2, 12/80, f], [2, 12/79, f]).
step_down([2, 12/81, f], [2, 12/80, f]).
step_down([2, 12/82, f], [2, 12/81, f]).
step_down([2, 12/87, f], [2, 12/86, f]).
step_down([2, 12/88, f], [2, 12/87, f]).
step_down([2, 12/93, f], [2, 12/92, f]).
step_down([2, 12/94, f], [2, 12/93, f]).
step_down([2, 12/99, f], [2, 12/98, f]).
step_down([2, 12/100, f], [2, 12/99, f]).
step_down([2, 12/103, f], [2, 12/102, f]).
step_down([2, 12/104, f], [2, 12/103, f]).
step_down([2, 12/109, f], [2, 12/108, f]).
step_down([2, 12/110, f], [2, 12/109, f]).
step_down([2, 12/113, f], [2, 12/112, f]).
step_down([2, 12/114, f], [2, 12/113, f]).
step_down([2, 12/115, f], [2, 12/114, f]).
step_down([2, 12/116, f], [2, 12/115, f]).
step_down([2, 14/3, f], [2, 14/2, f]).
step_down([2, 14/4, f], [2, 14/3, f]).
step_down([2, 14/5, f], [2, 14/4, f]).
step_down([2, 14/6, f], [2, 14/5, f]).
step_down([2, 14/9, f], [2, 14/8, f]).
step_down([2, 14/10, f], [2, 14/9, f]).
step_down([2, 14/11, f], [2, 14/10, f]).
step_down([2, 14/12, f], [2, 14/11, f]).
step_down([2, 14/13, f], [2, 14/12, f]).
step_down([2, 14/14, f], [2, 14/13, f]).
step_down([2, 14/21, f], [2, 14/20, f]).
step_down([2, 14/22, f], [2, 14/21, f]).
step_down([2, 14/33, f], [2, 14/32, f]).
step_down([2, 14/34, f], [2, 14/33, f]).
step_down([2, 14/39, f], [2, 14/38, f]).
step_down([2, 14/40, f], [2, 14/39, f]).
step_down([2, 14/43, f], [2, 14/42, f]).
step_down([2, 14/44, f], [2, 14/43, f]).
step_down([2, 14/49, f], [2, 14/48, f]).
step_down([2, 14/50, f], [2, 14/49, f]).
step_down([2, 14/53, f], [2, 14/52, f]).
step_down([2, 14/54, f], [2, 14/53, f]).
step_down([2, 14/55, f], [2, 14/54, f]).
step_down([2, 14/56, f], [2, 14/55, f]).
step_down([2, 14/61, f], [2, 14/60, f]).
step_down([2, 14/62, f], [2, 14/61, f]).
step_down([2, 14/67, f], [2, 14/66, f]).
step_down([2, 14/68, f], [2, 14/67, f]).
step_down([2, 14/71, f], [2, 14/70, f]).
step_down([2, 14/72, f], [2, 14/71, f]).
step_down([2, 14/73, f], [2, 14/72, f]).
step_down([2, 14/74, f], [2, 14/73, f]).
step_down([2, 14/75, f], [2, 14/74, f]).
step_down([2, 14/76, f], [2, 14/75, f]).
step_down([2, 14/85, f], [2, 14/84, f]).
step_down([2, 14/86, f], [2, 14/85, f]).
step_down([2, 14/91, f], [2, 14/90, f]).
step_down([2, 14/92, f], [2, 14/91, f]).
step_down([2, 14/95, f], [2, 14/94, f]).
step_down([2, 14/96, f], [2, 14/95, f]).
step_down([2, 14/101, f], [2, 14/100, f]).
step_down([2, 14/102, f], [2, 14/101, f]).
step_down([2, 14/107, f], [2, 14/106, f]).
step_down([2, 14/108, f], [2, 14/107, f]).
step_down([2, 16/3, f], [2, 16/2, f]).
step_down([2, 16/4, f], [2, 16/3, f]).
step_down([2, 16/7, f], [2, 16/6, f]).
step_down([2, 16/8, f], [2, 16/7, f]).
step_down([2, 16/9, f], [2, 16/8, f]).
step_down([2, 16/10, f], [2, 16/9, f]).
step_down([2, 16/13, f], [2, 16/12, f]).
step_down([2, 16/14, f], [2, 16/13, f]).
step_down([2, 16/15, f], [2, 16/14, f]).
step_down([2, 16/16, f], [2, 16/15, f]).
step_down([2, 16/23, f], [2, 16/22, f]).
step_down([2, 16/24, f], [2, 16/23, f]).
step_down([2, 16/29, f], [2, 16/28, f]).
step_down([2, 16/30, f], [2, 16/29, f]).
step_down([2, 16/37, f], [2, 16/36, f]).
step_down([2, 16/38, f], [2, 16/37, f]).
step_down([2, 16/41, f], [2, 16/40, f]).
step_down([2, 16/42, f], [2, 16/41, f]).
step_down([2, 16/45, f], [2, 16/44, f]).
step_down([2, 16/46, f], [2, 16/45, f]).
step_down([2, 16/55, f], [2, 16/54, f]).
step_down([2, 16/56, f], [2, 16/55, f]).
step_down([2, 16/57, f], [2, 16/56, f]).
step_down([2, 16/58, f], [2, 16/57, f]).
step_down([2, 16/61, f], [2, 16/60, f]).
step_down([2, 16/62, f], [2, 16/61, f]).
step_down([2, 16/63, f], [2, 16/62, f]).
step_down([2, 16/64, f], [2, 16/63, f]).
step_down([2, 16/69, f], [2, 16/68, f]).
step_down([2, 16/70, f], [2, 16/69, f]).
step_down([2, 16/71, f], [2, 16/70, f]).
step_down([2, 16/72, f], [2, 16/71, f]).
step_down([2, 16/73, f], [2, 16/72, f]).
step_down([2, 16/74, f], [2, 16/73, f]).
step_down([2, 16/75, f], [2, 16/74, f]).
step_down([2, 16/76, f], [2, 16/75, f]).
step_down([2, 16/77, f], [2, 16/76, f]).
step_down([2, 16/78, f], [2, 16/77, f]).
step_down([2, 16/81, f], [2, 16/80, f]).
step_down([2, 16/82, f], [2, 16/81, f]).
step_down([2, 16/83, f], [2, 16/82, f]).
step_down([2, 16/84, f], [2, 16/83, f]).
step_down([2, 16/87, f], [2, 16/86, f]).
step_down([2, 16/88, f], [2, 16/87, f]).
step_down([2, 16/89, f], [2, 16/88, f]).
step_down([2, 16/90, f], [2, 16/89, f]).
step_down([2, 16/93, f], [2, 16/92, f]).
step_down([2, 16/94, f], [2, 16/93, f]).
step_down([2, 16/95, f], [2, 16/94, f]).
step_down([2, 16/96, f], [2, 16/95, f]).
step_down([2, 16/101, f], [2, 16/100, f]).
step_down([2, 16/102, f], [2, 16/101, f]).
step_down([2, 16/105, f], [2, 16/104, f]).
step_down([2, 16/106, f], [2, 16/105, f]).
step_down([2, 16/107, f], [2, 16/106, f]).
step_down([2, 16/108, f], [2, 16/107, f]).
step_down([2, 16/109, f], [2, 16/108, f]).
step_down([2, 16/110, f], [2, 16/109, f]).
step_down([2, 16/111, f], [2, 16/110, f]).
step_down([2, 16/112, f], [2, 16/111, f]).
step_down([2, 16/115, f], [2, 16/114, f]).
step_down([2, 16/116, f], [2, 16/115, f]).
step_down([2, 16/117, f], [2, 16/116, f]).
step_down([2, 16/118, f], [2, 16/117, f]).
step_down([2, 18/1, f], [2, 18/0, f]).
step_down([2, 18/2, f], [2, 18/1, f]).
step_down([2, 18/7, f], [2, 18/6, f]).
step_down([2, 18/8, f], [2, 18/7, f]).
step_down([2, 18/11, f], [2, 18/10, f]).
step_down([2, 18/12, f], [2, 18/11, f]).
step_down([2, 18/15, f], [2, 18/14, f]).
step_down([2, 18/16, f], [2, 18/15, f]).
step_down([2, 18/21, f], [2, 18/20, f]).
step_down([2, 18/22, f], [2, 18/21, f]).
step_down([2, 18/27, f], [2, 18/26, f]).
step_down([2, 18/28, f], [2, 18/27, f]).
step_down([2, 18/31, f], [2, 18/30, f]).
step_down([2, 18/32, f], [2, 18/31, f]).
step_down([2, 18/35, f], [2, 18/34, f]).
step_down([2, 18/36, f], [2, 18/35, f]).
step_down([2, 18/41, f], [2, 18/40, f]).
step_down([2, 18/42, f], [2, 18/41, f]).
step_down([2, 18/45, f], [2, 18/44, f]).
step_down([2, 18/46, f], [2, 18/45, f]).
step_down([2, 18/47, f], [2, 18/46, f]).
step_down([2, 18/48, f], [2, 18/47, f]).
step_down([2, 18/51, f], [2, 18/50, f]).
step_down([2, 18/52, f], [2, 18/51, f]).
step_down([2, 18/55, f], [2, 18/54, f]).
step_down([2, 18/56, f], [2, 18/55, f]).
step_down([2, 18/63, f], [2, 18/62, f]).
step_down([2, 18/64, f], [2, 18/63, f]).
step_down([2, 18/65, f], [2, 18/64, f]).
step_down([2, 18/66, f], [2, 18/65, f]).
step_down([2, 18/73, f], [2, 18/72, f]).
step_down([2, 18/74, f], [2, 18/73, f]).
step_down([2, 18/75, f], [2, 18/74, f]).
step_down([2, 18/76, f], [2, 18/75, f]).
step_down([2, 18/77, f], [2, 18/76, f]).
step_down([2, 18/78, f], [2, 18/77, f]).
step_down([2, 18/81, f], [2, 18/80, f]).
step_down([2, 18/82, f], [2, 18/81, f]).
step_down([2, 18/83, f], [2, 18/82, f]).
step_down([2, 18/84, f], [2, 18/83, f]).
step_down([2, 18/85, f], [2, 18/84, f]).
step_down([2, 18/86, f], [2, 18/85, f]).
step_down([2, 18/89, f], [2, 18/88, f]).
step_down([2, 18/90, f], [2, 18/89, f]).
step_down([2, 18/95, f], [2, 18/94, f]).
step_down([2, 18/96, f], [2, 18/95, f]).
step_down([2, 18/99, f], [2, 18/98, f]).
step_down([2, 18/100, f], [2, 18/99, f]).
step_down([2, 18/103, f], [2, 18/102, f]).
step_down([2, 18/104, f], [2, 18/103, f]).
step_down([2, 18/107, f], [2, 18/106, f]).
step_down([2, 18/108, f], [2, 18/107, f]).
step_down([2, 18/109, f], [2, 18/108, f]).
step_down([2, 18/110, f], [2, 18/109, f]).
step_down([2, 18/111, f], [2, 18/110, f]).
step_down([2, 18/112, f], [2, 18/111, f]).
step_down([2, 18/113, f], [2, 18/112, f]).
step_down([2, 18/114, f], [2, 18/113, f]).
step_down([2, 18/115, f], [2, 18/114, f]).
step_down([2, 18/116, f], [2, 18/115, f]).
step_down([2, 18/117, f], [2, 18/116, f]).
step_down([2, 18/118, f], [2, 18/117, f]).
step_down([2, 20/5, f], [2, 20/4, f]).
step_down([2, 20/6, f], [2, 20/5, f]).
step_down([2, 20/9, f], [2, 20/8, f]).
step_down([2, 20/10, f], [2, 20/9, f]).
step_down([2, 20/21, f], [2, 20/20, f]).
step_down([2, 20/22, f], [2, 20/21, f]).
step_down([2, 20/25, f], [2, 20/24, f]).
step_down([2, 20/26, f], [2, 20/25, f]).
step_down([2, 20/27, f], [2, 20/26, f]).
step_down([2, 20/28, f], [2, 20/27, f]).
step_down([2, 20/31, f], [2, 20/30, f]).
step_down([2, 20/32, f], [2, 20/31, f]).
step_down([2, 20/37, f], [2, 20/36, f]).
step_down([2, 20/38, f], [2, 20/37, f]).
step_down([2, 20/39, f], [2, 20/38, f]).
step_down([2, 20/40, f], [2, 20/39, f]).
step_down([2, 20/43, f], [2, 20/42, f]).
step_down([2, 20/44, f], [2, 20/43, f]).
step_down([2, 20/47, f], [2, 20/46, f]).
step_down([2, 20/48, f], [2, 20/47, f]).
step_down([2, 20/49, f], [2, 20/48, f]).
step_down([2, 20/50, f], [2, 20/49, f]).
step_down([2, 20/53, f], [2, 20/52, f]).
step_down([2, 20/54, f], [2, 20/53, f]).
step_down([2, 20/57, f], [2, 20/56, f]).
step_down([2, 20/58, f], [2, 20/57, f]).
step_down([2, 20/59, f], [2, 20/58, f]).
step_down([2, 20/60, f], [2, 20/59, f]).
step_down([2, 20/65, f], [2, 20/64, f]).
step_down([2, 20/66, f], [2, 20/65, f]).
step_down([2, 20/71, f], [2, 20/70, f]).
step_down([2, 20/72, f], [2, 20/71, f]).
step_down([2, 20/75, f], [2, 20/74, f]).
step_down([2, 20/76, f], [2, 20/75, f]).
step_down([2, 20/77, f], [2, 20/76, f]).
step_down([2, 20/78, f], [2, 20/77, f]).
step_down([2, 20/79, f], [2, 20/78, f]).
step_down([2, 20/80, f], [2, 20/79, f]).
step_down([2, 20/89, f], [2, 20/88, f]).
step_down([2, 20/90, f], [2, 20/89, f]).
step_down([2, 20/97, f], [2, 20/96, f]).
step_down([2, 20/98, f], [2, 20/97, f]).
step_down([2, 20/101, f], [2, 20/100, f]).
step_down([2, 20/102, f], [2, 20/101, f]).
step_down([2, 20/105, f], [2, 20/104, f]).
step_down([2, 20/106, f], [2, 20/105, f]).
step_down([2, 20/107, f], [2, 20/106, f]).
step_down([2, 20/108, f], [2, 20/107, f]).
step_down([2, 20/109, f], [2, 20/108, f]).
step_down([2, 20/110, f], [2, 20/109, f]).
step_down([2, 20/113, f], [2, 20/112, f]).
step_down([2, 20/114, f], [2, 20/113, f]).
step_down([2, 20/115, f], [2, 20/114, f]).
step_down([2, 20/116, f], [2, 20/115, f]).
step_down([2, 20/117, f], [2, 20/116, f]).
step_down([2, 20/118, f], [2, 20/117, f]).
step_down([2, 22/7, f], [2, 22/6, f]).
step_down([2, 22/8, f], [2, 22/7, f]).
step_down([2, 22/11, f], [2, 22/10, f]).
step_down([2, 22/12, f], [2, 22/11, f]).
step_down([2, 22/17, f], [2, 22/16, f]).
step_down([2, 22/18, f], [2, 22/17, f]).
step_down([2, 22/25, f], [2, 22/24, f]).
step_down([2, 22/26, f], [2, 22/25, f]).
step_down([2, 22/29, f], [2, 22/28, f]).
step_down([2, 22/30, f], [2, 22/29, f]).
step_down([2, 22/37, f], [2, 22/36, f]).
step_down([2, 22/38, f], [2, 22/37, f]).
step_down([2, 22/41, f], [2, 22/40, f]).
step_down([2, 22/42, f], [2, 22/41, f]).
step_down([2, 22/43, f], [2, 22/42, f]).
step_down([2, 22/44, f], [2, 22/43, f]).
step_down([2, 22/45, f], [2, 22/44, f]).
step_down([2, 22/46, f], [2, 22/45, f]).
step_down([2, 22/51, f], [2, 22/50, f]).
step_down([2, 22/52, f], [2, 22/51, f]).
step_down([2, 22/55, f], [2, 22/54, f]).
step_down([2, 22/56, f], [2, 22/55, f]).
step_down([2, 22/57, f], [2, 22/56, f]).
step_down([2, 22/58, f], [2, 22/57, f]).
step_down([2, 22/61, f], [2, 22/60, f]).
step_down([2, 22/62, f], [2, 22/61, f]).
step_down([2, 22/69, f], [2, 22/68, f]).
step_down([2, 22/70, f], [2, 22/69, f]).
step_down([2, 22/77, f], [2, 22/76, f]).
step_down([2, 22/78, f], [2, 22/77, f]).
step_down([2, 22/79, f], [2, 22/78, f]).
step_down([2, 22/80, f], [2, 22/79, f]).
step_down([2, 22/81, f], [2, 22/80, f]).
step_down([2, 22/82, f], [2, 22/81, f]).
step_down([2, 22/85, f], [2, 22/84, f]).
step_down([2, 22/86, f], [2, 22/85, f]).
step_down([2, 22/87, f], [2, 22/86, f]).
step_down([2, 22/88, f], [2, 22/87, f]).
step_down([2, 22/89, f], [2, 22/88, f]).
step_down([2, 22/90, f], [2, 22/89, f]).
step_down([2, 22/95, f], [2, 22/94, f]).
step_down([2, 22/96, f], [2, 22/95, f]).
step_down([2, 22/103, f], [2, 22/102, f]).
step_down([2, 22/104, f], [2, 22/103, f]).
step_down([2, 22/105, f], [2, 22/104, f]).
step_down([2, 22/106, f], [2, 22/105, f]).
step_down([2, 22/109, f], [2, 22/108, f]).
step_down([2, 22/110, f], [2, 22/109, f]).
step_down([2, 22/115, f], [2, 22/114, f]).
step_down([2, 22/116, f], [2, 22/115, f]).
step_down([2, 22/117, f], [2, 22/116, f]).
step_down([2, 22/118, f], [2, 22/117, f]).
step_down([2, 24/1, f], [2, 24/0, f]).
step_down([2, 24/2, f], [2, 24/1, f]).
step_down([2, 24/5, f], [2, 24/4, f]).
step_down([2, 24/6, f], [2, 24/5, f]).
step_down([2, 24/7, f], [2, 24/6, f]).
step_down([2, 24/8, f], [2, 24/7, f]).
step_down([2, 24/11, f], [2, 24/10, f]).
step_down([2, 24/12, f], [2, 24/11, f]).
step_down([2, 24/17, f], [2, 24/16, f]).
step_down([2, 24/18, f], [2, 24/17, f]).
step_down([2, 24/19, f], [2, 24/18, f]).
step_down([2, 24/20, f], [2, 24/19, f]).
step_down([2, 24/25, f], [2, 24/24, f]).
step_down([2, 24/26, f], [2, 24/25, f]).
step_down([2, 24/31, f], [2, 24/30, f]).
step_down([2, 24/32, f], [2, 24/31, f]).
step_down([2, 24/35, f], [2, 24/34, f]).
step_down([2, 24/36, f], [2, 24/35, f]).
step_down([2, 24/39, f], [2, 24/38, f]).
step_down([2, 24/40, f], [2, 24/39, f]).
step_down([2, 24/43, f], [2, 24/42, f]).
step_down([2, 24/44, f], [2, 24/43, f]).
step_down([2, 24/45, f], [2, 24/44, f]).
step_down([2, 24/46, f], [2, 24/45, f]).
step_down([2, 24/47, f], [2, 24/46, f]).
step_down([2, 24/48, f], [2, 24/47, f]).
step_down([2, 24/53, f], [2, 24/52, f]).
step_down([2, 24/54, f], [2, 24/53, f]).
step_down([2, 24/57, f], [2, 24/56, f]).
step_down([2, 24/58, f], [2, 24/57, f]).
step_down([2, 24/59, f], [2, 24/58, f]).
step_down([2, 24/60, f], [2, 24/59, f]).
step_down([2, 24/69, f], [2, 24/68, f]).
step_down([2, 24/70, f], [2, 24/69, f]).
step_down([2, 24/79, f], [2, 24/78, f]).
step_down([2, 24/80, f], [2, 24/79, f]).
step_down([2, 24/81, f], [2, 24/80, f]).
step_down([2, 24/82, f], [2, 24/81, f]).
step_down([2, 24/83, f], [2, 24/82, f]).
step_down([2, 24/84, f], [2, 24/83, f]).
step_down([2, 24/85, f], [2, 24/84, f]).
step_down([2, 24/86, f], [2, 24/85, f]).
step_down([2, 24/87, f], [2, 24/86, f]).
step_down([2, 24/88, f], [2, 24/87, f]).
step_down([2, 24/89, f], [2, 24/88, f]).
step_down([2, 24/90, f], [2, 24/89, f]).
step_down([2, 24/91, f], [2, 24/90, f]).
step_down([2, 24/92, f], [2, 24/91, f]).
step_down([2, 24/93, f], [2, 24/92, f]).
step_down([2, 24/94, f], [2, 24/93, f]).
step_down([2, 24/97, f], [2, 24/96, f]).
step_down([2, 24/98, f], [2, 24/97, f]).
step_down([2, 24/101, f], [2, 24/100, f]).
step_down([2, 24/102, f], [2, 24/101, f]).
step_down([2, 24/103, f], [2, 24/102, f]).
step_down([2, 24/104, f], [2, 24/103, f]).
step_down([2, 24/107, f], [2, 24/106, f]).
step_down([2, 24/108, f], [2, 24/107, f]).
step_down([2, 24/117, f], [2, 24/116, f]).
step_down([2, 24/118, f], [2, 24/117, f]).
step_down([2, 26/1, f], [2, 26/0, f]).
step_down([2, 26/2, f], [2, 26/1, f]).
step_down([2, 26/7, f], [2, 26/6, f]).
step_down([2, 26/8, f], [2, 26/7, f]).
step_down([2, 26/11, f], [2, 26/10, f]).
step_down([2, 26/12, f], [2, 26/11, f]).
step_down([2, 26/15, f], [2, 26/14, f]).
step_down([2, 26/16, f], [2, 26/15, f]).
step_down([2, 26/17, f], [2, 26/16, f]).
step_down([2, 26/18, f], [2, 26/17, f]).
step_down([2, 26/21, f], [2, 26/20, f]).
step_down([2, 26/22, f], [2, 26/21, f]).
step_down([2, 26/23, f], [2, 26/22, f]).
step_down([2, 26/24, f], [2, 26/23, f]).
step_down([2, 26/25, f], [2, 26/24, f]).
step_down([2, 26/26, f], [2, 26/25, f]).
step_down([2, 26/29, f], [2, 26/28, f]).
step_down([2, 26/30, f], [2, 26/29, f]).
step_down([2, 26/31, f], [2, 26/30, f]).
step_down([2, 26/32, f], [2, 26/31, f]).
step_down([2, 26/33, f], [2, 26/32, f]).
step_down([2, 26/34, f], [2, 26/33, f]).
step_down([2, 26/39, f], [2, 26/38, f]).
step_down([2, 26/40, f], [2, 26/39, f]).
step_down([2, 26/43, f], [2, 26/42, f]).
step_down([2, 26/44, f], [2, 26/43, f]).
step_down([2, 26/49, f], [2, 26/48, f]).
step_down([2, 26/50, f], [2, 26/49, f]).
step_down([2, 26/53, f], [2, 26/52, f]).
step_down([2, 26/54, f], [2, 26/53, f]).
step_down([2, 26/55, f], [2, 26/54, f]).
step_down([2, 26/56, f], [2, 26/55, f]).
step_down([2, 26/57, f], [2, 26/56, f]).
step_down([2, 26/58, f], [2, 26/57, f]).
step_down([2, 26/63, f], [2, 26/62, f]).
step_down([2, 26/64, f], [2, 26/63, f]).
step_down([2, 26/67, f], [2, 26/66, f]).
step_down([2, 26/68, f], [2, 26/67, f]).
step_down([2, 26/71, f], [2, 26/70, f]).
step_down([2, 26/72, f], [2, 26/71, f]).
step_down([2, 26/79, f], [2, 26/78, f]).
step_down([2, 26/80, f], [2, 26/79, f]).
step_down([2, 26/81, f], [2, 26/80, f]).
step_down([2, 26/82, f], [2, 26/81, f]).
step_down([2, 26/85, f], [2, 26/84, f]).
step_down([2, 26/86, f], [2, 26/85, f]).
step_down([2, 26/87, f], [2, 26/86, f]).
step_down([2, 26/88, f], [2, 26/87, f]).
step_down([2, 26/91, f], [2, 26/90, f]).
step_down([2, 26/92, f], [2, 26/91, f]).
step_down([2, 26/95, f], [2, 26/94, f]).
step_down([2, 26/96, f], [2, 26/95, f]).
step_down([2, 26/99, f], [2, 26/98, f]).
step_down([2, 26/100, f], [2, 26/99, f]).
step_down([2, 26/103, f], [2, 26/102, f]).
step_down([2, 26/104, f], [2, 26/103, f]).
step_down([2, 26/107, f], [2, 26/106, f]).
step_down([2, 26/108, f], [2, 26/107, f]).
step_down([2, 26/113, f], [2, 26/112, f]).
step_down([2, 26/114, f], [2, 26/113, f]).
step_down([2, 26/115, f], [2, 26/114, f]).
step_down([2, 26/116, f], [2, 26/115, f]).
step_down([2, 26/117, f], [2, 26/116, f]).
step_down([2, 26/118, f], [2, 26/117, f]).
step_down([2, 28/11, f], [2, 28/10, f]).
step_down([2, 28/12, f], [2, 28/11, f]).
step_down([2, 28/13, f], [2, 28/12, f]).
step_down([2, 28/14, f], [2, 28/13, f]).
step_down([2, 28/17, f], [2, 28/16, f]).
step_down([2, 28/18, f], [2, 28/17, f]).
step_down([2, 28/23, f], [2, 28/22, f]).
step_down([2, 28/24, f], [2, 28/23, f]).
step_down([2, 28/25, f], [2, 28/24, f]).
step_down([2, 28/26, f], [2, 28/25, f]).
step_down([2, 28/27, f], [2, 28/26, f]).
step_down([2, 28/28, f], [2, 28/27, f]).
step_down([2, 28/31, f], [2, 28/30, f]).
step_down([2, 28/32, f], [2, 28/31, f]).
step_down([2, 28/39, f], [2, 28/38, f]).
step_down([2, 28/40, f], [2, 28/39, f]).
step_down([2, 28/43, f], [2, 28/42, f]).
step_down([2, 28/44, f], [2, 28/43, f]).
step_down([2, 28/47, f], [2, 28/46, f]).
step_down([2, 28/48, f], [2, 28/47, f]).
step_down([2, 28/49, f], [2, 28/48, f]).
step_down([2, 28/50, f], [2, 28/49, f]).
step_down([2, 28/53, f], [2, 28/52, f]).
step_down([2, 28/54, f], [2, 28/53, f]).
step_down([2, 28/61, f], [2, 28/60, f]).
step_down([2, 28/62, f], [2, 28/61, f]).
step_down([2, 28/63, f], [2, 28/62, f]).
step_down([2, 28/64, f], [2, 28/63, f]).
step_down([2, 28/67, f], [2, 28/66, f]).
step_down([2, 28/68, f], [2, 28/67, f]).
step_down([2, 28/69, f], [2, 28/68, f]).
step_down([2, 28/70, f], [2, 28/69, f]).
step_down([2, 28/77, f], [2, 28/76, f]).
step_down([2, 28/78, f], [2, 28/77, f]).
step_down([2, 28/81, f], [2, 28/80, f]).
step_down([2, 28/82, f], [2, 28/81, f]).
step_down([2, 28/83, f], [2, 28/82, f]).
step_down([2, 28/84, f], [2, 28/83, f]).
step_down([2, 28/85, f], [2, 28/84, f]).
step_down([2, 28/86, f], [2, 28/85, f]).
step_down([2, 28/89, f], [2, 28/88, f]).
step_down([2, 28/90, f], [2, 28/89, f]).
step_down([2, 28/91, f], [2, 28/90, f]).
step_down([2, 28/92, f], [2, 28/91, f]).
step_down([2, 28/93, f], [2, 28/92, f]).
step_down([2, 28/94, f], [2, 28/93, f]).
step_down([2, 28/97, f], [2, 28/96, f]).
step_down([2, 28/98, f], [2, 28/97, f]).
step_down([2, 28/101, f], [2, 28/100, f]).
step_down([2, 28/102, f], [2, 28/101, f]).
step_down([2, 28/105, f], [2, 28/104, f]).
step_down([2, 28/106, f], [2, 28/105, f]).
step_down([2, 28/107, f], [2, 28/106, f]).
step_down([2, 28/108, f], [2, 28/107, f]).
step_down([2, 28/111, f], [2, 28/110, f]).
step_down([2, 28/112, f], [2, 28/111, f]).
step_down([2, 28/113, f], [2, 28/112, f]).
step_down([2, 28/114, f], [2, 28/113, f]).
step_down([2, 28/117, f], [2, 28/116, f]).
step_down([2, 28/118, f], [2, 28/117, f]).
step_down([2, 30/3, f], [2, 30/2, f]).
step_down([2, 30/4, f], [2, 30/3, f]).
step_down([2, 30/5, f], [2, 30/4, f]).
step_down([2, 30/6, f], [2, 30/5, f]).
step_down([2, 30/11, f], [2, 30/10, f]).
step_down([2, 30/12, f], [2, 30/11, f]).
step_down([2, 30/15, f], [2, 30/14, f]).
step_down([2, 30/16, f], [2, 30/15, f]).
step_down([2, 30/19, f], [2, 30/18, f]).
step_down([2, 30/20, f], [2, 30/19, f]).
step_down([2, 30/23, f], [2, 30/22, f]).
step_down([2, 30/24, f], [2, 30/23, f]).
step_down([2, 30/27, f], [2, 30/26, f]).
step_down([2, 30/28, f], [2, 30/27, f]).
step_down([2, 30/31, f], [2, 30/30, f]).
step_down([2, 30/32, f], [2, 30/31, f]).
step_down([2, 30/41, f], [2, 30/40, f]).
step_down([2, 30/42, f], [2, 30/41, f]).
step_down([2, 30/43, f], [2, 30/42, f]).
step_down([2, 30/44, f], [2, 30/43, f]).
step_down([2, 30/45, f], [2, 30/44, f]).
step_down([2, 30/46, f], [2, 30/45, f]).
step_down([2, 30/47, f], [2, 30/46, f]).
step_down([2, 30/48, f], [2, 30/47, f]).
step_down([2, 30/51, f], [2, 30/50, f]).
step_down([2, 30/52, f], [2, 30/51, f]).
step_down([2, 30/55, f], [2, 30/54, f]).
step_down([2, 30/56, f], [2, 30/55, f]).
step_down([2, 30/61, f], [2, 30/60, f]).
step_down([2, 30/62, f], [2, 30/61, f]).
step_down([2, 30/75, f], [2, 30/74, f]).
step_down([2, 30/76, f], [2, 30/75, f]).
step_down([2, 30/79, f], [2, 30/78, f]).
step_down([2, 30/80, f], [2, 30/79, f]).
step_down([2, 30/85, f], [2, 30/84, f]).
step_down([2, 30/86, f], [2, 30/85, f]).
step_down([2, 30/89, f], [2, 30/88, f]).
step_down([2, 30/90, f], [2, 30/89, f]).
step_down([2, 30/93, f], [2, 30/92, f]).
step_down([2, 30/94, f], [2, 30/93, f]).
step_down([2, 30/97, f], [2, 30/96, f]).
step_down([2, 30/98, f], [2, 30/97, f]).
step_down([2, 30/99, f], [2, 30/98, f]).
step_down([2, 30/100, f], [2, 30/99, f]).
step_down([2, 30/101, f], [2, 30/100, f]).
step_down([2, 30/102, f], [2, 30/101, f]).
step_down([2, 30/105, f], [2, 30/104, f]).
step_down([2, 30/106, f], [2, 30/105, f]).
step_down([2, 30/107, f], [2, 30/106, f]).
step_down([2, 30/108, f], [2, 30/107, f]).
step_down([2, 30/109, f], [2, 30/108, f]).
step_down([2, 30/110, f], [2, 30/109, f]).
step_down([2, 30/113, f], [2, 30/112, f]).
step_down([2, 30/114, f], [2, 30/113, f]).
step_down([2, 30/115, f], [2, 30/114, f]).
step_down([2, 30/116, f], [2, 30/115, f]).
step_down([2, 32/1, f], [2, 32/0, f]).
step_down([2, 32/2, f], [2, 32/1, f]).
step_down([2, 32/3, f], [2, 32/2, f]).
step_down([2, 32/4, f], [2, 32/3, f]).
step_down([2, 32/5, f], [2, 32/4, f]).
step_down([2, 32/6, f], [2, 32/5, f]).
step_down([2, 32/13, f], [2, 32/12, f]).
step_down([2, 32/14, f], [2, 32/13, f]).
step_down([2, 32/15, f], [2, 32/14, f]).
step_down([2, 32/16, f], [2, 32/15, f]).
step_down([2, 32/21, f], [2, 32/20, f]).
step_down([2, 32/22, f], [2, 32/21, f]).
step_down([2, 32/25, f], [2, 32/24, f]).
step_down([2, 32/26, f], [2, 32/25, f]).
step_down([2, 32/39, f], [2, 32/38, f]).
step_down([2, 32/40, f], [2, 32/39, f]).
step_down([2, 32/41, f], [2, 32/40, f]).
step_down([2, 32/42, f], [2, 32/41, f]).
step_down([2, 32/43, f], [2, 32/42, f]).
step_down([2, 32/44, f], [2, 32/43, f]).
step_down([2, 32/51, f], [2, 32/50, f]).
step_down([2, 32/52, f], [2, 32/51, f]).
step_down([2, 32/53, f], [2, 32/52, f]).
step_down([2, 32/54, f], [2, 32/53, f]).
step_down([2, 32/57, f], [2, 32/56, f]).
step_down([2, 32/58, f], [2, 32/57, f]).
step_down([2, 32/67, f], [2, 32/66, f]).
step_down([2, 32/68, f], [2, 32/67, f]).
step_down([2, 32/73, f], [2, 32/72, f]).
step_down([2, 32/74, f], [2, 32/73, f]).
step_down([2, 32/77, f], [2, 32/76, f]).
step_down([2, 32/78, f], [2, 32/77, f]).
step_down([2, 32/87, f], [2, 32/86, f]).
step_down([2, 32/88, f], [2, 32/87, f]).
step_down([2, 32/89, f], [2, 32/88, f]).
step_down([2, 32/90, f], [2, 32/89, f]).
step_down([2, 32/91, f], [2, 32/90, f]).
step_down([2, 32/92, f], [2, 32/91, f]).
step_down([2, 32/95, f], [2, 32/94, f]).
step_down([2, 32/96, f], [2, 32/95, f]).
step_down([2, 32/99, f], [2, 32/98, f]).
step_down([2, 32/100, f], [2, 32/99, f]).
step_down([2, 32/105, f], [2, 32/104, f]).
step_down([2, 32/106, f], [2, 32/105, f]).
step_down([2, 32/107, f], [2, 32/106, f]).
step_down([2, 32/108, f], [2, 32/107, f]).
step_down([2, 32/111, f], [2, 32/110, f]).
step_down([2, 32/112, f], [2, 32/111, f]).
step_down([2, 32/113, f], [2, 32/112, f]).
step_down([2, 32/114, f], [2, 32/113, f]).
step_down([2, 32/115, f], [2, 32/114, f]).
step_down([2, 32/116, f], [2, 32/115, f]).
step_down([2, 34/1, f], [2, 34/0, f]).
step_down([2, 34/2, f], [2, 34/1, f]).
step_down([2, 34/7, f], [2, 34/6, f]).
step_down([2, 34/8, f], [2, 34/7, f]).
step_down([2, 34/11, f], [2, 34/10, f]).
step_down([2, 34/12, f], [2, 34/11, f]).
step_down([2, 34/15, f], [2, 34/14, f]).
step_down([2, 34/16, f], [2, 34/15, f]).
step_down([2, 34/17, f], [2, 34/16, f]).
step_down([2, 34/18, f], [2, 34/17, f]).
step_down([2, 34/29, f], [2, 34/28, f]).
step_down([2, 34/30, f], [2, 34/29, f]).
step_down([2, 34/39, f], [2, 34/38, f]).
step_down([2, 34/40, f], [2, 34/39, f]).
step_down([2, 34/41, f], [2, 34/40, f]).
step_down([2, 34/42, f], [2, 34/41, f]).
step_down([2, 34/43, f], [2, 34/42, f]).
step_down([2, 34/44, f], [2, 34/43, f]).
step_down([2, 34/47, f], [2, 34/46, f]).
step_down([2, 34/48, f], [2, 34/47, f]).
step_down([2, 34/51, f], [2, 34/50, f]).
step_down([2, 34/52, f], [2, 34/51, f]).
step_down([2, 34/55, f], [2, 34/54, f]).
step_down([2, 34/56, f], [2, 34/55, f]).
step_down([2, 34/67, f], [2, 34/66, f]).
step_down([2, 34/68, f], [2, 34/67, f]).
step_down([2, 34/73, f], [2, 34/72, f]).
step_down([2, 34/74, f], [2, 34/73, f]).
step_down([2, 34/81, f], [2, 34/80, f]).
step_down([2, 34/82, f], [2, 34/81, f]).
step_down([2, 34/87, f], [2, 34/86, f]).
step_down([2, 34/88, f], [2, 34/87, f]).
step_down([2, 34/89, f], [2, 34/88, f]).
step_down([2, 34/90, f], [2, 34/89, f]).
step_down([2, 34/91, f], [2, 34/90, f]).
step_down([2, 34/92, f], [2, 34/91, f]).
step_down([2, 34/93, f], [2, 34/92, f]).
step_down([2, 34/94, f], [2, 34/93, f]).
step_down([2, 34/97, f], [2, 34/96, f]).
step_down([2, 34/98, f], [2, 34/97, f]).
step_down([2, 34/105, f], [2, 34/104, f]).
step_down([2, 34/106, f], [2, 34/105, f]).
step_down([2, 34/107, f], [2, 34/106, f]).
step_down([2, 34/108, f], [2, 34/107, f]).
step_down([2, 34/109, f], [2, 34/108, f]).
step_down([2, 34/110, f], [2, 34/109, f]).
step_down([2, 34/113, f], [2, 34/112, f]).
step_down([2, 34/114, f], [2, 34/113, f]).
step_down([2, 36/5, f], [2, 36/4, f]).
step_down([2, 36/6, f], [2, 36/5, f]).
step_down([2, 36/9, f], [2, 36/8, f]).
step_down([2, 36/10, f], [2, 36/9, f]).
step_down([2, 36/13, f], [2, 36/12, f]).
step_down([2, 36/14, f], [2, 36/13, f]).
step_down([2, 36/17, f], [2, 36/16, f]).
step_down([2, 36/18, f], [2, 36/17, f]).
step_down([2, 36/19, f], [2, 36/18, f]).
step_down([2, 36/20, f], [2, 36/19, f]).
step_down([2, 36/23, f], [2, 36/22, f]).
step_down([2, 36/24, f], [2, 36/23, f]).
step_down([2, 36/31, f], [2, 36/30, f]).
step_down([2, 36/32, f], [2, 36/31, f]).
step_down([2, 36/37, f], [2, 36/36, f]).
step_down([2, 36/38, f], [2, 36/37, f]).
step_down([2, 36/41, f], [2, 36/40, f]).
step_down([2, 36/42, f], [2, 36/41, f]).
step_down([2, 36/43, f], [2, 36/42, f]).
step_down([2, 36/44, f], [2, 36/43, f]).
step_down([2, 36/45, f], [2, 36/44, f]).
step_down([2, 36/46, f], [2, 36/45, f]).
step_down([2, 36/47, f], [2, 36/46, f]).
step_down([2, 36/48, f], [2, 36/47, f]).
step_down([2, 36/49, f], [2, 36/48, f]).
step_down([2, 36/50, f], [2, 36/49, f]).
step_down([2, 36/59, f], [2, 36/58, f]).
step_down([2, 36/60, f], [2, 36/59, f]).
step_down([2, 36/63, f], [2, 36/62, f]).
step_down([2, 36/64, f], [2, 36/63, f]).
step_down([2, 36/65, f], [2, 36/64, f]).
step_down([2, 36/66, f], [2, 36/65, f]).
step_down([2, 36/67, f], [2, 36/66, f]).
step_down([2, 36/68, f], [2, 36/67, f]).
step_down([2, 36/69, f], [2, 36/68, f]).
step_down([2, 36/70, f], [2, 36/69, f]).
step_down([2, 36/75, f], [2, 36/74, f]).
step_down([2, 36/76, f], [2, 36/75, f]).
step_down([2, 36/79, f], [2, 36/78, f]).
step_down([2, 36/80, f], [2, 36/79, f]).
step_down([2, 36/85, f], [2, 36/84, f]).
step_down([2, 36/86, f], [2, 36/85, f]).
step_down([2, 36/93, f], [2, 36/92, f]).
step_down([2, 36/94, f], [2, 36/93, f]).
step_down([2, 36/101, f], [2, 36/100, f]).
step_down([2, 36/102, f], [2, 36/101, f]).
step_down([2, 36/107, f], [2, 36/106, f]).
step_down([2, 36/108, f], [2, 36/107, f]).
step_down([2, 36/113, f], [2, 36/112, f]).
step_down([2, 36/114, f], [2, 36/113, f]).
step_down([2, 36/117, f], [2, 36/116, f]).
step_down([2, 36/118, f], [2, 36/117, f]).
step_down([2, 38/5, f], [2, 38/4, f]).
step_down([2, 38/6, f], [2, 38/5, f]).
step_down([2, 38/11, f], [2, 38/10, f]).
step_down([2, 38/12, f], [2, 38/11, f]).
step_down([2, 38/15, f], [2, 38/14, f]).
step_down([2, 38/16, f], [2, 38/15, f]).
step_down([2, 38/21, f], [2, 38/20, f]).
step_down([2, 38/22, f], [2, 38/21, f]).
step_down([2, 38/25, f], [2, 38/24, f]).
step_down([2, 38/26, f], [2, 38/25, f]).
step_down([2, 38/27, f], [2, 38/26, f]).
step_down([2, 38/28, f], [2, 38/27, f]).
step_down([2, 38/39, f], [2, 38/38, f]).
step_down([2, 38/40, f], [2, 38/39, f]).
step_down([2, 38/41, f], [2, 38/40, f]).
step_down([2, 38/42, f], [2, 38/41, f]).
step_down([2, 38/43, f], [2, 38/42, f]).
step_down([2, 38/44, f], [2, 38/43, f]).
step_down([2, 38/47, f], [2, 38/46, f]).
step_down([2, 38/48, f], [2, 38/47, f]).
step_down([2, 38/49, f], [2, 38/48, f]).
step_down([2, 38/50, f], [2, 38/49, f]).
step_down([2, 38/53, f], [2, 38/52, f]).
step_down([2, 38/54, f], [2, 38/53, f]).
step_down([2, 38/59, f], [2, 38/58, f]).
step_down([2, 38/60, f], [2, 38/59, f]).
step_down([2, 38/61, f], [2, 38/60, f]).
step_down([2, 38/62, f], [2, 38/61, f]).
step_down([2, 38/63, f], [2, 38/62, f]).
step_down([2, 38/64, f], [2, 38/63, f]).
step_down([2, 38/65, f], [2, 38/64, f]).
step_down([2, 38/66, f], [2, 38/65, f]).
step_down([2, 38/67, f], [2, 38/66, f]).
step_down([2, 38/68, f], [2, 38/67, f]).
step_down([2, 38/69, f], [2, 38/68, f]).
step_down([2, 38/70, f], [2, 38/69, f]).
step_down([2, 38/77, f], [2, 38/76, f]).
step_down([2, 38/78, f], [2, 38/77, f]).
step_down([2, 38/79, f], [2, 38/78, f]).
step_down([2, 38/80, f], [2, 38/79, f]).
step_down([2, 38/85, f], [2, 38/84, f]).
step_down([2, 38/86, f], [2, 38/85, f]).
step_down([2, 38/91, f], [2, 38/90, f]).
step_down([2, 38/92, f], [2, 38/91, f]).
step_down([2, 38/95, f], [2, 38/94, f]).
step_down([2, 38/96, f], [2, 38/95, f]).
step_down([2, 38/97, f], [2, 38/96, f]).
step_down([2, 38/98, f], [2, 38/97, f]).
step_down([2, 38/101, f], [2, 38/100, f]).
step_down([2, 38/102, f], [2, 38/101, f]).
step_down([2, 38/105, f], [2, 38/104, f]).
step_down([2, 38/106, f], [2, 38/105, f]).
step_down([2, 38/113, f], [2, 38/112, f]).
step_down([2, 38/114, f], [2, 38/113, f]).
step_down([2, 40/5, f], [2, 40/4, f]).
step_down([2, 40/6, f], [2, 40/5, f]).
step_down([2, 40/17, f], [2, 40/16, f]).
step_down([2, 40/18, f], [2, 40/17, f]).
step_down([2, 40/23, f], [2, 40/22, f]).
step_down([2, 40/24, f], [2, 40/23, f]).
step_down([2, 40/25, f], [2, 40/24, f]).
step_down([2, 40/26, f], [2, 40/25, f]).
step_down([2, 40/29, f], [2, 40/28, f]).
step_down([2, 40/30, f], [2, 40/29, f]).
step_down([2, 40/33, f], [2, 40/32, f]).
step_down([2, 40/34, f], [2, 40/33, f]).
step_down([2, 40/39, f], [2, 40/38, f]).
step_down([2, 40/40, f], [2, 40/39, f]).
step_down([2, 40/41, f], [2, 40/40, f]).
step_down([2, 40/42, f], [2, 40/41, f]).
step_down([2, 40/45, f], [2, 40/44, f]).
step_down([2, 40/46, f], [2, 40/45, f]).
step_down([2, 40/47, f], [2, 40/46, f]).
step_down([2, 40/48, f], [2, 40/47, f]).
step_down([2, 40/51, f], [2, 40/50, f]).
step_down([2, 40/52, f], [2, 40/51, f]).
step_down([2, 40/57, f], [2, 40/56, f]).
step_down([2, 40/58, f], [2, 40/57, f]).
step_down([2, 40/61, f], [2, 40/60, f]).
step_down([2, 40/62, f], [2, 40/61, f]).
step_down([2, 40/63, f], [2, 40/62, f]).
step_down([2, 40/64, f], [2, 40/63, f]).
step_down([2, 40/65, f], [2, 40/64, f]).
step_down([2, 40/66, f], [2, 40/65, f]).
step_down([2, 40/67, f], [2, 40/66, f]).
step_down([2, 40/68, f], [2, 40/67, f]).
step_down([2, 40/69, f], [2, 40/68, f]).
step_down([2, 40/70, f], [2, 40/69, f]).
step_down([2, 40/73, f], [2, 40/72, f]).
step_down([2, 40/74, f], [2, 40/73, f]).
step_down([2, 40/79, f], [2, 40/78, f]).
step_down([2, 40/80, f], [2, 40/79, f]).
step_down([2, 40/85, f], [2, 40/84, f]).
step_down([2, 40/86, f], [2, 40/85, f]).
step_down([2, 40/89, f], [2, 40/88, f]).
step_down([2, 40/90, f], [2, 40/89, f]).
step_down([2, 40/93, f], [2, 40/92, f]).
step_down([2, 40/94, f], [2, 40/93, f]).
step_down([2, 40/95, f], [2, 40/94, f]).
step_down([2, 40/96, f], [2, 40/95, f]).
step_down([2, 40/99, f], [2, 40/98, f]).
step_down([2, 40/100, f], [2, 40/99, f]).
step_down([2, 40/101, f], [2, 40/100, f]).
step_down([2, 40/102, f], [2, 40/101, f]).
step_down([2, 40/105, f], [2, 40/104, f]).
step_down([2, 40/106, f], [2, 40/105, f]).
step_down([2, 40/109, f], [2, 40/108, f]).
step_down([2, 40/110, f], [2, 40/109, f]).
step_down([2, 40/113, f], [2, 40/112, f]).
step_down([2, 40/114, f], [2, 40/113, f]).
step_down([2, 40/117, f], [2, 40/116, f]).
step_down([2, 40/118, f], [2, 40/117, f]).
step_down([2, 42/1, f], [2, 42/0, f]).
step_down([2, 42/2, f], [2, 42/1, f]).
step_down([2, 42/19, f], [2, 42/18, f]).
step_down([2, 42/20, f], [2, 42/19, f]).
step_down([2, 42/21, f], [2, 42/20, f]).
step_down([2, 42/22, f], [2, 42/21, f]).
step_down([2, 42/23, f], [2, 42/22, f]).
step_down([2, 42/24, f], [2, 42/23, f]).
step_down([2, 42/27, f], [2, 42/26, f]).
step_down([2, 42/28, f], [2, 42/27, f]).
step_down([2, 42/31, f], [2, 42/30, f]).
step_down([2, 42/32, f], [2, 42/31, f]).
step_down([2, 42/35, f], [2, 42/34, f]).
step_down([2, 42/36, f], [2, 42/35, f]).
step_down([2, 42/41, f], [2, 42/40, f]).
step_down([2, 42/42, f], [2, 42/41, f]).
step_down([2, 42/43, f], [2, 42/42, f]).
step_down([2, 42/44, f], [2, 42/43, f]).
step_down([2, 42/45, f], [2, 42/44, f]).
step_down([2, 42/46, f], [2, 42/45, f]).
step_down([2, 42/49, f], [2, 42/48, f]).
step_down([2, 42/50, f], [2, 42/49, f]).
step_down([2, 42/57, f], [2, 42/56, f]).
step_down([2, 42/58, f], [2, 42/57, f]).
step_down([2, 42/63, f], [2, 42/62, f]).
step_down([2, 42/64, f], [2, 42/63, f]).
step_down([2, 42/65, f], [2, 42/64, f]).
step_down([2, 42/66, f], [2, 42/65, f]).
step_down([2, 42/67, f], [2, 42/66, f]).
step_down([2, 42/68, f], [2, 42/67, f]).
step_down([2, 42/69, f], [2, 42/68, f]).
step_down([2, 42/70, f], [2, 42/69, f]).
step_down([2, 42/75, f], [2, 42/74, f]).
step_down([2, 42/76, f], [2, 42/75, f]).
step_down([2, 42/81, f], [2, 42/80, f]).
step_down([2, 42/82, f], [2, 42/81, f]).
step_down([2, 42/83, f], [2, 42/82, f]).
step_down([2, 42/84, f], [2, 42/83, f]).
step_down([2, 42/89, f], [2, 42/88, f]).
step_down([2, 42/90, f], [2, 42/89, f]).
step_down([2, 42/95, f], [2, 42/94, f]).
step_down([2, 42/96, f], [2, 42/95, f]).
step_down([2, 42/101, f], [2, 42/100, f]).
step_down([2, 42/102, f], [2, 42/101, f]).
step_down([2, 42/103, f], [2, 42/102, f]).
step_down([2, 42/104, f], [2, 42/103, f]).
step_down([2, 42/107, f], [2, 42/106, f]).
step_down([2, 42/108, f], [2, 42/107, f]).
step_down([2, 42/115, f], [2, 42/114, f]).
step_down([2, 42/116, f], [2, 42/115, f]).
step_down([2, 42/117, f], [2, 42/116, f]).
step_down([2, 42/118, f], [2, 42/117, f]).
step_down([2, 44/1, f], [2, 44/0, f]).
step_down([2, 44/2, f], [2, 44/1, f]).
step_down([2, 44/3, f], [2, 44/2, f]).
step_down([2, 44/4, f], [2, 44/3, f]).
step_down([2, 44/7, f], [2, 44/6, f]).
step_down([2, 44/8, f], [2, 44/7, f]).
step_down([2, 44/21, f], [2, 44/20, f]).
step_down([2, 44/22, f], [2, 44/21, f]).
step_down([2, 44/23, f], [2, 44/22, f]).
step_down([2, 44/24, f], [2, 44/23, f]).
step_down([2, 44/25, f], [2, 44/24, f]).
step_down([2, 44/26, f], [2, 44/25, f]).
step_down([2, 44/29, f], [2, 44/28, f]).
step_down([2, 44/30, f], [2, 44/29, f]).
step_down([2, 44/33, f], [2, 44/32, f]).
step_down([2, 44/34, f], [2, 44/33, f]).
step_down([2, 44/39, f], [2, 44/38, f]).
step_down([2, 44/40, f], [2, 44/39, f]).
step_down([2, 44/41, f], [2, 44/40, f]).
step_down([2, 44/42, f], [2, 44/41, f]).
step_down([2, 44/43, f], [2, 44/42, f]).
step_down([2, 44/44, f], [2, 44/43, f]).
step_down([2, 44/47, f], [2, 44/46, f]).
step_down([2, 44/48, f], [2, 44/47, f]).
step_down([2, 44/53, f], [2, 44/52, f]).
step_down([2, 44/54, f], [2, 44/53, f]).
step_down([2, 44/59, f], [2, 44/58, f]).
step_down([2, 44/60, f], [2, 44/59, f]).
step_down([2, 44/65, f], [2, 44/64, f]).
step_down([2, 44/66, f], [2, 44/65, f]).
step_down([2, 44/67, f], [2, 44/66, f]).
step_down([2, 44/68, f], [2, 44/67, f]).
step_down([2, 44/71, f], [2, 44/70, f]).
step_down([2, 44/72, f], [2, 44/71, f]).
step_down([2, 44/77, f], [2, 44/76, f]).
step_down([2, 44/78, f], [2, 44/77, f]).
step_down([2, 44/79, f], [2, 44/78, f]).
step_down([2, 44/80, f], [2, 44/79, f]).
step_down([2, 44/83, f], [2, 44/82, f]).
step_down([2, 44/84, f], [2, 44/83, f]).
step_down([2, 44/85, f], [2, 44/84, f]).
step_down([2, 44/86, f], [2, 44/85, f]).
step_down([2, 44/91, f], [2, 44/90, f]).
step_down([2, 44/92, f], [2, 44/91, f]).
step_down([2, 44/99, f], [2, 44/98, f]).
step_down([2, 44/100, f], [2, 44/99, f]).
step_down([2, 44/105, f], [2, 44/104, f]).
step_down([2, 44/106, f], [2, 44/105, f]).
step_down([2, 44/109, f], [2, 44/108, f]).
step_down([2, 44/110, f], [2, 44/109, f]).
step_down([2, 44/113, f], [2, 44/112, f]).
step_down([2, 44/114, f], [2, 44/113, f]).
step_down([2, 44/115, f], [2, 44/114, f]).
step_down([2, 44/116, f], [2, 44/115, f]).
step_down([2, 46/1, f], [2, 46/0, f]).
step_down([2, 46/2, f], [2, 46/1, f]).
step_down([2, 46/7, f], [2, 46/6, f]).
step_down([2, 46/8, f], [2, 46/7, f]).
step_down([2, 46/9, f], [2, 46/8, f]).
step_down([2, 46/10, f], [2, 46/9, f]).
step_down([2, 46/13, f], [2, 46/12, f]).
step_down([2, 46/14, f], [2, 46/13, f]).
step_down([2, 46/17, f], [2, 46/16, f]).
step_down([2, 46/18, f], [2, 46/17, f]).
step_down([2, 46/21, f], [2, 46/20, f]).
step_down([2, 46/22, f], [2, 46/21, f]).
step_down([2, 46/23, f], [2, 46/22, f]).
step_down([2, 46/24, f], [2, 46/23, f]).
step_down([2, 46/25, f], [2, 46/24, f]).
step_down([2, 46/26, f], [2, 46/25, f]).
step_down([2, 46/27, f], [2, 46/26, f]).
step_down([2, 46/28, f], [2, 46/27, f]).
step_down([2, 46/35, f], [2, 46/34, f]).
step_down([2, 46/36, f], [2, 46/35, f]).
step_down([2, 46/37, f], [2, 46/36, f]).
step_down([2, 46/38, f], [2, 46/37, f]).
step_down([2, 46/39, f], [2, 46/38, f]).
step_down([2, 46/40, f], [2, 46/39, f]).
step_down([2, 46/41, f], [2, 46/40, f]).
step_down([2, 46/42, f], [2, 46/41, f]).
step_down([2, 46/43, f], [2, 46/42, f]).
step_down([2, 46/44, f], [2, 46/43, f]).
step_down([2, 46/53, f], [2, 46/52, f]).
step_down([2, 46/54, f], [2, 46/53, f]).
step_down([2, 46/61, f], [2, 46/60, f]).
step_down([2, 46/62, f], [2, 46/61, f]).
step_down([2, 46/67, f], [2, 46/66, f]).
step_down([2, 46/68, f], [2, 46/67, f]).
step_down([2, 46/77, f], [2, 46/76, f]).
step_down([2, 46/78, f], [2, 46/77, f]).
step_down([2, 46/81, f], [2, 46/80, f]).
step_down([2, 46/82, f], [2, 46/81, f]).
step_down([2, 46/85, f], [2, 46/84, f]).
step_down([2, 46/86, f], [2, 46/85, f]).
step_down([2, 46/89, f], [2, 46/88, f]).
step_down([2, 46/90, f], [2, 46/89, f]).
step_down([2, 46/99, f], [2, 46/98, f]).
step_down([2, 46/100, f], [2, 46/99, f]).
step_down([2, 46/103, f], [2, 46/102, f]).
step_down([2, 46/104, f], [2, 46/103, f]).
step_down([2, 46/107, f], [2, 46/106, f]).
step_down([2, 46/108, f], [2, 46/107, f]).
step_down([2, 46/111, f], [2, 46/110, f]).
step_down([2, 46/112, f], [2, 46/111, f]).
step_down([2, 46/113, f], [2, 46/112, f]).
step_down([2, 46/114, f], [2, 46/113, f]).
step_down([2, 46/117, f], [2, 46/116, f]).
step_down([2, 46/118, f], [2, 46/117, f]).
step_down([2, 48/1, f], [2, 48/0, f]).
step_down([2, 48/2, f], [2, 48/1, f]).
step_down([2, 48/5, f], [2, 48/4, f]).
step_down([2, 48/6, f], [2, 48/5, f]).
step_down([2, 48/7, f], [2, 48/6, f]).
step_down([2, 48/8, f], [2, 48/7, f]).
step_down([2, 48/9, f], [2, 48/8, f]).
step_down([2, 48/10, f], [2, 48/9, f]).
step_down([2, 48/15, f], [2, 48/14, f]).
step_down([2, 48/16, f], [2, 48/15, f]).
step_down([2, 48/19, f], [2, 48/18, f]).
step_down([2, 48/20, f], [2, 48/19, f]).
step_down([2, 48/21, f], [2, 48/20, f]).
step_down([2, 48/22, f], [2, 48/21, f]).
step_down([2, 48/23, f], [2, 48/22, f]).
step_down([2, 48/24, f], [2, 48/23, f]).
step_down([2, 48/27, f], [2, 48/26, f]).
step_down([2, 48/28, f], [2, 48/27, f]).
step_down([2, 48/31, f], [2, 48/30, f]).
step_down([2, 48/32, f], [2, 48/31, f]).
step_down([2, 48/33, f], [2, 48/32, f]).
step_down([2, 48/34, f], [2, 48/33, f]).
step_down([2, 48/35, f], [2, 48/34, f]).
step_down([2, 48/36, f], [2, 48/35, f]).
step_down([2, 48/37, f], [2, 48/36, f]).
step_down([2, 48/38, f], [2, 48/37, f]).
step_down([2, 48/41, f], [2, 48/40, f]).
step_down([2, 48/42, f], [2, 48/41, f]).
step_down([2, 48/45, f], [2, 48/44, f]).
step_down([2, 48/46, f], [2, 48/45, f]).
step_down([2, 48/53, f], [2, 48/52, f]).
step_down([2, 48/54, f], [2, 48/53, f]).
step_down([2, 48/57, f], [2, 48/56, f]).
step_down([2, 48/58, f], [2, 48/57, f]).
step_down([2, 48/63, f], [2, 48/62, f]).
step_down([2, 48/64, f], [2, 48/63, f]).
step_down([2, 48/67, f], [2, 48/66, f]).
step_down([2, 48/68, f], [2, 48/67, f]).
step_down([2, 48/71, f], [2, 48/70, f]).
step_down([2, 48/72, f], [2, 48/71, f]).
step_down([2, 48/75, f], [2, 48/74, f]).
step_down([2, 48/76, f], [2, 48/75, f]).
step_down([2, 48/79, f], [2, 48/78, f]).
step_down([2, 48/80, f], [2, 48/79, f]).
step_down([2, 48/83, f], [2, 48/82, f]).
step_down([2, 48/84, f], [2, 48/83, f]).
step_down([2, 48/87, f], [2, 48/86, f]).
step_down([2, 48/88, f], [2, 48/87, f]).
step_down([2, 48/91, f], [2, 48/90, f]).
step_down([2, 48/92, f], [2, 48/91, f]).
step_down([2, 48/95, f], [2, 48/94, f]).
step_down([2, 48/96, f], [2, 48/95, f]).
step_down([2, 48/101, f], [2, 48/100, f]).
step_down([2, 48/102, f], [2, 48/101, f]).
step_down([2, 48/103, f], [2, 48/102, f]).
step_down([2, 48/104, f], [2, 48/103, f]).
step_down([2, 48/105, f], [2, 48/104, f]).
step_down([2, 48/106, f], [2, 48/105, f]).
step_down([2, 48/109, f], [2, 48/108, f]).
step_down([2, 48/110, f], [2, 48/109, f]).
step_down([2, 48/111, f], [2, 48/110, f]).
step_down([2, 48/112, f], [2, 48/111, f]).
step_down([2, 48/115, f], [2, 48/114, f]).
step_down([2, 48/116, f], [2, 48/115, f]).
step_down([2, 50/1, f], [2, 50/0, f]).
step_down([2, 50/2, f], [2, 50/1, f]).
step_down([2, 50/3, f], [2, 50/2, f]).
step_down([2, 50/4, f], [2, 50/3, f]).
step_down([2, 50/5, f], [2, 50/4, f]).
step_down([2, 50/6, f], [2, 50/5, f]).
step_down([2, 50/9, f], [2, 50/8, f]).
step_down([2, 50/10, f], [2, 50/9, f]).
step_down([2, 50/13, f], [2, 50/12, f]).
step_down([2, 50/14, f], [2, 50/13, f]).
step_down([2, 50/17, f], [2, 50/16, f]).
step_down([2, 50/18, f], [2, 50/17, f]).
step_down([2, 50/19, f], [2, 50/18, f]).
step_down([2, 50/20, f], [2, 50/19, f]).
step_down([2, 50/21, f], [2, 50/20, f]).
step_down([2, 50/22, f], [2, 50/21, f]).
step_down([2, 50/25, f], [2, 50/24, f]).
step_down([2, 50/26, f], [2, 50/25, f]).
step_down([2, 50/29, f], [2, 50/28, f]).
step_down([2, 50/30, f], [2, 50/29, f]).
step_down([2, 50/35, f], [2, 50/34, f]).
step_down([2, 50/36, f], [2, 50/35, f]).
step_down([2, 50/37, f], [2, 50/36, f]).
step_down([2, 50/38, f], [2, 50/37, f]).
step_down([2, 50/39, f], [2, 50/38, f]).
step_down([2, 50/40, f], [2, 50/39, f]).
step_down([2, 50/43, f], [2, 50/42, f]).
step_down([2, 50/44, f], [2, 50/43, f]).
step_down([2, 50/47, f], [2, 50/46, f]).
step_down([2, 50/48, f], [2, 50/47, f]).
step_down([2, 50/49, f], [2, 50/48, f]).
step_down([2, 50/50, f], [2, 50/49, f]).
step_down([2, 50/53, f], [2, 50/52, f]).
step_down([2, 50/54, f], [2, 50/53, f]).
step_down([2, 50/55, f], [2, 50/54, f]).
step_down([2, 50/56, f], [2, 50/55, f]).
step_down([2, 50/59, f], [2, 50/58, f]).
step_down([2, 50/60, f], [2, 50/59, f]).
step_down([2, 50/65, f], [2, 50/64, f]).
step_down([2, 50/66, f], [2, 50/65, f]).
step_down([2, 50/71, f], [2, 50/70, f]).
step_down([2, 50/72, f], [2, 50/71, f]).
step_down([2, 50/75, f], [2, 50/74, f]).
step_down([2, 50/76, f], [2, 50/75, f]).
step_down([2, 50/77, f], [2, 50/76, f]).
step_down([2, 50/78, f], [2, 50/77, f]).
step_down([2, 50/81, f], [2, 50/80, f]).
step_down([2, 50/82, f], [2, 50/81, f]).
step_down([2, 50/85, f], [2, 50/84, f]).
step_down([2, 50/86, f], [2, 50/85, f]).
step_down([2, 50/89, f], [2, 50/88, f]).
step_down([2, 50/90, f], [2, 50/89, f]).
step_down([2, 50/99, f], [2, 50/98, f]).
step_down([2, 50/100, f], [2, 50/99, f]).
step_down([2, 50/103, f], [2, 50/102, f]).
step_down([2, 50/104, f], [2, 50/103, f]).
step_down([2, 50/105, f], [2, 50/104, f]).
step_down([2, 50/106, f], [2, 50/105, f]).
step_down([2, 50/107, f], [2, 50/106, f]).
step_down([2, 50/108, f], [2, 50/107, f]).
step_down([2, 50/111, f], [2, 50/110, f]).
step_down([2, 50/112, f], [2, 50/111, f]).
step_down([2, 50/115, f], [2, 50/114, f]).
step_down([2, 50/116, f], [2, 50/115, f]).
step_down([2, 52/3, f], [2, 52/2, f]).
step_down([2, 52/4, f], [2, 52/3, f]).
step_down([2, 52/9, f], [2, 52/8, f]).
step_down([2, 52/10, f], [2, 52/9, f]).
step_down([2, 52/11, f], [2, 52/10, f]).
step_down([2, 52/12, f], [2, 52/11, f]).
step_down([2, 52/15, f], [2, 52/14, f]).
step_down([2, 52/16, f], [2, 52/15, f]).
step_down([2, 52/17, f], [2, 52/16, f]).
step_down([2, 52/18, f], [2, 52/17, f]).
step_down([2, 52/21, f], [2, 52/20, f]).
step_down([2, 52/22, f], [2, 52/21, f]).
step_down([2, 52/27, f], [2, 52/26, f]).
step_down([2, 52/28, f], [2, 52/27, f]).
step_down([2, 52/31, f], [2, 52/30, f]).
step_down([2, 52/32, f], [2, 52/31, f]).
step_down([2, 52/37, f], [2, 52/36, f]).
step_down([2, 52/38, f], [2, 52/37, f]).
step_down([2, 52/41, f], [2, 52/40, f]).
step_down([2, 52/42, f], [2, 52/41, f]).
step_down([2, 52/47, f], [2, 52/46, f]).
step_down([2, 52/48, f], [2, 52/47, f]).
step_down([2, 52/51, f], [2, 52/50, f]).
step_down([2, 52/52, f], [2, 52/51, f]).
step_down([2, 52/53, f], [2, 52/52, f]).
step_down([2, 52/54, f], [2, 52/53, f]).
step_down([2, 52/55, f], [2, 52/54, f]).
step_down([2, 52/56, f], [2, 52/55, f]).
step_down([2, 52/57, f], [2, 52/56, f]).
step_down([2, 52/58, f], [2, 52/57, f]).
step_down([2, 52/61, f], [2, 52/60, f]).
step_down([2, 52/62, f], [2, 52/61, f]).
step_down([2, 52/67, f], [2, 52/66, f]).
step_down([2, 52/68, f], [2, 52/67, f]).
step_down([2, 52/71, f], [2, 52/70, f]).
step_down([2, 52/72, f], [2, 52/71, f]).
step_down([2, 52/73, f], [2, 52/72, f]).
step_down([2, 52/74, f], [2, 52/73, f]).
step_down([2, 52/77, f], [2, 52/76, f]).
step_down([2, 52/78, f], [2, 52/77, f]).
step_down([2, 52/79, f], [2, 52/78, f]).
step_down([2, 52/80, f], [2, 52/79, f]).
step_down([2, 52/85, f], [2, 52/84, f]).
step_down([2, 52/86, f], [2, 52/85, f]).
step_down([2, 52/91, f], [2, 52/90, f]).
step_down([2, 52/92, f], [2, 52/91, f]).
step_down([2, 52/97, f], [2, 52/96, f]).
step_down([2, 52/98, f], [2, 52/97, f]).
step_down([2, 52/99, f], [2, 52/98, f]).
step_down([2, 52/100, f], [2, 52/99, f]).
step_down([2, 52/101, f], [2, 52/100, f]).
step_down([2, 52/102, f], [2, 52/101, f]).
step_down([2, 52/103, f], [2, 52/102, f]).
step_down([2, 52/104, f], [2, 52/103, f]).
step_down([2, 52/105, f], [2, 52/104, f]).
step_down([2, 52/106, f], [2, 52/105, f]).
step_down([2, 52/111, f], [2, 52/110, f]).
step_down([2, 52/112, f], [2, 52/111, f]).
step_down([2, 54/1, f], [2, 54/0, f]).
step_down([2, 54/2, f], [2, 54/1, f]).
step_down([2, 54/9, f], [2, 54/8, f]).
step_down([2, 54/10, f], [2, 54/9, f]).
step_down([2, 54/13, f], [2, 54/12, f]).
step_down([2, 54/14, f], [2, 54/13, f]).
step_down([2, 54/17, f], [2, 54/16, f]).
step_down([2, 54/18, f], [2, 54/17, f]).
step_down([2, 54/19, f], [2, 54/18, f]).
step_down([2, 54/20, f], [2, 54/19, f]).
step_down([2, 54/23, f], [2, 54/22, f]).
step_down([2, 54/24, f], [2, 54/23, f]).
step_down([2, 54/27, f], [2, 54/26, f]).
step_down([2, 54/28, f], [2, 54/27, f]).
step_down([2, 54/31, f], [2, 54/30, f]).
step_down([2, 54/32, f], [2, 54/31, f]).
step_down([2, 54/33, f], [2, 54/32, f]).
step_down([2, 54/34, f], [2, 54/33, f]).
step_down([2, 54/37, f], [2, 54/36, f]).
step_down([2, 54/38, f], [2, 54/37, f]).
step_down([2, 54/39, f], [2, 54/38, f]).
step_down([2, 54/40, f], [2, 54/39, f]).
step_down([2, 54/43, f], [2, 54/42, f]).
step_down([2, 54/44, f], [2, 54/43, f]).
step_down([2, 54/47, f], [2, 54/46, f]).
step_down([2, 54/48, f], [2, 54/47, f]).
step_down([2, 54/53, f], [2, 54/52, f]).
step_down([2, 54/54, f], [2, 54/53, f]).
step_down([2, 54/57, f], [2, 54/56, f]).
step_down([2, 54/58, f], [2, 54/57, f]).
step_down([2, 54/59, f], [2, 54/58, f]).
step_down([2, 54/60, f], [2, 54/59, f]).
step_down([2, 54/63, f], [2, 54/62, f]).
step_down([2, 54/64, f], [2, 54/63, f]).
step_down([2, 54/67, f], [2, 54/66, f]).
step_down([2, 54/68, f], [2, 54/67, f]).
step_down([2, 54/69, f], [2, 54/68, f]).
step_down([2, 54/70, f], [2, 54/69, f]).
step_down([2, 54/71, f], [2, 54/70, f]).
step_down([2, 54/72, f], [2, 54/71, f]).
step_down([2, 54/73, f], [2, 54/72, f]).
step_down([2, 54/74, f], [2, 54/73, f]).
step_down([2, 54/77, f], [2, 54/76, f]).
step_down([2, 54/78, f], [2, 54/77, f]).
step_down([2, 54/81, f], [2, 54/80, f]).
step_down([2, 54/82, f], [2, 54/81, f]).
step_down([2, 54/85, f], [2, 54/84, f]).
step_down([2, 54/86, f], [2, 54/85, f]).
step_down([2, 54/89, f], [2, 54/88, f]).
step_down([2, 54/90, f], [2, 54/89, f]).
step_down([2, 54/93, f], [2, 54/92, f]).
step_down([2, 54/94, f], [2, 54/93, f]).
step_down([2, 54/95, f], [2, 54/94, f]).
step_down([2, 54/96, f], [2, 54/95, f]).
step_down([2, 54/97, f], [2, 54/96, f]).
step_down([2, 54/98, f], [2, 54/97, f]).
step_down([2, 54/99, f], [2, 54/98, f]).
step_down([2, 54/100, f], [2, 54/99, f]).
step_down([2, 54/103, f], [2, 54/102, f]).
step_down([2, 54/104, f], [2, 54/103, f]).
step_down([2, 54/105, f], [2, 54/104, f]).
step_down([2, 54/106, f], [2, 54/105, f]).
step_down([2, 54/113, f], [2, 54/112, f]).
step_down([2, 54/114, f], [2, 54/113, f]).
step_down([2, 54/117, f], [2, 54/116, f]).
step_down([2, 54/118, f], [2, 54/117, f]).
step_down([2, 56/1, f], [2, 56/0, f]).
step_down([2, 56/2, f], [2, 56/1, f]).
step_down([2, 56/3, f], [2, 56/2, f]).
step_down([2, 56/4, f], [2, 56/3, f]).
step_down([2, 56/7, f], [2, 56/6, f]).
step_down([2, 56/8, f], [2, 56/7, f]).
step_down([2, 56/11, f], [2, 56/10, f]).
step_down([2, 56/12, f], [2, 56/11, f]).
step_down([2, 56/15, f], [2, 56/14, f]).
step_down([2, 56/16, f], [2, 56/15, f]).
step_down([2, 56/19, f], [2, 56/18, f]).
step_down([2, 56/20, f], [2, 56/19, f]).
step_down([2, 56/23, f], [2, 56/22, f]).
step_down([2, 56/24, f], [2, 56/23, f]).
step_down([2, 56/25, f], [2, 56/24, f]).
step_down([2, 56/26, f], [2, 56/25, f]).
step_down([2, 56/29, f], [2, 56/28, f]).
step_down([2, 56/30, f], [2, 56/29, f]).
step_down([2, 56/31, f], [2, 56/30, f]).
step_down([2, 56/32, f], [2, 56/31, f]).
step_down([2, 56/33, f], [2, 56/32, f]).
step_down([2, 56/34, f], [2, 56/33, f]).
step_down([2, 56/35, f], [2, 56/34, f]).
step_down([2, 56/36, f], [2, 56/35, f]).
step_down([2, 56/39, f], [2, 56/38, f]).
step_down([2, 56/40, f], [2, 56/39, f]).
step_down([2, 56/43, f], [2, 56/42, f]).
step_down([2, 56/44, f], [2, 56/43, f]).
step_down([2, 56/45, f], [2, 56/44, f]).
step_down([2, 56/46, f], [2, 56/45, f]).
step_down([2, 56/49, f], [2, 56/48, f]).
step_down([2, 56/50, f], [2, 56/49, f]).
step_down([2, 56/51, f], [2, 56/50, f]).
step_down([2, 56/52, f], [2, 56/51, f]).
step_down([2, 56/55, f], [2, 56/54, f]).
step_down([2, 56/56, f], [2, 56/55, f]).
step_down([2, 56/57, f], [2, 56/56, f]).
step_down([2, 56/58, f], [2, 56/57, f]).
step_down([2, 56/61, f], [2, 56/60, f]).
step_down([2, 56/62, f], [2, 56/61, f]).
step_down([2, 56/63, f], [2, 56/62, f]).
step_down([2, 56/64, f], [2, 56/63, f]).
step_down([2, 56/67, f], [2, 56/66, f]).
step_down([2, 56/68, f], [2, 56/67, f]).
step_down([2, 56/71, f], [2, 56/70, f]).
step_down([2, 56/72, f], [2, 56/71, f]).
step_down([2, 56/73, f], [2, 56/72, f]).
step_down([2, 56/74, f], [2, 56/73, f]).
step_down([2, 56/75, f], [2, 56/74, f]).
step_down([2, 56/76, f], [2, 56/75, f]).
step_down([2, 56/79, f], [2, 56/78, f]).
step_down([2, 56/80, f], [2, 56/79, f]).
step_down([2, 56/93, f], [2, 56/92, f]).
step_down([2, 56/94, f], [2, 56/93, f]).
step_down([2, 56/97, f], [2, 56/96, f]).
step_down([2, 56/98, f], [2, 56/97, f]).
step_down([2, 56/99, f], [2, 56/98, f]).
step_down([2, 56/100, f], [2, 56/99, f]).
step_down([2, 56/103, f], [2, 56/102, f]).
step_down([2, 56/104, f], [2, 56/103, f]).
step_down([2, 56/105, f], [2, 56/104, f]).
step_down([2, 56/106, f], [2, 56/105, f]).
step_down([2, 56/109, f], [2, 56/108, f]).
step_down([2, 56/110, f], [2, 56/109, f]).
step_down([2, 56/113, f], [2, 56/112, f]).
step_down([2, 56/114, f], [2, 56/113, f]).
step_down([2, 58/5, f], [2, 58/4, f]).
step_down([2, 58/6, f], [2, 58/5, f]).
step_down([2, 58/11, f], [2, 58/10, f]).
step_down([2, 58/12, f], [2, 58/11, f]).
step_down([2, 58/13, f], [2, 58/12, f]).
step_down([2, 58/14, f], [2, 58/13, f]).
step_down([2, 58/17, f], [2, 58/16, f]).
step_down([2, 58/18, f], [2, 58/17, f]).
step_down([2, 58/19, f], [2, 58/18, f]).
step_down([2, 58/20, f], [2, 58/19, f]).
step_down([2, 58/21, f], [2, 58/20, f]).
step_down([2, 58/22, f], [2, 58/21, f]).
step_down([2, 58/25, f], [2, 58/24, f]).
step_down([2, 58/26, f], [2, 58/25, f]).
step_down([2, 58/29, f], [2, 58/28, f]).
step_down([2, 58/30, f], [2, 58/29, f]).
step_down([2, 58/31, f], [2, 58/30, f]).
step_down([2, 58/32, f], [2, 58/31, f]).
step_down([2, 58/33, f], [2, 58/32, f]).
step_down([2, 58/34, f], [2, 58/33, f]).
step_down([2, 58/37, f], [2, 58/36, f]).
step_down([2, 58/38, f], [2, 58/37, f]).
step_down([2, 58/43, f], [2, 58/42, f]).
step_down([2, 58/44, f], [2, 58/43, f]).
step_down([2, 58/45, f], [2, 58/44, f]).
step_down([2, 58/46, f], [2, 58/45, f]).
step_down([2, 58/47, f], [2, 58/46, f]).
step_down([2, 58/48, f], [2, 58/47, f]).
step_down([2, 58/49, f], [2, 58/48, f]).
step_down([2, 58/50, f], [2, 58/49, f]).
step_down([2, 58/55, f], [2, 58/54, f]).
step_down([2, 58/56, f], [2, 58/55, f]).
step_down([2, 58/57, f], [2, 58/56, f]).
step_down([2, 58/58, f], [2, 58/57, f]).
step_down([2, 58/59, f], [2, 58/58, f]).
step_down([2, 58/60, f], [2, 58/59, f]).
step_down([2, 58/63, f], [2, 58/62, f]).
step_down([2, 58/64, f], [2, 58/63, f]).
step_down([2, 58/69, f], [2, 58/68, f]).
step_down([2, 58/70, f], [2, 58/69, f]).
step_down([2, 58/73, f], [2, 58/72, f]).
step_down([2, 58/74, f], [2, 58/73, f]).
step_down([2, 58/75, f], [2, 58/74, f]).
step_down([2, 58/76, f], [2, 58/75, f]).
step_down([2, 58/77, f], [2, 58/76, f]).
step_down([2, 58/78, f], [2, 58/77, f]).
step_down([2, 58/79, f], [2, 58/78, f]).
step_down([2, 58/80, f], [2, 58/79, f]).
step_down([2, 58/83, f], [2, 58/82, f]).
step_down([2, 58/84, f], [2, 58/83, f]).
step_down([2, 58/91, f], [2, 58/90, f]).
step_down([2, 58/92, f], [2, 58/91, f]).
step_down([2, 58/95, f], [2, 58/94, f]).
step_down([2, 58/96, f], [2, 58/95, f]).
step_down([2, 58/99, f], [2, 58/98, f]).
step_down([2, 58/100, f], [2, 58/99, f]).
step_down([2, 58/103, f], [2, 58/102, f]).
step_down([2, 58/104, f], [2, 58/103, f]).
step_down([2, 58/107, f], [2, 58/106, f]).
step_down([2, 58/108, f], [2, 58/107, f]).
step_down([2, 58/113, f], [2, 58/112, f]).
step_down([2, 58/114, f], [2, 58/113, f]).
step_down([2, 58/117, f], [2, 58/116, f]).
step_down([2, 58/118, f], [2, 58/117, f]).
step_down([2, 60/1, f], [2, 60/0, f]).
step_down([2, 60/2, f], [2, 60/1, f]).
step_down([2, 60/7, f], [2, 60/6, f]).
step_down([2, 60/8, f], [2, 60/7, f]).
step_down([2, 60/13, f], [2, 60/12, f]).
step_down([2, 60/14, f], [2, 60/13, f]).
step_down([2, 60/15, f], [2, 60/14, f]).
step_down([2, 60/16, f], [2, 60/15, f]).
step_down([2, 60/17, f], [2, 60/16, f]).
step_down([2, 60/18, f], [2, 60/17, f]).
step_down([2, 60/19, f], [2, 60/18, f]).
step_down([2, 60/20, f], [2, 60/19, f]).
step_down([2, 60/21, f], [2, 60/20, f]).
step_down([2, 60/22, f], [2, 60/21, f]).
step_down([2, 60/25, f], [2, 60/24, f]).
step_down([2, 60/26, f], [2, 60/25, f]).
step_down([2, 60/27, f], [2, 60/26, f]).
step_down([2, 60/28, f], [2, 60/27, f]).
step_down([2, 60/31, f], [2, 60/30, f]).
step_down([2, 60/32, f], [2, 60/31, f]).
step_down([2, 60/35, f], [2, 60/34, f]).
step_down([2, 60/36, f], [2, 60/35, f]).
step_down([2, 60/37, f], [2, 60/36, f]).
step_down([2, 60/38, f], [2, 60/37, f]).
step_down([2, 60/41, f], [2, 60/40, f]).
step_down([2, 60/42, f], [2, 60/41, f]).
step_down([2, 60/43, f], [2, 60/42, f]).
step_down([2, 60/44, f], [2, 60/43, f]).
step_down([2, 60/47, f], [2, 60/46, f]).
step_down([2, 60/48, f], [2, 60/47, f]).
step_down([2, 60/49, f], [2, 60/48, f]).
step_down([2, 60/50, f], [2, 60/49, f]).
step_down([2, 60/57, f], [2, 60/56, f]).
step_down([2, 60/58, f], [2, 60/57, f]).
step_down([2, 60/63, f], [2, 60/62, f]).
step_down([2, 60/64, f], [2, 60/63, f]).
step_down([2, 60/67, f], [2, 60/66, f]).
step_down([2, 60/68, f], [2, 60/67, f]).
step_down([2, 60/71, f], [2, 60/70, f]).
step_down([2, 60/72, f], [2, 60/71, f]).
step_down([2, 60/75, f], [2, 60/74, f]).
step_down([2, 60/76, f], [2, 60/75, f]).
step_down([2, 60/77, f], [2, 60/76, f]).
step_down([2, 60/78, f], [2, 60/77, f]).
step_down([2, 60/83, f], [2, 60/82, f]).
step_down([2, 60/84, f], [2, 60/83, f]).
step_down([2, 60/85, f], [2, 60/84, f]).
step_down([2, 60/86, f], [2, 60/85, f]).
step_down([2, 60/89, f], [2, 60/88, f]).
step_down([2, 60/90, f], [2, 60/89, f]).
step_down([2, 60/93, f], [2, 60/92, f]).
step_down([2, 60/94, f], [2, 60/93, f]).
step_down([2, 60/97, f], [2, 60/96, f]).
step_down([2, 60/98, f], [2, 60/97, f]).
step_down([2, 60/101, f], [2, 60/100, f]).
step_down([2, 60/102, f], [2, 60/101, f]).
step_down([2, 60/105, f], [2, 60/104, f]).
step_down([2, 60/106, f], [2, 60/105, f]).
step_down([2, 60/109, f], [2, 60/108, f]).
step_down([2, 60/110, f], [2, 60/109, f]).
step_down([2, 60/111, f], [2, 60/110, f]).
step_down([2, 60/112, f], [2, 60/111, f]).
step_down([2, 60/115, f], [2, 60/114, f]).
step_down([2, 60/116, f], [2, 60/115, f]).
step_down([2, 62/3, f], [2, 62/2, f]).
step_down([2, 62/4, f], [2, 62/3, f]).
step_down([2, 62/17, f], [2, 62/16, f]).
step_down([2, 62/18, f], [2, 62/17, f]).
step_down([2, 62/21, f], [2, 62/20, f]).
step_down([2, 62/22, f], [2, 62/21, f]).
step_down([2, 62/25, f], [2, 62/24, f]).
step_down([2, 62/26, f], [2, 62/25, f]).
step_down([2, 62/29, f], [2, 62/28, f]).
step_down([2, 62/30, f], [2, 62/29, f]).
step_down([2, 62/33, f], [2, 62/32, f]).
step_down([2, 62/34, f], [2, 62/33, f]).
step_down([2, 62/39, f], [2, 62/38, f]).
step_down([2, 62/40, f], [2, 62/39, f]).
step_down([2, 62/43, f], [2, 62/42, f]).
step_down([2, 62/44, f], [2, 62/43, f]).
step_down([2, 62/49, f], [2, 62/48, f]).
step_down([2, 62/50, f], [2, 62/49, f]).
step_down([2, 62/51, f], [2, 62/50, f]).
step_down([2, 62/52, f], [2, 62/51, f]).
step_down([2, 62/53, f], [2, 62/52, f]).
step_down([2, 62/54, f], [2, 62/53, f]).
step_down([2, 62/57, f], [2, 62/56, f]).
step_down([2, 62/58, f], [2, 62/57, f]).
step_down([2, 62/63, f], [2, 62/62, f]).
step_down([2, 62/64, f], [2, 62/63, f]).
step_down([2, 62/65, f], [2, 62/64, f]).
step_down([2, 62/66, f], [2, 62/65, f]).
step_down([2, 62/69, f], [2, 62/68, f]).
step_down([2, 62/70, f], [2, 62/69, f]).
step_down([2, 62/71, f], [2, 62/70, f]).
step_down([2, 62/72, f], [2, 62/71, f]).
step_down([2, 62/73, f], [2, 62/72, f]).
step_down([2, 62/74, f], [2, 62/73, f]).
step_down([2, 62/77, f], [2, 62/76, f]).
step_down([2, 62/78, f], [2, 62/77, f]).
step_down([2, 62/81, f], [2, 62/80, f]).
step_down([2, 62/82, f], [2, 62/81, f]).
step_down([2, 62/85, f], [2, 62/84, f]).
step_down([2, 62/86, f], [2, 62/85, f]).
step_down([2, 62/87, f], [2, 62/86, f]).
step_down([2, 62/88, f], [2, 62/87, f]).
step_down([2, 62/89, f], [2, 62/88, f]).
step_down([2, 62/90, f], [2, 62/89, f]).
step_down([2, 62/93, f], [2, 62/92, f]).
step_down([2, 62/94, f], [2, 62/93, f]).
step_down([2, 62/95, f], [2, 62/94, f]).
step_down([2, 62/96, f], [2, 62/95, f]).
step_down([2, 62/97, f], [2, 62/96, f]).
step_down([2, 62/98, f], [2, 62/97, f]).
step_down([2, 62/103, f], [2, 62/102, f]).
step_down([2, 62/104, f], [2, 62/103, f]).
step_down([2, 62/107, f], [2, 62/106, f]).
step_down([2, 62/108, f], [2, 62/107, f]).
step_down([2, 62/113, f], [2, 62/112, f]).
step_down([2, 62/114, f], [2, 62/113, f]).
step_down([2, 62/115, f], [2, 62/114, f]).
step_down([2, 62/116, f], [2, 62/115, f]).
step_down([2, 64/1, f], [2, 64/0, f]).
step_down([2, 64/2, f], [2, 64/1, f]).
step_down([2, 64/3, f], [2, 64/2, f]).
step_down([2, 64/4, f], [2, 64/3, f]).
step_down([2, 64/5, f], [2, 64/4, f]).
step_down([2, 64/6, f], [2, 64/5, f]).
step_down([2, 64/9, f], [2, 64/8, f]).
step_down([2, 64/10, f], [2, 64/9, f]).
step_down([2, 64/15, f], [2, 64/14, f]).
step_down([2, 64/16, f], [2, 64/15, f]).
step_down([2, 64/19, f], [2, 64/18, f]).
step_down([2, 64/20, f], [2, 64/19, f]).
step_down([2, 64/21, f], [2, 64/20, f]).
step_down([2, 64/22, f], [2, 64/21, f]).
step_down([2, 64/23, f], [2, 64/22, f]).
step_down([2, 64/24, f], [2, 64/23, f]).
step_down([2, 64/27, f], [2, 64/26, f]).
step_down([2, 64/28, f], [2, 64/27, f]).
step_down([2, 64/35, f], [2, 64/34, f]).
step_down([2, 64/36, f], [2, 64/35, f]).
step_down([2, 64/37, f], [2, 64/36, f]).
step_down([2, 64/38, f], [2, 64/37, f]).
step_down([2, 64/41, f], [2, 64/40, f]).
step_down([2, 64/42, f], [2, 64/41, f]).
step_down([2, 64/43, f], [2, 64/42, f]).
step_down([2, 64/44, f], [2, 64/43, f]).
step_down([2, 64/51, f], [2, 64/50, f]).
step_down([2, 64/52, f], [2, 64/51, f]).
step_down([2, 64/55, f], [2, 64/54, f]).
step_down([2, 64/56, f], [2, 64/55, f]).
step_down([2, 64/61, f], [2, 64/60, f]).
step_down([2, 64/62, f], [2, 64/61, f]).
step_down([2, 64/63, f], [2, 64/62, f]).
step_down([2, 64/64, f], [2, 64/63, f]).
step_down([2, 64/65, f], [2, 64/64, f]).
step_down([2, 64/66, f], [2, 64/65, f]).
step_down([2, 64/67, f], [2, 64/66, f]).
step_down([2, 64/68, f], [2, 64/67, f]).
step_down([2, 64/73, f], [2, 64/72, f]).
step_down([2, 64/74, f], [2, 64/73, f]).
step_down([2, 64/79, f], [2, 64/78, f]).
step_down([2, 64/80, f], [2, 64/79, f]).
step_down([2, 64/87, f], [2, 64/86, f]).
step_down([2, 64/88, f], [2, 64/87, f]).
step_down([2, 64/89, f], [2, 64/88, f]).
step_down([2, 64/90, f], [2, 64/89, f]).
step_down([2, 64/91, f], [2, 64/90, f]).
step_down([2, 64/92, f], [2, 64/91, f]).
step_down([2, 64/95, f], [2, 64/94, f]).
step_down([2, 64/96, f], [2, 64/95, f]).
step_down([2, 64/101, f], [2, 64/100, f]).
step_down([2, 64/102, f], [2, 64/101, f]).
step_down([2, 64/105, f], [2, 64/104, f]).
step_down([2, 64/106, f], [2, 64/105, f]).
step_down([2, 64/111, f], [2, 64/110, f]).
step_down([2, 64/112, f], [2, 64/111, f]).
step_down([2, 66/1, f], [2, 66/0, f]).
step_down([2, 66/2, f], [2, 66/1, f]).
step_down([2, 66/3, f], [2, 66/2, f]).
step_down([2, 66/4, f], [2, 66/3, f]).
step_down([2, 66/9, f], [2, 66/8, f]).
step_down([2, 66/10, f], [2, 66/9, f]).
step_down([2, 66/11, f], [2, 66/10, f]).
step_down([2, 66/12, f], [2, 66/11, f]).
step_down([2, 66/15, f], [2, 66/14, f]).
step_down([2, 66/16, f], [2, 66/15, f]).
step_down([2, 66/17, f], [2, 66/16, f]).
step_down([2, 66/18, f], [2, 66/17, f]).
step_down([2, 66/19, f], [2, 66/18, f]).
step_down([2, 66/20, f], [2, 66/19, f]).
step_down([2, 66/23, f], [2, 66/22, f]).
step_down([2, 66/24, f], [2, 66/23, f]).
step_down([2, 66/27, f], [2, 66/26, f]).
step_down([2, 66/28, f], [2, 66/27, f]).
step_down([2, 66/33, f], [2, 66/32, f]).
step_down([2, 66/34, f], [2, 66/33, f]).
step_down([2, 66/41, f], [2, 66/40, f]).
step_down([2, 66/42, f], [2, 66/41, f]).
step_down([2, 66/47, f], [2, 66/46, f]).
step_down([2, 66/48, f], [2, 66/47, f]).
step_down([2, 66/53, f], [2, 66/52, f]).
step_down([2, 66/54, f], [2, 66/53, f]).
step_down([2, 66/57, f], [2, 66/56, f]).
step_down([2, 66/58, f], [2, 66/57, f]).
step_down([2, 66/61, f], [2, 66/60, f]).
step_down([2, 66/62, f], [2, 66/61, f]).
step_down([2, 66/63, f], [2, 66/62, f]).
step_down([2, 66/64, f], [2, 66/63, f]).
step_down([2, 66/65, f], [2, 66/64, f]).
step_down([2, 66/66, f], [2, 66/65, f]).
step_down([2, 66/67, f], [2, 66/66, f]).
step_down([2, 66/68, f], [2, 66/67, f]).
step_down([2, 66/77, f], [2, 66/76, f]).
step_down([2, 66/78, f], [2, 66/77, f]).
step_down([2, 66/81, f], [2, 66/80, f]).
step_down([2, 66/82, f], [2, 66/81, f]).
step_down([2, 66/85, f], [2, 66/84, f]).
step_down([2, 66/86, f], [2, 66/85, f]).
step_down([2, 66/89, f], [2, 66/88, f]).
step_down([2, 66/90, f], [2, 66/89, f]).
step_down([2, 66/91, f], [2, 66/90, f]).
step_down([2, 66/92, f], [2, 66/91, f]).
step_down([2, 66/97, f], [2, 66/96, f]).
step_down([2, 66/98, f], [2, 66/97, f]).
step_down([2, 66/101, f], [2, 66/100, f]).
step_down([2, 66/102, f], [2, 66/101, f]).
step_down([2, 66/105, f], [2, 66/104, f]).
step_down([2, 66/106, f], [2, 66/105, f]).
step_down([2, 66/111, f], [2, 66/110, f]).
step_down([2, 66/112, f], [2, 66/111, f]).
step_down([2, 68/3, f], [2, 68/2, f]).
step_down([2, 68/4, f], [2, 68/3, f]).
step_down([2, 68/11, f], [2, 68/10, f]).
step_down([2, 68/12, f], [2, 68/11, f]).
step_down([2, 68/15, f], [2, 68/14, f]).
step_down([2, 68/16, f], [2, 68/15, f]).
step_down([2, 68/17, f], [2, 68/16, f]).
step_down([2, 68/18, f], [2, 68/17, f]).
step_down([2, 68/23, f], [2, 68/22, f]).
step_down([2, 68/24, f], [2, 68/23, f]).
step_down([2, 68/25, f], [2, 68/24, f]).
step_down([2, 68/26, f], [2, 68/25, f]).
step_down([2, 68/29, f], [2, 68/28, f]).
step_down([2, 68/30, f], [2, 68/29, f]).
step_down([2, 68/39, f], [2, 68/38, f]).
step_down([2, 68/40, f], [2, 68/39, f]).
step_down([2, 68/43, f], [2, 68/42, f]).
step_down([2, 68/44, f], [2, 68/43, f]).
step_down([2, 68/47, f], [2, 68/46, f]).
step_down([2, 68/48, f], [2, 68/47, f]).
step_down([2, 68/49, f], [2, 68/48, f]).
step_down([2, 68/50, f], [2, 68/49, f]).
step_down([2, 68/53, f], [2, 68/52, f]).
step_down([2, 68/54, f], [2, 68/53, f]).
step_down([2, 68/55, f], [2, 68/54, f]).
step_down([2, 68/56, f], [2, 68/55, f]).
step_down([2, 68/57, f], [2, 68/56, f]).
step_down([2, 68/58, f], [2, 68/57, f]).
step_down([2, 68/59, f], [2, 68/58, f]).
step_down([2, 68/60, f], [2, 68/59, f]).
step_down([2, 68/63, f], [2, 68/62, f]).
step_down([2, 68/64, f], [2, 68/63, f]).
step_down([2, 68/65, f], [2, 68/64, f]).
step_down([2, 68/66, f], [2, 68/65, f]).
step_down([2, 68/69, f], [2, 68/68, f]).
step_down([2, 68/70, f], [2, 68/69, f]).
step_down([2, 68/71, f], [2, 68/70, f]).
step_down([2, 68/72, f], [2, 68/71, f]).
step_down([2, 68/75, f], [2, 68/74, f]).
step_down([2, 68/76, f], [2, 68/75, f]).
step_down([2, 68/77, f], [2, 68/76, f]).
step_down([2, 68/78, f], [2, 68/77, f]).
step_down([2, 68/79, f], [2, 68/78, f]).
step_down([2, 68/80, f], [2, 68/79, f]).
step_down([2, 68/81, f], [2, 68/80, f]).
step_down([2, 68/82, f], [2, 68/81, f]).
step_down([2, 68/83, f], [2, 68/82, f]).
step_down([2, 68/84, f], [2, 68/83, f]).
step_down([2, 68/97, f], [2, 68/96, f]).
step_down([2, 68/98, f], [2, 68/97, f]).
step_down([2, 68/99, f], [2, 68/98, f]).
step_down([2, 68/100, f], [2, 68/99, f]).
step_down([2, 68/103, f], [2, 68/102, f]).
step_down([2, 68/104, f], [2, 68/103, f]).
step_down([2, 68/109, f], [2, 68/108, f]).
step_down([2, 68/110, f], [2, 68/109, f]).
step_down([2, 68/113, f], [2, 68/112, f]).
step_down([2, 68/114, f], [2, 68/113, f]).
step_down([2, 70/1, f], [2, 70/0, f]).
step_down([2, 70/2, f], [2, 70/1, f]).
step_down([2, 70/5, f], [2, 70/4, f]).
step_down([2, 70/6, f], [2, 70/5, f]).
step_down([2, 70/9, f], [2, 70/8, f]).
step_down([2, 70/10, f], [2, 70/9, f]).
step_down([2, 70/13, f], [2, 70/12, f]).
step_down([2, 70/14, f], [2, 70/13, f]).
step_down([2, 70/15, f], [2, 70/14, f]).
step_down([2, 70/16, f], [2, 70/15, f]).
step_down([2, 70/25, f], [2, 70/24, f]).
step_down([2, 70/26, f], [2, 70/25, f]).
step_down([2, 70/27, f], [2, 70/26, f]).
step_down([2, 70/28, f], [2, 70/27, f]).
step_down([2, 70/31, f], [2, 70/30, f]).
step_down([2, 70/32, f], [2, 70/31, f]).
step_down([2, 70/33, f], [2, 70/32, f]).
step_down([2, 70/34, f], [2, 70/33, f]).
step_down([2, 70/37, f], [2, 70/36, f]).
step_down([2, 70/38, f], [2, 70/37, f]).
step_down([2, 70/39, f], [2, 70/38, f]).
step_down([2, 70/40, f], [2, 70/39, f]).
step_down([2, 70/43, f], [2, 70/42, f]).
step_down([2, 70/44, f], [2, 70/43, f]).
step_down([2, 70/45, f], [2, 70/44, f]).
step_down([2, 70/46, f], [2, 70/45, f]).
step_down([2, 70/47, f], [2, 70/46, f]).
step_down([2, 70/48, f], [2, 70/47, f]).
step_down([2, 70/49, f], [2, 70/48, f]).
step_down([2, 70/50, f], [2, 70/49, f]).
step_down([2, 70/53, f], [2, 70/52, f]).
step_down([2, 70/54, f], [2, 70/53, f]).
step_down([2, 70/55, f], [2, 70/54, f]).
step_down([2, 70/56, f], [2, 70/55, f]).
step_down([2, 70/59, f], [2, 70/58, f]).
step_down([2, 70/60, f], [2, 70/59, f]).
step_down([2, 70/63, f], [2, 70/62, f]).
step_down([2, 70/64, f], [2, 70/63, f]).
step_down([2, 70/71, f], [2, 70/70, f]).
step_down([2, 70/72, f], [2, 70/71, f]).
step_down([2, 70/73, f], [2, 70/72, f]).
step_down([2, 70/74, f], [2, 70/73, f]).
step_down([2, 70/77, f], [2, 70/76, f]).
step_down([2, 70/78, f], [2, 70/77, f]).
step_down([2, 70/79, f], [2, 70/78, f]).
step_down([2, 70/80, f], [2, 70/79, f]).
step_down([2, 70/81, f], [2, 70/80, f]).
step_down([2, 70/82, f], [2, 70/81, f]).
step_down([2, 70/85, f], [2, 70/84, f]).
step_down([2, 70/86, f], [2, 70/85, f]).
step_down([2, 70/89, f], [2, 70/88, f]).
step_down([2, 70/90, f], [2, 70/89, f]).
step_down([2, 70/95, f], [2, 70/94, f]).
step_down([2, 70/96, f], [2, 70/95, f]).
step_down([2, 70/97, f], [2, 70/96, f]).
step_down([2, 70/98, f], [2, 70/97, f]).
step_down([2, 70/103, f], [2, 70/102, f]).
step_down([2, 70/104, f], [2, 70/103, f]).
step_down([2, 70/111, f], [2, 70/110, f]).
step_down([2, 70/112, f], [2, 70/111, f]).
step_down([2, 70/115, f], [2, 70/114, f]).
step_down([2, 70/116, f], [2, 70/115, f]).
step_down([2, 70/117, f], [2, 70/116, f]).
step_down([2, 70/118, f], [2, 70/117, f]).
step_down([2, 72/1, f], [2, 72/0, f]).
step_down([2, 72/2, f], [2, 72/1, f]).
step_down([2, 72/5, f], [2, 72/4, f]).
step_down([2, 72/6, f], [2, 72/5, f]).
step_down([2, 72/7, f], [2, 72/6, f]).
step_down([2, 72/8, f], [2, 72/7, f]).
step_down([2, 72/11, f], [2, 72/10, f]).
step_down([2, 72/12, f], [2, 72/11, f]).
step_down([2, 72/13, f], [2, 72/12, f]).
step_down([2, 72/14, f], [2, 72/13, f]).
step_down([2, 72/21, f], [2, 72/20, f]).
step_down([2, 72/22, f], [2, 72/21, f]).
step_down([2, 72/27, f], [2, 72/26, f]).
step_down([2, 72/28, f], [2, 72/27, f]).
step_down([2, 72/29, f], [2, 72/28, f]).
step_down([2, 72/30, f], [2, 72/29, f]).
step_down([2, 72/31, f], [2, 72/30, f]).
step_down([2, 72/32, f], [2, 72/31, f]).
step_down([2, 72/35, f], [2, 72/34, f]).
step_down([2, 72/36, f], [2, 72/35, f]).
step_down([2, 72/37, f], [2, 72/36, f]).
step_down([2, 72/38, f], [2, 72/37, f]).
step_down([2, 72/41, f], [2, 72/40, f]).
step_down([2, 72/42, f], [2, 72/41, f]).
step_down([2, 72/43, f], [2, 72/42, f]).
step_down([2, 72/44, f], [2, 72/43, f]).
step_down([2, 72/45, f], [2, 72/44, f]).
step_down([2, 72/46, f], [2, 72/45, f]).
step_down([2, 72/47, f], [2, 72/46, f]).
step_down([2, 72/48, f], [2, 72/47, f]).
step_down([2, 72/51, f], [2, 72/50, f]).
step_down([2, 72/52, f], [2, 72/51, f]).
step_down([2, 72/55, f], [2, 72/54, f]).
step_down([2, 72/56, f], [2, 72/55, f]).
step_down([2, 72/61, f], [2, 72/60, f]).
step_down([2, 72/62, f], [2, 72/61, f]).
step_down([2, 72/63, f], [2, 72/62, f]).
step_down([2, 72/64, f], [2, 72/63, f]).
step_down([2, 72/69, f], [2, 72/68, f]).
step_down([2, 72/70, f], [2, 72/69, f]).
step_down([2, 72/77, f], [2, 72/76, f]).
step_down([2, 72/78, f], [2, 72/77, f]).
step_down([2, 72/83, f], [2, 72/82, f]).
step_down([2, 72/84, f], [2, 72/83, f]).
step_down([2, 72/85, f], [2, 72/84, f]).
step_down([2, 72/86, f], [2, 72/85, f]).
step_down([2, 72/87, f], [2, 72/86, f]).
step_down([2, 72/88, f], [2, 72/87, f]).
step_down([2, 72/95, f], [2, 72/94, f]).
step_down([2, 72/96, f], [2, 72/95, f]).
step_down([2, 72/99, f], [2, 72/98, f]).
step_down([2, 72/100, f], [2, 72/99, f]).
step_down([2, 72/105, f], [2, 72/104, f]).
step_down([2, 72/106, f], [2, 72/105, f]).
step_down([2, 72/115, f], [2, 72/114, f]).
step_down([2, 72/116, f], [2, 72/115, f]).
step_down([2, 72/117, f], [2, 72/116, f]).
step_down([2, 72/118, f], [2, 72/117, f]).
step_down([2, 74/3, f], [2, 74/2, f]).
step_down([2, 74/4, f], [2, 74/3, f]).
step_down([2, 74/5, f], [2, 74/4, f]).
step_down([2, 74/6, f], [2, 74/5, f]).
step_down([2, 74/7, f], [2, 74/6, f]).
step_down([2, 74/8, f], [2, 74/7, f]).
step_down([2, 74/9, f], [2, 74/8, f]).
step_down([2, 74/10, f], [2, 74/9, f]).
step_down([2, 74/13, f], [2, 74/12, f]).
step_down([2, 74/14, f], [2, 74/13, f]).
step_down([2, 74/25, f], [2, 74/24, f]).
step_down([2, 74/26, f], [2, 74/25, f]).
step_down([2, 74/27, f], [2, 74/26, f]).
step_down([2, 74/28, f], [2, 74/27, f]).
step_down([2, 74/31, f], [2, 74/30, f]).
step_down([2, 74/32, f], [2, 74/31, f]).
step_down([2, 74/33, f], [2, 74/32, f]).
step_down([2, 74/34, f], [2, 74/33, f]).
step_down([2, 74/35, f], [2, 74/34, f]).
step_down([2, 74/36, f], [2, 74/35, f]).
step_down([2, 74/41, f], [2, 74/40, f]).
step_down([2, 74/42, f], [2, 74/41, f]).
step_down([2, 74/45, f], [2, 74/44, f]).
step_down([2, 74/46, f], [2, 74/45, f]).
step_down([2, 74/49, f], [2, 74/48, f]).
step_down([2, 74/50, f], [2, 74/49, f]).
step_down([2, 74/59, f], [2, 74/58, f]).
step_down([2, 74/60, f], [2, 74/59, f]).
step_down([2, 74/63, f], [2, 74/62, f]).
step_down([2, 74/64, f], [2, 74/63, f]).
step_down([2, 74/69, f], [2, 74/68, f]).
step_down([2, 74/70, f], [2, 74/69, f]).
step_down([2, 74/71, f], [2, 74/70, f]).
step_down([2, 74/72, f], [2, 74/71, f]).
step_down([2, 74/73, f], [2, 74/72, f]).
step_down([2, 74/74, f], [2, 74/73, f]).
step_down([2, 74/77, f], [2, 74/76, f]).
step_down([2, 74/78, f], [2, 74/77, f]).
step_down([2, 74/81, f], [2, 74/80, f]).
step_down([2, 74/82, f], [2, 74/81, f]).
step_down([2, 74/83, f], [2, 74/82, f]).
step_down([2, 74/84, f], [2, 74/83, f]).
step_down([2, 74/85, f], [2, 74/84, f]).
step_down([2, 74/86, f], [2, 74/85, f]).
step_down([2, 74/89, f], [2, 74/88, f]).
step_down([2, 74/90, f], [2, 74/89, f]).
step_down([2, 74/93, f], [2, 74/92, f]).
step_down([2, 74/94, f], [2, 74/93, f]).
step_down([2, 74/97, f], [2, 74/96, f]).
step_down([2, 74/98, f], [2, 74/97, f]).
step_down([2, 74/99, f], [2, 74/98, f]).
step_down([2, 74/100, f], [2, 74/99, f]).
step_down([2, 74/101, f], [2, 74/100, f]).
step_down([2, 74/102, f], [2, 74/101, f]).
step_down([2, 74/107, f], [2, 74/106, f]).
step_down([2, 74/108, f], [2, 74/107, f]).
step_down([2, 74/111, f], [2, 74/110, f]).
step_down([2, 74/112, f], [2, 74/111, f]).
step_down([2, 74/113, f], [2, 74/112, f]).
step_down([2, 74/114, f], [2, 74/113, f]).
step_down([2, 74/115, f], [2, 74/114, f]).
step_down([2, 74/116, f], [2, 74/115, f]).
step_down([2, 76/1, f], [2, 76/0, f]).
step_down([2, 76/2, f], [2, 76/1, f]).
step_down([2, 76/3, f], [2, 76/2, f]).
step_down([2, 76/4, f], [2, 76/3, f]).
step_down([2, 76/11, f], [2, 76/10, f]).
step_down([2, 76/12, f], [2, 76/11, f]).
step_down([2, 76/21, f], [2, 76/20, f]).
step_down([2, 76/22, f], [2, 76/21, f]).
step_down([2, 76/23, f], [2, 76/22, f]).
step_down([2, 76/24, f], [2, 76/23, f]).
step_down([2, 76/27, f], [2, 76/26, f]).
step_down([2, 76/28, f], [2, 76/27, f]).
step_down([2, 76/31, f], [2, 76/30, f]).
step_down([2, 76/32, f], [2, 76/31, f]).
step_down([2, 76/39, f], [2, 76/38, f]).
step_down([2, 76/40, f], [2, 76/39, f]).
step_down([2, 76/47, f], [2, 76/46, f]).
step_down([2, 76/48, f], [2, 76/47, f]).
step_down([2, 76/49, f], [2, 76/48, f]).
step_down([2, 76/50, f], [2, 76/49, f]).
step_down([2, 76/59, f], [2, 76/58, f]).
step_down([2, 76/60, f], [2, 76/59, f]).
step_down([2, 76/65, f], [2, 76/64, f]).
step_down([2, 76/66, f], [2, 76/65, f]).
step_down([2, 76/71, f], [2, 76/70, f]).
step_down([2, 76/72, f], [2, 76/71, f]).
step_down([2, 76/73, f], [2, 76/72, f]).
step_down([2, 76/74, f], [2, 76/73, f]).
step_down([2, 76/79, f], [2, 76/78, f]).
step_down([2, 76/80, f], [2, 76/79, f]).
step_down([2, 76/81, f], [2, 76/80, f]).
step_down([2, 76/82, f], [2, 76/81, f]).
step_down([2, 76/89, f], [2, 76/88, f]).
step_down([2, 76/90, f], [2, 76/89, f]).
step_down([2, 76/97, f], [2, 76/96, f]).
step_down([2, 76/98, f], [2, 76/97, f]).
step_down([2, 76/99, f], [2, 76/98, f]).
step_down([2, 76/100, f], [2, 76/99, f]).
step_down([2, 76/103, f], [2, 76/102, f]).
step_down([2, 76/104, f], [2, 76/103, f]).
step_down([2, 76/105, f], [2, 76/104, f]).
step_down([2, 76/106, f], [2, 76/105, f]).
step_down([2, 76/107, f], [2, 76/106, f]).
step_down([2, 76/108, f], [2, 76/107, f]).
step_down([2, 76/109, f], [2, 76/108, f]).
step_down([2, 76/110, f], [2, 76/109, f]).
step_down([2, 76/113, f], [2, 76/112, f]).
step_down([2, 76/114, f], [2, 76/113, f]).
step_down([2, 76/117, f], [2, 76/116, f]).
step_down([2, 76/118, f], [2, 76/117, f]).
step_down([2, 78/1, f], [2, 78/0, f]).
step_down([2, 78/2, f], [2, 78/1, f]).
step_down([2, 78/3, f], [2, 78/2, f]).
step_down([2, 78/4, f], [2, 78/3, f]).
step_down([2, 78/5, f], [2, 78/4, f]).
step_down([2, 78/6, f], [2, 78/5, f]).
step_down([2, 78/9, f], [2, 78/8, f]).
step_down([2, 78/10, f], [2, 78/9, f]).
step_down([2, 78/11, f], [2, 78/10, f]).
step_down([2, 78/12, f], [2, 78/11, f]).
step_down([2, 78/15, f], [2, 78/14, f]).
step_down([2, 78/16, f], [2, 78/15, f]).
step_down([2, 78/19, f], [2, 78/18, f]).
step_down([2, 78/20, f], [2, 78/19, f]).
step_down([2, 78/21, f], [2, 78/20, f]).
step_down([2, 78/22, f], [2, 78/21, f]).
step_down([2, 78/25, f], [2, 78/24, f]).
step_down([2, 78/26, f], [2, 78/25, f]).
step_down([2, 78/29, f], [2, 78/28, f]).
step_down([2, 78/30, f], [2, 78/29, f]).
step_down([2, 78/33, f], [2, 78/32, f]).
step_down([2, 78/34, f], [2, 78/33, f]).
step_down([2, 78/41, f], [2, 78/40, f]).
step_down([2, 78/42, f], [2, 78/41, f]).
step_down([2, 78/43, f], [2, 78/42, f]).
step_down([2, 78/44, f], [2, 78/43, f]).
step_down([2, 78/47, f], [2, 78/46, f]).
step_down([2, 78/48, f], [2, 78/47, f]).
step_down([2, 78/49, f], [2, 78/48, f]).
step_down([2, 78/50, f], [2, 78/49, f]).
step_down([2, 78/51, f], [2, 78/50, f]).
step_down([2, 78/52, f], [2, 78/51, f]).
step_down([2, 78/57, f], [2, 78/56, f]).
step_down([2, 78/58, f], [2, 78/57, f]).
step_down([2, 78/59, f], [2, 78/58, f]).
step_down([2, 78/60, f], [2, 78/59, f]).
step_down([2, 78/63, f], [2, 78/62, f]).
step_down([2, 78/64, f], [2, 78/63, f]).
step_down([2, 78/65, f], [2, 78/64, f]).
step_down([2, 78/66, f], [2, 78/65, f]).
step_down([2, 78/69, f], [2, 78/68, f]).
step_down([2, 78/70, f], [2, 78/69, f]).
step_down([2, 78/77, f], [2, 78/76, f]).
step_down([2, 78/78, f], [2, 78/77, f]).
step_down([2, 78/83, f], [2, 78/82, f]).
step_down([2, 78/84, f], [2, 78/83, f]).
step_down([2, 78/89, f], [2, 78/88, f]).
step_down([2, 78/90, f], [2, 78/89, f]).
step_down([2, 78/93, f], [2, 78/92, f]).
step_down([2, 78/94, f], [2, 78/93, f]).
step_down([2, 78/97, f], [2, 78/96, f]).
step_down([2, 78/98, f], [2, 78/97, f]).
step_down([2, 78/101, f], [2, 78/100, f]).
step_down([2, 78/102, f], [2, 78/101, f]).
step_down([2, 78/107, f], [2, 78/106, f]).
step_down([2, 78/108, f], [2, 78/107, f]).
step_down([2, 78/111, f], [2, 78/110, f]).
step_down([2, 78/112, f], [2, 78/111, f]).
step_down([2, 78/115, f], [2, 78/114, f]).
step_down([2, 78/116, f], [2, 78/115, f]).
step_down([2, 78/117, f], [2, 78/116, f]).
step_down([2, 78/118, f], [2, 78/117, f]).
step_down([2, 80/3, f], [2, 80/2, f]).
step_down([2, 80/4, f], [2, 80/3, f]).
step_down([2, 80/5, f], [2, 80/4, f]).
step_down([2, 80/6, f], [2, 80/5, f]).
step_down([2, 80/9, f], [2, 80/8, f]).
step_down([2, 80/10, f], [2, 80/9, f]).
step_down([2, 80/11, f], [2, 80/10, f]).
step_down([2, 80/12, f], [2, 80/11, f]).
step_down([2, 80/15, f], [2, 80/14, f]).
step_down([2, 80/16, f], [2, 80/15, f]).
step_down([2, 80/19, f], [2, 80/18, f]).
step_down([2, 80/20, f], [2, 80/19, f]).
step_down([2, 80/23, f], [2, 80/22, f]).
step_down([2, 80/24, f], [2, 80/23, f]).
step_down([2, 80/25, f], [2, 80/24, f]).
step_down([2, 80/26, f], [2, 80/25, f]).
step_down([2, 80/31, f], [2, 80/30, f]).
step_down([2, 80/32, f], [2, 80/31, f]).
step_down([2, 80/35, f], [2, 80/34, f]).
step_down([2, 80/36, f], [2, 80/35, f]).
step_down([2, 80/37, f], [2, 80/36, f]).
step_down([2, 80/38, f], [2, 80/37, f]).
step_down([2, 80/41, f], [2, 80/40, f]).
step_down([2, 80/42, f], [2, 80/41, f]).
step_down([2, 80/43, f], [2, 80/42, f]).
step_down([2, 80/44, f], [2, 80/43, f]).
step_down([2, 80/45, f], [2, 80/44, f]).
step_down([2, 80/46, f], [2, 80/45, f]).
step_down([2, 80/47, f], [2, 80/46, f]).
step_down([2, 80/48, f], [2, 80/47, f]).
step_down([2, 80/51, f], [2, 80/50, f]).
step_down([2, 80/52, f], [2, 80/51, f]).
step_down([2, 80/55, f], [2, 80/54, f]).
step_down([2, 80/56, f], [2, 80/55, f]).
step_down([2, 80/57, f], [2, 80/56, f]).
step_down([2, 80/58, f], [2, 80/57, f]).
step_down([2, 80/61, f], [2, 80/60, f]).
step_down([2, 80/62, f], [2, 80/61, f]).
step_down([2, 80/63, f], [2, 80/62, f]).
step_down([2, 80/64, f], [2, 80/63, f]).
step_down([2, 80/67, f], [2, 80/66, f]).
step_down([2, 80/68, f], [2, 80/67, f]).
step_down([2, 80/69, f], [2, 80/68, f]).
step_down([2, 80/70, f], [2, 80/69, f]).
step_down([2, 80/71, f], [2, 80/70, f]).
step_down([2, 80/72, f], [2, 80/71, f]).
step_down([2, 80/79, f], [2, 80/78, f]).
step_down([2, 80/80, f], [2, 80/79, f]).
step_down([2, 80/83, f], [2, 80/82, f]).
step_down([2, 80/84, f], [2, 80/83, f]).
step_down([2, 80/85, f], [2, 80/84, f]).
step_down([2, 80/86, f], [2, 80/85, f]).
step_down([2, 80/87, f], [2, 80/86, f]).
step_down([2, 80/88, f], [2, 80/87, f]).
step_down([2, 80/89, f], [2, 80/88, f]).
step_down([2, 80/90, f], [2, 80/89, f]).
step_down([2, 80/95, f], [2, 80/94, f]).
step_down([2, 80/96, f], [2, 80/95, f]).
step_down([2, 80/97, f], [2, 80/96, f]).
step_down([2, 80/98, f], [2, 80/97, f]).
step_down([2, 80/101, f], [2, 80/100, f]).
step_down([2, 80/102, f], [2, 80/101, f]).
step_down([2, 80/111, f], [2, 80/110, f]).
step_down([2, 80/112, f], [2, 80/111, f]).
step_down([2, 80/115, f], [2, 80/114, f]).
step_down([2, 80/116, f], [2, 80/115, f]).
step_down([2, 82/1, f], [2, 82/0, f]).
step_down([2, 82/2, f], [2, 82/1, f]).
step_down([2, 82/3, f], [2, 82/2, f]).
step_down([2, 82/4, f], [2, 82/3, f]).
step_down([2, 82/5, f], [2, 82/4, f]).
step_down([2, 82/6, f], [2, 82/5, f]).
step_down([2, 82/7, f], [2, 82/6, f]).
step_down([2, 82/8, f], [2, 82/7, f]).
step_down([2, 82/11, f], [2, 82/10, f]).
step_down([2, 82/12, f], [2, 82/11, f]).
step_down([2, 82/13, f], [2, 82/12, f]).
step_down([2, 82/14, f], [2, 82/13, f]).
step_down([2, 82/19, f], [2, 82/18, f]).
step_down([2, 82/20, f], [2, 82/19, f]).
step_down([2, 82/23, f], [2, 82/22, f]).
step_down([2, 82/24, f], [2, 82/23, f]).
step_down([2, 82/27, f], [2, 82/26, f]).
step_down([2, 82/28, f], [2, 82/27, f]).
step_down([2, 82/33, f], [2, 82/32, f]).
step_down([2, 82/34, f], [2, 82/33, f]).
step_down([2, 82/37, f], [2, 82/36, f]).
step_down([2, 82/38, f], [2, 82/37, f]).
step_down([2, 82/39, f], [2, 82/38, f]).
step_down([2, 82/40, f], [2, 82/39, f]).
step_down([2, 82/43, f], [2, 82/42, f]).
step_down([2, 82/44, f], [2, 82/43, f]).
step_down([2, 82/45, f], [2, 82/44, f]).
step_down([2, 82/46, f], [2, 82/45, f]).
step_down([2, 82/47, f], [2, 82/46, f]).
step_down([2, 82/48, f], [2, 82/47, f]).
step_down([2, 82/51, f], [2, 82/50, f]).
step_down([2, 82/52, f], [2, 82/51, f]).
step_down([2, 82/53, f], [2, 82/52, f]).
step_down([2, 82/54, f], [2, 82/53, f]).
step_down([2, 82/55, f], [2, 82/54, f]).
step_down([2, 82/56, f], [2, 82/55, f]).
step_down([2, 82/59, f], [2, 82/58, f]).
step_down([2, 82/60, f], [2, 82/59, f]).
step_down([2, 82/63, f], [2, 82/62, f]).
step_down([2, 82/64, f], [2, 82/63, f]).
step_down([2, 82/65, f], [2, 82/64, f]).
step_down([2, 82/66, f], [2, 82/65, f]).
step_down([2, 82/69, f], [2, 82/68, f]).
step_down([2, 82/70, f], [2, 82/69, f]).
step_down([2, 82/75, f], [2, 82/74, f]).
step_down([2, 82/76, f], [2, 82/75, f]).
step_down([2, 82/85, f], [2, 82/84, f]).
step_down([2, 82/86, f], [2, 82/85, f]).
step_down([2, 82/87, f], [2, 82/86, f]).
step_down([2, 82/88, f], [2, 82/87, f]).
step_down([2, 82/91, f], [2, 82/90, f]).
step_down([2, 82/92, f], [2, 82/91, f]).
step_down([2, 82/93, f], [2, 82/92, f]).
step_down([2, 82/94, f], [2, 82/93, f]).
step_down([2, 82/103, f], [2, 82/102, f]).
step_down([2, 82/104, f], [2, 82/103, f]).
step_down([2, 82/107, f], [2, 82/106, f]).
step_down([2, 82/108, f], [2, 82/107, f]).
step_down([2, 82/113, f], [2, 82/112, f]).
step_down([2, 82/114, f], [2, 82/113, f]).
step_down([2, 82/115, f], [2, 82/114, f]).
step_down([2, 82/116, f], [2, 82/115, f]).
step_down([2, 82/117, f], [2, 82/116, f]).
step_down([2, 82/118, f], [2, 82/117, f]).
step_down([2, 84/3, f], [2, 84/2, f]).
step_down([2, 84/4, f], [2, 84/3, f]).
step_down([2, 84/5, f], [2, 84/4, f]).
step_down([2, 84/6, f], [2, 84/5, f]).
step_down([2, 84/7, f], [2, 84/6, f]).
step_down([2, 84/8, f], [2, 84/7, f]).
step_down([2, 84/11, f], [2, 84/10, f]).
step_down([2, 84/12, f], [2, 84/11, f]).
step_down([2, 84/13, f], [2, 84/12, f]).
step_down([2, 84/14, f], [2, 84/13, f]).
step_down([2, 84/17, f], [2, 84/16, f]).
step_down([2, 84/18, f], [2, 84/17, f]).
step_down([2, 84/27, f], [2, 84/26, f]).
step_down([2, 84/28, f], [2, 84/27, f]).
step_down([2, 84/31, f], [2, 84/30, f]).
step_down([2, 84/32, f], [2, 84/31, f]).
step_down([2, 84/39, f], [2, 84/38, f]).
step_down([2, 84/40, f], [2, 84/39, f]).
step_down([2, 84/43, f], [2, 84/42, f]).
step_down([2, 84/44, f], [2, 84/43, f]).
step_down([2, 84/45, f], [2, 84/44, f]).
step_down([2, 84/46, f], [2, 84/45, f]).
step_down([2, 84/49, f], [2, 84/48, f]).
step_down([2, 84/50, f], [2, 84/49, f]).
step_down([2, 84/51, f], [2, 84/50, f]).
step_down([2, 84/52, f], [2, 84/51, f]).
step_down([2, 84/55, f], [2, 84/54, f]).
step_down([2, 84/56, f], [2, 84/55, f]).
step_down([2, 84/57, f], [2, 84/56, f]).
step_down([2, 84/58, f], [2, 84/57, f]).
step_down([2, 84/61, f], [2, 84/60, f]).
step_down([2, 84/62, f], [2, 84/61, f]).
step_down([2, 84/63, f], [2, 84/62, f]).
step_down([2, 84/64, f], [2, 84/63, f]).
step_down([2, 84/65, f], [2, 84/64, f]).
step_down([2, 84/66, f], [2, 84/65, f]).
step_down([2, 84/67, f], [2, 84/66, f]).
step_down([2, 84/68, f], [2, 84/67, f]).
step_down([2, 84/71, f], [2, 84/70, f]).
step_down([2, 84/72, f], [2, 84/71, f]).
step_down([2, 84/73, f], [2, 84/72, f]).
step_down([2, 84/74, f], [2, 84/73, f]).
step_down([2, 84/83, f], [2, 84/82, f]).
step_down([2, 84/84, f], [2, 84/83, f]).
step_down([2, 84/87, f], [2, 84/86, f]).
step_down([2, 84/88, f], [2, 84/87, f]).
step_down([2, 84/89, f], [2, 84/88, f]).
step_down([2, 84/90, f], [2, 84/89, f]).
step_down([2, 84/91, f], [2, 84/90, f]).
step_down([2, 84/92, f], [2, 84/91, f]).
step_down([2, 84/101, f], [2, 84/100, f]).
step_down([2, 84/102, f], [2, 84/101, f]).
step_down([2, 84/105, f], [2, 84/104, f]).
step_down([2, 84/106, f], [2, 84/105, f]).
step_down([2, 84/107, f], [2, 84/106, f]).
step_down([2, 84/108, f], [2, 84/107, f]).
step_down([2, 84/109, f], [2, 84/108, f]).
step_down([2, 84/110, f], [2, 84/109, f]).
step_down([2, 84/113, f], [2, 84/112, f]).
step_down([2, 84/114, f], [2, 84/113, f]).
step_down([2, 84/115, f], [2, 84/114, f]).
step_down([2, 84/116, f], [2, 84/115, f]).
step_down([2, 86/1, f], [2, 86/0, f]).
step_down([2, 86/2, f], [2, 86/1, f]).
step_down([2, 86/3, f], [2, 86/2, f]).
step_down([2, 86/4, f], [2, 86/3, f]).
step_down([2, 86/7, f], [2, 86/6, f]).
step_down([2, 86/8, f], [2, 86/7, f]).
step_down([2, 86/9, f], [2, 86/8, f]).
step_down([2, 86/10, f], [2, 86/9, f]).
step_down([2, 86/11, f], [2, 86/10, f]).
step_down([2, 86/12, f], [2, 86/11, f]).
step_down([2, 86/15, f], [2, 86/14, f]).
step_down([2, 86/16, f], [2, 86/15, f]).
step_down([2, 86/19, f], [2, 86/18, f]).
step_down([2, 86/20, f], [2, 86/19, f]).
step_down([2, 86/25, f], [2, 86/24, f]).
step_down([2, 86/26, f], [2, 86/25, f]).
step_down([2, 86/29, f], [2, 86/28, f]).
step_down([2, 86/30, f], [2, 86/29, f]).
step_down([2, 86/31, f], [2, 86/30, f]).
step_down([2, 86/32, f], [2, 86/31, f]).
step_down([2, 86/35, f], [2, 86/34, f]).
step_down([2, 86/36, f], [2, 86/35, f]).
step_down([2, 86/41, f], [2, 86/40, f]).
step_down([2, 86/42, f], [2, 86/41, f]).
step_down([2, 86/45, f], [2, 86/44, f]).
step_down([2, 86/46, f], [2, 86/45, f]).
step_down([2, 86/57, f], [2, 86/56, f]).
step_down([2, 86/58, f], [2, 86/57, f]).
step_down([2, 86/63, f], [2, 86/62, f]).
step_down([2, 86/64, f], [2, 86/63, f]).
step_down([2, 86/65, f], [2, 86/64, f]).
step_down([2, 86/66, f], [2, 86/65, f]).
step_down([2, 86/67, f], [2, 86/66, f]).
step_down([2, 86/68, f], [2, 86/67, f]).
step_down([2, 86/69, f], [2, 86/68, f]).
step_down([2, 86/70, f], [2, 86/69, f]).
step_down([2, 86/73, f], [2, 86/72, f]).
step_down([2, 86/74, f], [2, 86/73, f]).
step_down([2, 86/85, f], [2, 86/84, f]).
step_down([2, 86/86, f], [2, 86/85, f]).
step_down([2, 86/87, f], [2, 86/86, f]).
step_down([2, 86/88, f], [2, 86/87, f]).
step_down([2, 86/91, f], [2, 86/90, f]).
step_down([2, 86/92, f], [2, 86/91, f]).
step_down([2, 86/93, f], [2, 86/92, f]).
step_down([2, 86/94, f], [2, 86/93, f]).
step_down([2, 86/97, f], [2, 86/96, f]).
step_down([2, 86/98, f], [2, 86/97, f]).
step_down([2, 86/99, f], [2, 86/98, f]).
step_down([2, 86/100, f], [2, 86/99, f]).
step_down([2, 86/101, f], [2, 86/100, f]).
step_down([2, 86/102, f], [2, 86/101, f]).
step_down([2, 86/103, f], [2, 86/102, f]).
step_down([2, 86/104, f], [2, 86/103, f]).
step_down([2, 86/107, f], [2, 86/106, f]).
step_down([2, 86/108, f], [2, 86/107, f]).
step_down([2, 86/109, f], [2, 86/108, f]).
step_down([2, 86/110, f], [2, 86/109, f]).
step_down([2, 86/111, f], [2, 86/110, f]).
step_down([2, 86/112, f], [2, 86/111, f]).
step_down([2, 86/113, f], [2, 86/112, f]).
step_down([2, 86/114, f], [2, 86/113, f]).
step_down([2, 86/117, f], [2, 86/116, f]).
step_down([2, 86/118, f], [2, 86/117, f]).
step_down([2, 88/1, f], [2, 88/0, f]).
step_down([2, 88/2, f], [2, 88/1, f]).
step_down([2, 88/5, f], [2, 88/4, f]).
step_down([2, 88/6, f], [2, 88/5, f]).
step_down([2, 88/9, f], [2, 88/8, f]).
step_down([2, 88/10, f], [2, 88/9, f]).
step_down([2, 88/11, f], [2, 88/10, f]).
step_down([2, 88/12, f], [2, 88/11, f]).
step_down([2, 88/15, f], [2, 88/14, f]).
step_down([2, 88/16, f], [2, 88/15, f]).
step_down([2, 88/17, f], [2, 88/16, f]).
step_down([2, 88/18, f], [2, 88/17, f]).
step_down([2, 88/23, f], [2, 88/22, f]).
step_down([2, 88/24, f], [2, 88/23, f]).
step_down([2, 88/25, f], [2, 88/24, f]).
step_down([2, 88/26, f], [2, 88/25, f]).
step_down([2, 88/27, f], [2, 88/26, f]).
step_down([2, 88/28, f], [2, 88/27, f]).
step_down([2, 88/29, f], [2, 88/28, f]).
step_down([2, 88/30, f], [2, 88/29, f]).
step_down([2, 88/33, f], [2, 88/32, f]).
step_down([2, 88/34, f], [2, 88/33, f]).
step_down([2, 88/35, f], [2, 88/34, f]).
step_down([2, 88/36, f], [2, 88/35, f]).
step_down([2, 88/43, f], [2, 88/42, f]).
step_down([2, 88/44, f], [2, 88/43, f]).
step_down([2, 88/47, f], [2, 88/46, f]).
step_down([2, 88/48, f], [2, 88/47, f]).
step_down([2, 88/51, f], [2, 88/50, f]).
step_down([2, 88/52, f], [2, 88/51, f]).
step_down([2, 88/53, f], [2, 88/52, f]).
step_down([2, 88/54, f], [2, 88/53, f]).
step_down([2, 88/57, f], [2, 88/56, f]).
step_down([2, 88/58, f], [2, 88/57, f]).
step_down([2, 88/61, f], [2, 88/60, f]).
step_down([2, 88/62, f], [2, 88/61, f]).
step_down([2, 88/63, f], [2, 88/62, f]).
step_down([2, 88/64, f], [2, 88/63, f]).
step_down([2, 88/65, f], [2, 88/64, f]).
step_down([2, 88/66, f], [2, 88/65, f]).
step_down([2, 88/69, f], [2, 88/68, f]).
step_down([2, 88/70, f], [2, 88/69, f]).
step_down([2, 88/71, f], [2, 88/70, f]).
step_down([2, 88/72, f], [2, 88/71, f]).
step_down([2, 88/79, f], [2, 88/78, f]).
step_down([2, 88/80, f], [2, 88/79, f]).
step_down([2, 88/83, f], [2, 88/82, f]).
step_down([2, 88/84, f], [2, 88/83, f]).
step_down([2, 88/89, f], [2, 88/88, f]).
step_down([2, 88/90, f], [2, 88/89, f]).
step_down([2, 88/93, f], [2, 88/92, f]).
step_down([2, 88/94, f], [2, 88/93, f]).
step_down([2, 88/99, f], [2, 88/98, f]).
step_down([2, 88/100, f], [2, 88/99, f]).
step_down([2, 88/101, f], [2, 88/100, f]).
step_down([2, 88/102, f], [2, 88/101, f]).
step_down([2, 88/103, f], [2, 88/102, f]).
step_down([2, 88/104, f], [2, 88/103, f]).
step_down([2, 88/107, f], [2, 88/106, f]).
step_down([2, 88/108, f], [2, 88/107, f]).
step_down([2, 88/109, f], [2, 88/108, f]).
step_down([2, 88/110, f], [2, 88/109, f]).
step_down([2, 90/3, f], [2, 90/2, f]).
step_down([2, 90/4, f], [2, 90/3, f]).
step_down([2, 90/5, f], [2, 90/4, f]).
step_down([2, 90/6, f], [2, 90/5, f]).
step_down([2, 90/9, f], [2, 90/8, f]).
step_down([2, 90/10, f], [2, 90/9, f]).
step_down([2, 90/13, f], [2, 90/12, f]).
step_down([2, 90/14, f], [2, 90/13, f]).
step_down([2, 90/17, f], [2, 90/16, f]).
step_down([2, 90/18, f], [2, 90/17, f]).
step_down([2, 90/21, f], [2, 90/20, f]).
step_down([2, 90/22, f], [2, 90/21, f]).
step_down([2, 90/23, f], [2, 90/22, f]).
step_down([2, 90/24, f], [2, 90/23, f]).
step_down([2, 90/27, f], [2, 90/26, f]).
step_down([2, 90/28, f], [2, 90/27, f]).
step_down([2, 90/31, f], [2, 90/30, f]).
step_down([2, 90/32, f], [2, 90/31, f]).
step_down([2, 90/35, f], [2, 90/34, f]).
step_down([2, 90/36, f], [2, 90/35, f]).
step_down([2, 90/37, f], [2, 90/36, f]).
step_down([2, 90/38, f], [2, 90/37, f]).
step_down([2, 90/41, f], [2, 90/40, f]).
step_down([2, 90/42, f], [2, 90/41, f]).
step_down([2, 90/45, f], [2, 90/44, f]).
step_down([2, 90/46, f], [2, 90/45, f]).
step_down([2, 90/47, f], [2, 90/46, f]).
step_down([2, 90/48, f], [2, 90/47, f]).
step_down([2, 90/51, f], [2, 90/50, f]).
step_down([2, 90/52, f], [2, 90/51, f]).
step_down([2, 90/55, f], [2, 90/54, f]).
step_down([2, 90/56, f], [2, 90/55, f]).
step_down([2, 90/61, f], [2, 90/60, f]).
step_down([2, 90/62, f], [2, 90/61, f]).
step_down([2, 90/63, f], [2, 90/62, f]).
step_down([2, 90/64, f], [2, 90/63, f]).
step_down([2, 90/65, f], [2, 90/64, f]).
step_down([2, 90/66, f], [2, 90/65, f]).
step_down([2, 90/69, f], [2, 90/68, f]).
step_down([2, 90/70, f], [2, 90/69, f]).
step_down([2, 90/75, f], [2, 90/74, f]).
step_down([2, 90/76, f], [2, 90/75, f]).
step_down([2, 90/81, f], [2, 90/80, f]).
step_down([2, 90/82, f], [2, 90/81, f]).
step_down([2, 90/85, f], [2, 90/84, f]).
step_down([2, 90/86, f], [2, 90/85, f]).
step_down([2, 90/87, f], [2, 90/86, f]).
step_down([2, 90/88, f], [2, 90/87, f]).
step_down([2, 90/93, f], [2, 90/92, f]).
step_down([2, 90/94, f], [2, 90/93, f]).
step_down([2, 90/95, f], [2, 90/94, f]).
step_down([2, 90/96, f], [2, 90/95, f]).
step_down([2, 90/97, f], [2, 90/96, f]).
step_down([2, 90/98, f], [2, 90/97, f]).
step_down([2, 90/101, f], [2, 90/100, f]).
step_down([2, 90/102, f], [2, 90/101, f]).
step_down([2, 90/103, f], [2, 90/102, f]).
step_down([2, 90/104, f], [2, 90/103, f]).
step_down([2, 90/105, f], [2, 90/104, f]).
step_down([2, 90/106, f], [2, 90/105, f]).
step_down([2, 90/109, f], [2, 90/108, f]).
step_down([2, 90/110, f], [2, 90/109, f]).
step_down([2, 90/111, f], [2, 90/110, f]).
step_down([2, 90/112, f], [2, 90/111, f]).
step_down([2, 90/113, f], [2, 90/112, f]).
step_down([2, 90/114, f], [2, 90/113, f]).
step_down([2, 92/1, f], [2, 92/0, f]).
step_down([2, 92/2, f], [2, 92/1, f]).
step_down([2, 92/5, f], [2, 92/4, f]).
step_down([2, 92/6, f], [2, 92/5, f]).
step_down([2, 92/7, f], [2, 92/6, f]).
step_down([2, 92/8, f], [2, 92/7, f]).
step_down([2, 92/9, f], [2, 92/8, f]).
step_down([2, 92/10, f], [2, 92/9, f]).
step_down([2, 92/11, f], [2, 92/10, f]).
step_down([2, 92/12, f], [2, 92/11, f]).
step_down([2, 92/15, f], [2, 92/14, f]).
step_down([2, 92/16, f], [2, 92/15, f]).
step_down([2, 92/17, f], [2, 92/16, f]).
step_down([2, 92/18, f], [2, 92/17, f]).
step_down([2, 92/21, f], [2, 92/20, f]).
step_down([2, 92/22, f], [2, 92/21, f]).
step_down([2, 92/23, f], [2, 92/22, f]).
step_down([2, 92/24, f], [2, 92/23, f]).
step_down([2, 92/25, f], [2, 92/24, f]).
step_down([2, 92/26, f], [2, 92/25, f]).
step_down([2, 92/33, f], [2, 92/32, f]).
step_down([2, 92/34, f], [2, 92/33, f]).
step_down([2, 92/37, f], [2, 92/36, f]).
step_down([2, 92/38, f], [2, 92/37, f]).
step_down([2, 92/41, f], [2, 92/40, f]).
step_down([2, 92/42, f], [2, 92/41, f]).
step_down([2, 92/47, f], [2, 92/46, f]).
step_down([2, 92/48, f], [2, 92/47, f]).
step_down([2, 92/49, f], [2, 92/48, f]).
step_down([2, 92/50, f], [2, 92/49, f]).
step_down([2, 92/55, f], [2, 92/54, f]).
step_down([2, 92/56, f], [2, 92/55, f]).
step_down([2, 92/59, f], [2, 92/58, f]).
step_down([2, 92/60, f], [2, 92/59, f]).
step_down([2, 92/63, f], [2, 92/62, f]).
step_down([2, 92/64, f], [2, 92/63, f]).
step_down([2, 92/67, f], [2, 92/66, f]).
step_down([2, 92/68, f], [2, 92/67, f]).
step_down([2, 92/71, f], [2, 92/70, f]).
step_down([2, 92/72, f], [2, 92/71, f]).
step_down([2, 92/75, f], [2, 92/74, f]).
step_down([2, 92/76, f], [2, 92/75, f]).
step_down([2, 92/77, f], [2, 92/76, f]).
step_down([2, 92/78, f], [2, 92/77, f]).
step_down([2, 92/83, f], [2, 92/82, f]).
step_down([2, 92/84, f], [2, 92/83, f]).
step_down([2, 92/85, f], [2, 92/84, f]).
step_down([2, 92/86, f], [2, 92/85, f]).
step_down([2, 92/91, f], [2, 92/90, f]).
step_down([2, 92/92, f], [2, 92/91, f]).
step_down([2, 92/93, f], [2, 92/92, f]).
step_down([2, 92/94, f], [2, 92/93, f]).
step_down([2, 92/97, f], [2, 92/96, f]).
step_down([2, 92/98, f], [2, 92/97, f]).
step_down([2, 92/101, f], [2, 92/100, f]).
step_down([2, 92/102, f], [2, 92/101, f]).
step_down([2, 92/107, f], [2, 92/106, f]).
step_down([2, 92/108, f], [2, 92/107, f]).
step_down([2, 92/109, f], [2, 92/108, f]).
step_down([2, 92/110, f], [2, 92/109, f]).
step_down([2, 92/113, f], [2, 92/112, f]).
step_down([2, 92/114, f], [2, 92/113, f]).
step_down([2, 92/115, f], [2, 92/114, f]).
step_down([2, 92/116, f], [2, 92/115, f]).
step_down([2, 92/117, f], [2, 92/116, f]).
step_down([2, 92/118, f], [2, 92/117, f]).
step_down([2, 94/1, f], [2, 94/0, f]).
step_down([2, 94/2, f], [2, 94/1, f]).
step_down([2, 94/3, f], [2, 94/2, f]).
step_down([2, 94/4, f], [2, 94/3, f]).
step_down([2, 94/7, f], [2, 94/6, f]).
step_down([2, 94/8, f], [2, 94/7, f]).
step_down([2, 94/13, f], [2, 94/12, f]).
step_down([2, 94/14, f], [2, 94/13, f]).
step_down([2, 94/17, f], [2, 94/16, f]).
step_down([2, 94/18, f], [2, 94/17, f]).
step_down([2, 94/25, f], [2, 94/24, f]).
step_down([2, 94/26, f], [2, 94/25, f]).
step_down([2, 94/27, f], [2, 94/26, f]).
step_down([2, 94/28, f], [2, 94/27, f]).
step_down([2, 94/29, f], [2, 94/28, f]).
step_down([2, 94/30, f], [2, 94/29, f]).
step_down([2, 94/33, f], [2, 94/32, f]).
step_down([2, 94/34, f], [2, 94/33, f]).
step_down([2, 94/39, f], [2, 94/38, f]).
step_down([2, 94/40, f], [2, 94/39, f]).
step_down([2, 94/41, f], [2, 94/40, f]).
step_down([2, 94/42, f], [2, 94/41, f]).
step_down([2, 94/47, f], [2, 94/46, f]).
step_down([2, 94/48, f], [2, 94/47, f]).
step_down([2, 94/49, f], [2, 94/48, f]).
step_down([2, 94/50, f], [2, 94/49, f]).
step_down([2, 94/51, f], [2, 94/50, f]).
step_down([2, 94/52, f], [2, 94/51, f]).
step_down([2, 94/55, f], [2, 94/54, f]).
step_down([2, 94/56, f], [2, 94/55, f]).
step_down([2, 94/57, f], [2, 94/56, f]).
step_down([2, 94/58, f], [2, 94/57, f]).
step_down([2, 94/59, f], [2, 94/58, f]).
step_down([2, 94/60, f], [2, 94/59, f]).
step_down([2, 94/63, f], [2, 94/62, f]).
step_down([2, 94/64, f], [2, 94/63, f]).
step_down([2, 94/65, f], [2, 94/64, f]).
step_down([2, 94/66, f], [2, 94/65, f]).
step_down([2, 94/67, f], [2, 94/66, f]).
step_down([2, 94/68, f], [2, 94/67, f]).
step_down([2, 94/71, f], [2, 94/70, f]).
step_down([2, 94/72, f], [2, 94/71, f]).
step_down([2, 94/75, f], [2, 94/74, f]).
step_down([2, 94/76, f], [2, 94/75, f]).
step_down([2, 94/81, f], [2, 94/80, f]).
step_down([2, 94/82, f], [2, 94/81, f]).
step_down([2, 94/85, f], [2, 94/84, f]).
step_down([2, 94/86, f], [2, 94/85, f]).
step_down([2, 94/89, f], [2, 94/88, f]).
step_down([2, 94/90, f], [2, 94/89, f]).
step_down([2, 94/91, f], [2, 94/90, f]).
step_down([2, 94/92, f], [2, 94/91, f]).
step_down([2, 94/93, f], [2, 94/92, f]).
step_down([2, 94/94, f], [2, 94/93, f]).
step_down([2, 94/99, f], [2, 94/98, f]).
step_down([2, 94/100, f], [2, 94/99, f]).
step_down([2, 94/103, f], [2, 94/102, f]).
step_down([2, 94/104, f], [2, 94/103, f]).
step_down([2, 94/109, f], [2, 94/108, f]).
step_down([2, 94/110, f], [2, 94/109, f]).
step_down([2, 94/113, f], [2, 94/112, f]).
step_down([2, 94/114, f], [2, 94/113, f]).
step_down([2, 94/115, f], [2, 94/114, f]).
step_down([2, 94/116, f], [2, 94/115, f]).
step_down([2, 96/1, f], [2, 96/0, f]).
step_down([2, 96/2, f], [2, 96/1, f]).
step_down([2, 96/3, f], [2, 96/2, f]).
step_down([2, 96/4, f], [2, 96/3, f]).
step_down([2, 96/9, f], [2, 96/8, f]).
step_down([2, 96/10, f], [2, 96/9, f]).
step_down([2, 96/13, f], [2, 96/12, f]).
step_down([2, 96/14, f], [2, 96/13, f]).
step_down([2, 96/19, f], [2, 96/18, f]).
step_down([2, 96/20, f], [2, 96/19, f]).
step_down([2, 96/27, f], [2, 96/26, f]).
step_down([2, 96/28, f], [2, 96/27, f]).
step_down([2, 96/29, f], [2, 96/28, f]).
step_down([2, 96/30, f], [2, 96/29, f]).
step_down([2, 96/31, f], [2, 96/30, f]).
step_down([2, 96/32, f], [2, 96/31, f]).
step_down([2, 96/37, f], [2, 96/36, f]).
step_down([2, 96/38, f], [2, 96/37, f]).
step_down([2, 96/39, f], [2, 96/38, f]).
step_down([2, 96/40, f], [2, 96/39, f]).
step_down([2, 96/43, f], [2, 96/42, f]).
step_down([2, 96/44, f], [2, 96/43, f]).
step_down([2, 96/45, f], [2, 96/44, f]).
step_down([2, 96/46, f], [2, 96/45, f]).
step_down([2, 96/49, f], [2, 96/48, f]).
step_down([2, 96/50, f], [2, 96/49, f]).
step_down([2, 96/51, f], [2, 96/50, f]).
step_down([2, 96/52, f], [2, 96/51, f]).
step_down([2, 96/59, f], [2, 96/58, f]).
step_down([2, 96/60, f], [2, 96/59, f]).
step_down([2, 96/67, f], [2, 96/66, f]).
step_down([2, 96/68, f], [2, 96/67, f]).
step_down([2, 96/73, f], [2, 96/72, f]).
step_down([2, 96/74, f], [2, 96/73, f]).
step_down([2, 96/81, f], [2, 96/80, f]).
step_down([2, 96/82, f], [2, 96/81, f]).
step_down([2, 96/87, f], [2, 96/86, f]).
step_down([2, 96/88, f], [2, 96/87, f]).
step_down([2, 96/91, f], [2, 96/90, f]).
step_down([2, 96/92, f], [2, 96/91, f]).
step_down([2, 96/93, f], [2, 96/92, f]).
step_down([2, 96/94, f], [2, 96/93, f]).
step_down([2, 96/97, f], [2, 96/96, f]).
step_down([2, 96/98, f], [2, 96/97, f]).
step_down([2, 96/101, f], [2, 96/100, f]).
step_down([2, 96/102, f], [2, 96/101, f]).
step_down([2, 96/105, f], [2, 96/104, f]).
step_down([2, 96/106, f], [2, 96/105, f]).
step_down([2, 96/111, f], [2, 96/110, f]).
step_down([2, 96/112, f], [2, 96/111, f]).
step_down([2, 96/115, f], [2, 96/114, f]).
step_down([2, 96/116, f], [2, 96/115, f]).
step_down([2, 98/5, f], [2, 98/4, f]).
step_down([2, 98/6, f], [2, 98/5, f]).
step_down([2, 98/9, f], [2, 98/8, f]).
step_down([2, 98/10, f], [2, 98/9, f]).
step_down([2, 98/11, f], [2, 98/10, f]).
step_down([2, 98/12, f], [2, 98/11, f]).
step_down([2, 98/21, f], [2, 98/20, f]).
step_down([2, 98/22, f], [2, 98/21, f]).
step_down([2, 98/31, f], [2, 98/30, f]).
step_down([2, 98/32, f], [2, 98/31, f]).
step_down([2, 98/35, f], [2, 98/34, f]).
step_down([2, 98/36, f], [2, 98/35, f]).
step_down([2, 98/37, f], [2, 98/36, f]).
step_down([2, 98/38, f], [2, 98/37, f]).
step_down([2, 98/45, f], [2, 98/44, f]).
step_down([2, 98/46, f], [2, 98/45, f]).
step_down([2, 98/53, f], [2, 98/52, f]).
step_down([2, 98/54, f], [2, 98/53, f]).
step_down([2, 98/59, f], [2, 98/58, f]).
step_down([2, 98/60, f], [2, 98/59, f]).
step_down([2, 98/63, f], [2, 98/62, f]).
step_down([2, 98/64, f], [2, 98/63, f]).
step_down([2, 98/69, f], [2, 98/68, f]).
step_down([2, 98/70, f], [2, 98/69, f]).
step_down([2, 98/75, f], [2, 98/74, f]).
step_down([2, 98/76, f], [2, 98/75, f]).
step_down([2, 98/77, f], [2, 98/76, f]).
step_down([2, 98/78, f], [2, 98/77, f]).
step_down([2, 98/81, f], [2, 98/80, f]).
step_down([2, 98/82, f], [2, 98/81, f]).
step_down([2, 98/89, f], [2, 98/88, f]).
step_down([2, 98/90, f], [2, 98/89, f]).
step_down([2, 98/91, f], [2, 98/90, f]).
step_down([2, 98/92, f], [2, 98/91, f]).
step_down([2, 98/95, f], [2, 98/94, f]).
step_down([2, 98/96, f], [2, 98/95, f]).
step_down([2, 98/97, f], [2, 98/96, f]).
step_down([2, 98/98, f], [2, 98/97, f]).
step_down([2, 98/101, f], [2, 98/100, f]).
step_down([2, 98/102, f], [2, 98/101, f]).
step_down([2, 98/103, f], [2, 98/102, f]).
step_down([2, 98/104, f], [2, 98/103, f]).
step_down([2, 98/105, f], [2, 98/104, f]).
step_down([2, 98/106, f], [2, 98/105, f]).
step_down([2, 98/111, f], [2, 98/110, f]).
step_down([2, 98/112, f], [2, 98/111, f]).
step_down([2, 100/3, f], [2, 100/2, f]).
step_down([2, 100/4, f], [2, 100/3, f]).
step_down([2, 100/5, f], [2, 100/4, f]).
step_down([2, 100/6, f], [2, 100/5, f]).
step_down([2, 100/7, f], [2, 100/6, f]).
step_down([2, 100/8, f], [2, 100/7, f]).
step_down([2, 100/9, f], [2, 100/8, f]).
step_down([2, 100/10, f], [2, 100/9, f]).
step_down([2, 100/17, f], [2, 100/16, f]).
step_down([2, 100/18, f], [2, 100/17, f]).
step_down([2, 100/31, f], [2, 100/30, f]).
step_down([2, 100/32, f], [2, 100/31, f]).
step_down([2, 100/33, f], [2, 100/32, f]).
step_down([2, 100/34, f], [2, 100/33, f]).
step_down([2, 100/35, f], [2, 100/34, f]).
step_down([2, 100/36, f], [2, 100/35, f]).
step_down([2, 100/41, f], [2, 100/40, f]).
step_down([2, 100/42, f], [2, 100/41, f]).
step_down([2, 100/47, f], [2, 100/46, f]).
step_down([2, 100/48, f], [2, 100/47, f]).
step_down([2, 100/51, f], [2, 100/50, f]).
step_down([2, 100/52, f], [2, 100/51, f]).
step_down([2, 100/57, f], [2, 100/56, f]).
step_down([2, 100/58, f], [2, 100/57, f]).
step_down([2, 100/63, f], [2, 100/62, f]).
step_down([2, 100/64, f], [2, 100/63, f]).
step_down([2, 100/67, f], [2, 100/66, f]).
step_down([2, 100/68, f], [2, 100/67, f]).
step_down([2, 100/71, f], [2, 100/70, f]).
step_down([2, 100/72, f], [2, 100/71, f]).
step_down([2, 100/73, f], [2, 100/72, f]).
step_down([2, 100/74, f], [2, 100/73, f]).
step_down([2, 100/75, f], [2, 100/74, f]).
step_down([2, 100/76, f], [2, 100/75, f]).
step_down([2, 100/77, f], [2, 100/76, f]).
step_down([2, 100/78, f], [2, 100/77, f]).
step_down([2, 100/79, f], [2, 100/78, f]).
step_down([2, 100/80, f], [2, 100/79, f]).
step_down([2, 100/81, f], [2, 100/80, f]).
step_down([2, 100/82, f], [2, 100/81, f]).
step_down([2, 100/83, f], [2, 100/82, f]).
step_down([2, 100/84, f], [2, 100/83, f]).
step_down([2, 100/91, f], [2, 100/90, f]).
step_down([2, 100/92, f], [2, 100/91, f]).
step_down([2, 100/95, f], [2, 100/94, f]).
step_down([2, 100/96, f], [2, 100/95, f]).
step_down([2, 100/97, f], [2, 100/96, f]).
step_down([2, 100/98, f], [2, 100/97, f]).
step_down([2, 100/101, f], [2, 100/100, f]).
step_down([2, 100/102, f], [2, 100/101, f]).
step_down([2, 100/109, f], [2, 100/108, f]).
step_down([2, 100/110, f], [2, 100/109, f]).
step_down([2, 100/115, f], [2, 100/114, f]).
step_down([2, 100/116, f], [2, 100/115, f]).
step_down([2, 100/117, f], [2, 100/116, f]).
step_down([2, 100/118, f], [2, 100/117, f]).
step_down([2, 102/3, f], [2, 102/2, f]).
step_down([2, 102/4, f], [2, 102/3, f]).
step_down([2, 102/7, f], [2, 102/6, f]).
step_down([2, 102/8, f], [2, 102/7, f]).
step_down([2, 102/9, f], [2, 102/8, f]).
step_down([2, 102/10, f], [2, 102/9, f]).
step_down([2, 102/11, f], [2, 102/10, f]).
step_down([2, 102/12, f], [2, 102/11, f]).
step_down([2, 102/13, f], [2, 102/12, f]).
step_down([2, 102/14, f], [2, 102/13, f]).
step_down([2, 102/17, f], [2, 102/16, f]).
step_down([2, 102/18, f], [2, 102/17, f]).
step_down([2, 102/19, f], [2, 102/18, f]).
step_down([2, 102/20, f], [2, 102/19, f]).
step_down([2, 102/21, f], [2, 102/20, f]).
step_down([2, 102/22, f], [2, 102/21, f]).
step_down([2, 102/25, f], [2, 102/24, f]).
step_down([2, 102/26, f], [2, 102/25, f]).
step_down([2, 102/29, f], [2, 102/28, f]).
step_down([2, 102/30, f], [2, 102/29, f]).
step_down([2, 102/31, f], [2, 102/30, f]).
step_down([2, 102/32, f], [2, 102/31, f]).
step_down([2, 102/33, f], [2, 102/32, f]).
step_down([2, 102/34, f], [2, 102/33, f]).
step_down([2, 102/37, f], [2, 102/36, f]).
step_down([2, 102/38, f], [2, 102/37, f]).
step_down([2, 102/39, f], [2, 102/38, f]).
step_down([2, 102/40, f], [2, 102/39, f]).
step_down([2, 102/41, f], [2, 102/40, f]).
step_down([2, 102/42, f], [2, 102/41, f]).
step_down([2, 102/45, f], [2, 102/44, f]).
step_down([2, 102/46, f], [2, 102/45, f]).
step_down([2, 102/51, f], [2, 102/50, f]).
step_down([2, 102/52, f], [2, 102/51, f]).
step_down([2, 102/57, f], [2, 102/56, f]).
step_down([2, 102/58, f], [2, 102/57, f]).
step_down([2, 102/65, f], [2, 102/64, f]).
step_down([2, 102/66, f], [2, 102/65, f]).
step_down([2, 102/67, f], [2, 102/66, f]).
step_down([2, 102/68, f], [2, 102/67, f]).
step_down([2, 102/71, f], [2, 102/70, f]).
step_down([2, 102/72, f], [2, 102/71, f]).
step_down([2, 102/73, f], [2, 102/72, f]).
step_down([2, 102/74, f], [2, 102/73, f]).
step_down([2, 102/75, f], [2, 102/74, f]).
step_down([2, 102/76, f], [2, 102/75, f]).
step_down([2, 102/77, f], [2, 102/76, f]).
step_down([2, 102/78, f], [2, 102/77, f]).
step_down([2, 102/81, f], [2, 102/80, f]).
step_down([2, 102/82, f], [2, 102/81, f]).
step_down([2, 102/87, f], [2, 102/86, f]).
step_down([2, 102/88, f], [2, 102/87, f]).
step_down([2, 102/95, f], [2, 102/94, f]).
step_down([2, 102/96, f], [2, 102/95, f]).
step_down([2, 102/97, f], [2, 102/96, f]).
step_down([2, 102/98, f], [2, 102/97, f]).
step_down([2, 102/99, f], [2, 102/98, f]).
step_down([2, 102/100, f], [2, 102/99, f]).
step_down([2, 102/101, f], [2, 102/100, f]).
step_down([2, 102/102, f], [2, 102/101, f]).
step_down([2, 102/103, f], [2, 102/102, f]).
step_down([2, 102/104, f], [2, 102/103, f]).
step_down([2, 102/107, f], [2, 102/106, f]).
step_down([2, 102/108, f], [2, 102/107, f]).
step_down([2, 102/109, f], [2, 102/108, f]).
step_down([2, 102/110, f], [2, 102/109, f]).
step_down([2, 102/115, f], [2, 102/114, f]).
step_down([2, 102/116, f], [2, 102/115, f]).
step_down([2, 102/117, f], [2, 102/116, f]).
step_down([2, 102/118, f], [2, 102/117, f]).
step_down([2, 104/1, f], [2, 104/0, f]).
step_down([2, 104/2, f], [2, 104/1, f]).
step_down([2, 104/5, f], [2, 104/4, f]).
step_down([2, 104/6, f], [2, 104/5, f]).
step_down([2, 104/9, f], [2, 104/8, f]).
step_down([2, 104/10, f], [2, 104/9, f]).
step_down([2, 104/11, f], [2, 104/10, f]).
step_down([2, 104/12, f], [2, 104/11, f]).
step_down([2, 104/19, f], [2, 104/18, f]).
step_down([2, 104/20, f], [2, 104/19, f]).
step_down([2, 104/21, f], [2, 104/20, f]).
step_down([2, 104/22, f], [2, 104/21, f]).
step_down([2, 104/25, f], [2, 104/24, f]).
step_down([2, 104/26, f], [2, 104/25, f]).
step_down([2, 104/33, f], [2, 104/32, f]).
step_down([2, 104/34, f], [2, 104/33, f]).
step_down([2, 104/35, f], [2, 104/34, f]).
step_down([2, 104/36, f], [2, 104/35, f]).
step_down([2, 104/39, f], [2, 104/38, f]).
step_down([2, 104/40, f], [2, 104/39, f]).
step_down([2, 104/45, f], [2, 104/44, f]).
step_down([2, 104/46, f], [2, 104/45, f]).
step_down([2, 104/47, f], [2, 104/46, f]).
step_down([2, 104/48, f], [2, 104/47, f]).
step_down([2, 104/53, f], [2, 104/52, f]).
step_down([2, 104/54, f], [2, 104/53, f]).
step_down([2, 104/59, f], [2, 104/58, f]).
step_down([2, 104/60, f], [2, 104/59, f]).
step_down([2, 104/61, f], [2, 104/60, f]).
step_down([2, 104/62, f], [2, 104/61, f]).
step_down([2, 104/67, f], [2, 104/66, f]).
step_down([2, 104/68, f], [2, 104/67, f]).
step_down([2, 104/69, f], [2, 104/68, f]).
step_down([2, 104/70, f], [2, 104/69, f]).
step_down([2, 104/73, f], [2, 104/72, f]).
step_down([2, 104/74, f], [2, 104/73, f]).
step_down([2, 104/75, f], [2, 104/74, f]).
step_down([2, 104/76, f], [2, 104/75, f]).
step_down([2, 104/77, f], [2, 104/76, f]).
step_down([2, 104/78, f], [2, 104/77, f]).
step_down([2, 104/79, f], [2, 104/78, f]).
step_down([2, 104/80, f], [2, 104/79, f]).
step_down([2, 104/87, f], [2, 104/86, f]).
step_down([2, 104/88, f], [2, 104/87, f]).
step_down([2, 104/89, f], [2, 104/88, f]).
step_down([2, 104/90, f], [2, 104/89, f]).
step_down([2, 104/95, f], [2, 104/94, f]).
step_down([2, 104/96, f], [2, 104/95, f]).
step_down([2, 104/97, f], [2, 104/96, f]).
step_down([2, 104/98, f], [2, 104/97, f]).
step_down([2, 104/101, f], [2, 104/100, f]).
step_down([2, 104/102, f], [2, 104/101, f]).
step_down([2, 104/105, f], [2, 104/104, f]).
step_down([2, 104/106, f], [2, 104/105, f]).
step_down([2, 104/109, f], [2, 104/108, f]).
step_down([2, 104/110, f], [2, 104/109, f]).
step_down([2, 104/113, f], [2, 104/112, f]).
step_down([2, 104/114, f], [2, 104/113, f]).
step_down([2, 104/115, f], [2, 104/114, f]).
step_down([2, 104/116, f], [2, 104/115, f]).
step_down([2, 106/3, f], [2, 106/2, f]).
step_down([2, 106/4, f], [2, 106/3, f]).
step_down([2, 106/7, f], [2, 106/6, f]).
step_down([2, 106/8, f], [2, 106/7, f]).
step_down([2, 106/9, f], [2, 106/8, f]).
step_down([2, 106/10, f], [2, 106/9, f]).
step_down([2, 106/11, f], [2, 106/10, f]).
step_down([2, 106/12, f], [2, 106/11, f]).
step_down([2, 106/21, f], [2, 106/20, f]).
step_down([2, 106/22, f], [2, 106/21, f]).
step_down([2, 106/23, f], [2, 106/22, f]).
step_down([2, 106/24, f], [2, 106/23, f]).
step_down([2, 106/27, f], [2, 106/26, f]).
step_down([2, 106/28, f], [2, 106/27, f]).
step_down([2, 106/33, f], [2, 106/32, f]).
step_down([2, 106/34, f], [2, 106/33, f]).
step_down([2, 106/41, f], [2, 106/40, f]).
step_down([2, 106/42, f], [2, 106/41, f]).
step_down([2, 106/49, f], [2, 106/48, f]).
step_down([2, 106/50, f], [2, 106/49, f]).
step_down([2, 106/53, f], [2, 106/52, f]).
step_down([2, 106/54, f], [2, 106/53, f]).
step_down([2, 106/61, f], [2, 106/60, f]).
step_down([2, 106/62, f], [2, 106/61, f]).
step_down([2, 106/63, f], [2, 106/62, f]).
step_down([2, 106/64, f], [2, 106/63, f]).
step_down([2, 106/69, f], [2, 106/68, f]).
step_down([2, 106/70, f], [2, 106/69, f]).
step_down([2, 106/71, f], [2, 106/70, f]).
step_down([2, 106/72, f], [2, 106/71, f]).
step_down([2, 106/75, f], [2, 106/74, f]).
step_down([2, 106/76, f], [2, 106/75, f]).
step_down([2, 106/77, f], [2, 106/76, f]).
step_down([2, 106/78, f], [2, 106/77, f]).
step_down([2, 106/79, f], [2, 106/78, f]).
step_down([2, 106/80, f], [2, 106/79, f]).
step_down([2, 106/89, f], [2, 106/88, f]).
step_down([2, 106/90, f], [2, 106/89, f]).
step_down([2, 106/93, f], [2, 106/92, f]).
step_down([2, 106/94, f], [2, 106/93, f]).
step_down([2, 106/97, f], [2, 106/96, f]).
step_down([2, 106/98, f], [2, 106/97, f]).
step_down([2, 106/103, f], [2, 106/102, f]).
step_down([2, 106/104, f], [2, 106/103, f]).
step_down([2, 106/107, f], [2, 106/106, f]).
step_down([2, 106/108, f], [2, 106/107, f]).
step_down([2, 106/109, f], [2, 106/108, f]).
step_down([2, 106/110, f], [2, 106/109, f]).
step_down([2, 106/111, f], [2, 106/110, f]).
step_down([2, 106/112, f], [2, 106/111, f]).
step_down([2, 106/113, f], [2, 106/112, f]).
step_down([2, 106/114, f], [2, 106/113, f]).
step_down([2, 108/9, f], [2, 108/8, f]).
step_down([2, 108/10, f], [2, 108/9, f]).
step_down([2, 108/19, f], [2, 108/18, f]).
step_down([2, 108/20, f], [2, 108/19, f]).
step_down([2, 108/21, f], [2, 108/20, f]).
step_down([2, 108/22, f], [2, 108/21, f]).
step_down([2, 108/23, f], [2, 108/22, f]).
step_down([2, 108/24, f], [2, 108/23, f]).
step_down([2, 108/29, f], [2, 108/28, f]).
step_down([2, 108/30, f], [2, 108/29, f]).
step_down([2, 108/35, f], [2, 108/34, f]).
step_down([2, 108/36, f], [2, 108/35, f]).
step_down([2, 108/41, f], [2, 108/40, f]).
step_down([2, 108/42, f], [2, 108/41, f]).
step_down([2, 108/43, f], [2, 108/42, f]).
step_down([2, 108/44, f], [2, 108/43, f]).
step_down([2, 108/49, f], [2, 108/48, f]).
step_down([2, 108/50, f], [2, 108/49, f]).
step_down([2, 108/53, f], [2, 108/52, f]).
step_down([2, 108/54, f], [2, 108/53, f]).
step_down([2, 108/55, f], [2, 108/54, f]).
step_down([2, 108/56, f], [2, 108/55, f]).
step_down([2, 108/59, f], [2, 108/58, f]).
step_down([2, 108/60, f], [2, 108/59, f]).
step_down([2, 108/61, f], [2, 108/60, f]).
step_down([2, 108/62, f], [2, 108/61, f]).
step_down([2, 108/69, f], [2, 108/68, f]).
step_down([2, 108/70, f], [2, 108/69, f]).
step_down([2, 108/71, f], [2, 108/70, f]).
step_down([2, 108/72, f], [2, 108/71, f]).
step_down([2, 108/79, f], [2, 108/78, f]).
step_down([2, 108/80, f], [2, 108/79, f]).
step_down([2, 108/85, f], [2, 108/84, f]).
step_down([2, 108/86, f], [2, 108/85, f]).
step_down([2, 108/89, f], [2, 108/88, f]).
step_down([2, 108/90, f], [2, 108/89, f]).
step_down([2, 108/99, f], [2, 108/98, f]).
step_down([2, 108/100, f], [2, 108/99, f]).
step_down([2, 108/103, f], [2, 108/102, f]).
step_down([2, 108/104, f], [2, 108/103, f]).
step_down([2, 108/109, f], [2, 108/108, f]).
step_down([2, 108/110, f], [2, 108/109, f]).
step_down([2, 108/111, f], [2, 108/110, f]).
step_down([2, 108/112, f], [2, 108/111, f]).
step_down([2, 108/113, f], [2, 108/112, f]).
step_down([2, 108/114, f], [2, 108/113, f]).
step_down([2, 108/115, f], [2, 108/114, f]).
step_down([2, 108/116, f], [2, 108/115, f]).
step_down([2, 110/5, f], [2, 110/4, f]).
step_down([2, 110/6, f], [2, 110/5, f]).
step_down([2, 110/7, f], [2, 110/6, f]).
step_down([2, 110/8, f], [2, 110/7, f]).
step_down([2, 110/9, f], [2, 110/8, f]).
step_down([2, 110/10, f], [2, 110/9, f]).
step_down([2, 110/13, f], [2, 110/12, f]).
step_down([2, 110/14, f], [2, 110/13, f]).
step_down([2, 110/17, f], [2, 110/16, f]).
step_down([2, 110/18, f], [2, 110/17, f]).
step_down([2, 110/19, f], [2, 110/18, f]).
step_down([2, 110/20, f], [2, 110/19, f]).
step_down([2, 110/21, f], [2, 110/20, f]).
step_down([2, 110/22, f], [2, 110/21, f]).
step_down([2, 110/27, f], [2, 110/26, f]).
step_down([2, 110/28, f], [2, 110/27, f]).
step_down([2, 110/33, f], [2, 110/32, f]).
step_down([2, 110/34, f], [2, 110/33, f]).
step_down([2, 110/37, f], [2, 110/36, f]).
step_down([2, 110/38, f], [2, 110/37, f]).
step_down([2, 110/41, f], [2, 110/40, f]).
step_down([2, 110/42, f], [2, 110/41, f]).
step_down([2, 110/43, f], [2, 110/42, f]).
step_down([2, 110/44, f], [2, 110/43, f]).
step_down([2, 110/45, f], [2, 110/44, f]).
step_down([2, 110/46, f], [2, 110/45, f]).
step_down([2, 110/55, f], [2, 110/54, f]).
step_down([2, 110/56, f], [2, 110/55, f]).
step_down([2, 110/57, f], [2, 110/56, f]).
step_down([2, 110/58, f], [2, 110/57, f]).
step_down([2, 110/59, f], [2, 110/58, f]).
step_down([2, 110/60, f], [2, 110/59, f]).
step_down([2, 110/61, f], [2, 110/60, f]).
step_down([2, 110/62, f], [2, 110/61, f]).
step_down([2, 110/63, f], [2, 110/62, f]).
step_down([2, 110/64, f], [2, 110/63, f]).
step_down([2, 110/67, f], [2, 110/66, f]).
step_down([2, 110/68, f], [2, 110/67, f]).
step_down([2, 110/69, f], [2, 110/68, f]).
step_down([2, 110/70, f], [2, 110/69, f]).
step_down([2, 110/75, f], [2, 110/74, f]).
step_down([2, 110/76, f], [2, 110/75, f]).
step_down([2, 110/81, f], [2, 110/80, f]).
step_down([2, 110/82, f], [2, 110/81, f]).
step_down([2, 110/85, f], [2, 110/84, f]).
step_down([2, 110/86, f], [2, 110/85, f]).
step_down([2, 110/91, f], [2, 110/90, f]).
step_down([2, 110/92, f], [2, 110/91, f]).
step_down([2, 110/105, f], [2, 110/104, f]).
step_down([2, 110/106, f], [2, 110/105, f]).
step_down([2, 110/111, f], [2, 110/110, f]).
step_down([2, 110/112, f], [2, 110/111, f]).
step_down([2, 110/113, f], [2, 110/112, f]).
step_down([2, 110/114, f], [2, 110/113, f]).
step_down([2, 110/115, f], [2, 110/114, f]).
step_down([2, 110/116, f], [2, 110/115, f]).
step_down([2, 112/3, f], [2, 112/2, f]).
step_down([2, 112/4, f], [2, 112/3, f]).
step_down([2, 112/5, f], [2, 112/4, f]).
step_down([2, 112/6, f], [2, 112/5, f]).
step_down([2, 112/7, f], [2, 112/6, f]).
step_down([2, 112/8, f], [2, 112/7, f]).
step_down([2, 112/9, f], [2, 112/8, f]).
step_down([2, 112/10, f], [2, 112/9, f]).
step_down([2, 112/11, f], [2, 112/10, f]).
step_down([2, 112/12, f], [2, 112/11, f]).
step_down([2, 112/15, f], [2, 112/14, f]).
step_down([2, 112/16, f], [2, 112/15, f]).
step_down([2, 112/19, f], [2, 112/18, f]).
step_down([2, 112/20, f], [2, 112/19, f]).
step_down([2, 112/25, f], [2, 112/24, f]).
step_down([2, 112/26, f], [2, 112/25, f]).
step_down([2, 112/31, f], [2, 112/30, f]).
step_down([2, 112/32, f], [2, 112/31, f]).
step_down([2, 112/39, f], [2, 112/38, f]).
step_down([2, 112/40, f], [2, 112/39, f]).
step_down([2, 112/43, f], [2, 112/42, f]).
step_down([2, 112/44, f], [2, 112/43, f]).
step_down([2, 112/45, f], [2, 112/44, f]).
step_down([2, 112/46, f], [2, 112/45, f]).
step_down([2, 112/47, f], [2, 112/46, f]).
step_down([2, 112/48, f], [2, 112/47, f]).
step_down([2, 112/51, f], [2, 112/50, f]).
step_down([2, 112/52, f], [2, 112/51, f]).
step_down([2, 112/57, f], [2, 112/56, f]).
step_down([2, 112/58, f], [2, 112/57, f]).
step_down([2, 112/59, f], [2, 112/58, f]).
step_down([2, 112/60, f], [2, 112/59, f]).
step_down([2, 112/61, f], [2, 112/60, f]).
step_down([2, 112/62, f], [2, 112/61, f]).
step_down([2, 112/63, f], [2, 112/62, f]).
step_down([2, 112/64, f], [2, 112/63, f]).
step_down([2, 112/69, f], [2, 112/68, f]).
step_down([2, 112/70, f], [2, 112/69, f]).
step_down([2, 112/73, f], [2, 112/72, f]).
step_down([2, 112/74, f], [2, 112/73, f]).
step_down([2, 112/77, f], [2, 112/76, f]).
step_down([2, 112/78, f], [2, 112/77, f]).
step_down([2, 112/79, f], [2, 112/78, f]).
step_down([2, 112/80, f], [2, 112/79, f]).
step_down([2, 112/81, f], [2, 112/80, f]).
step_down([2, 112/82, f], [2, 112/81, f]).
step_down([2, 112/83, f], [2, 112/82, f]).
step_down([2, 112/84, f], [2, 112/83, f]).
step_down([2, 112/89, f], [2, 112/88, f]).
step_down([2, 112/90, f], [2, 112/89, f]).
step_down([2, 112/91, f], [2, 112/90, f]).
step_down([2, 112/92, f], [2, 112/91, f]).
step_down([2, 112/95, f], [2, 112/94, f]).
step_down([2, 112/96, f], [2, 112/95, f]).
step_down([2, 112/101, f], [2, 112/100, f]).
step_down([2, 112/102, f], [2, 112/101, f]).
step_down([2, 112/109, f], [2, 112/108, f]).
step_down([2, 112/110, f], [2, 112/109, f]).
step_down([2, 112/111, f], [2, 112/110, f]).
step_down([2, 112/112, f], [2, 112/111, f]).
step_down([2, 112/113, f], [2, 112/112, f]).
step_down([2, 112/114, f], [2, 112/113, f]).
step_down([2, 114/5, f], [2, 114/4, f]).
step_down([2, 114/6, f], [2, 114/5, f]).
step_down([2, 114/11, f], [2, 114/10, f]).
step_down([2, 114/12, f], [2, 114/11, f]).
step_down([2, 114/15, f], [2, 114/14, f]).
step_down([2, 114/16, f], [2, 114/15, f]).
step_down([2, 114/23, f], [2, 114/22, f]).
step_down([2, 114/24, f], [2, 114/23, f]).
step_down([2, 114/29, f], [2, 114/28, f]).
step_down([2, 114/30, f], [2, 114/29, f]).
step_down([2, 114/37, f], [2, 114/36, f]).
step_down([2, 114/38, f], [2, 114/37, f]).
step_down([2, 114/41, f], [2, 114/40, f]).
step_down([2, 114/42, f], [2, 114/41, f]).
step_down([2, 114/47, f], [2, 114/46, f]).
step_down([2, 114/48, f], [2, 114/47, f]).
step_down([2, 114/53, f], [2, 114/52, f]).
step_down([2, 114/54, f], [2, 114/53, f]).
step_down([2, 114/61, f], [2, 114/60, f]).
step_down([2, 114/62, f], [2, 114/61, f]).
step_down([2, 114/63, f], [2, 114/62, f]).
step_down([2, 114/64, f], [2, 114/63, f]).
step_down([2, 114/65, f], [2, 114/64, f]).
step_down([2, 114/66, f], [2, 114/65, f]).
step_down([2, 114/73, f], [2, 114/72, f]).
step_down([2, 114/74, f], [2, 114/73, f]).
step_down([2, 114/77, f], [2, 114/76, f]).
step_down([2, 114/78, f], [2, 114/77, f]).
step_down([2, 114/79, f], [2, 114/78, f]).
step_down([2, 114/80, f], [2, 114/79, f]).
step_down([2, 114/83, f], [2, 114/82, f]).
step_down([2, 114/84, f], [2, 114/83, f]).
step_down([2, 114/87, f], [2, 114/86, f]).
step_down([2, 114/88, f], [2, 114/87, f]).
step_down([2, 114/91, f], [2, 114/90, f]).
step_down([2, 114/92, f], [2, 114/91, f]).
step_down([2, 114/95, f], [2, 114/94, f]).
step_down([2, 114/96, f], [2, 114/95, f]).
step_down([2, 114/97, f], [2, 114/96, f]).
step_down([2, 114/98, f], [2, 114/97, f]).
step_down([2, 114/101, f], [2, 114/100, f]).
step_down([2, 114/102, f], [2, 114/101, f]).
step_down([2, 114/105, f], [2, 114/104, f]).
step_down([2, 114/106, f], [2, 114/105, f]).
step_down([2, 114/107, f], [2, 114/106, f]).
step_down([2, 114/108, f], [2, 114/107, f]).
step_down([2, 114/111, f], [2, 114/110, f]).
step_down([2, 114/112, f], [2, 114/111, f]).
step_down([2, 114/117, f], [2, 114/116, f]).
step_down([2, 114/118, f], [2, 114/117, f]).
step_down([2, 116/1, f], [2, 116/0, f]).
step_down([2, 116/2, f], [2, 116/1, f]).
step_down([2, 116/7, f], [2, 116/6, f]).
step_down([2, 116/8, f], [2, 116/7, f]).
step_down([2, 116/9, f], [2, 116/8, f]).
step_down([2, 116/10, f], [2, 116/9, f]).
step_down([2, 116/13, f], [2, 116/12, f]).
step_down([2, 116/14, f], [2, 116/13, f]).
step_down([2, 116/15, f], [2, 116/14, f]).
step_down([2, 116/16, f], [2, 116/15, f]).
step_down([2, 116/21, f], [2, 116/20, f]).
step_down([2, 116/22, f], [2, 116/21, f]).
step_down([2, 116/25, f], [2, 116/24, f]).
step_down([2, 116/26, f], [2, 116/25, f]).
step_down([2, 116/29, f], [2, 116/28, f]).
step_down([2, 116/30, f], [2, 116/29, f]).
step_down([2, 116/35, f], [2, 116/34, f]).
step_down([2, 116/36, f], [2, 116/35, f]).
step_down([2, 116/37, f], [2, 116/36, f]).
step_down([2, 116/38, f], [2, 116/37, f]).
step_down([2, 116/47, f], [2, 116/46, f]).
step_down([2, 116/48, f], [2, 116/47, f]).
step_down([2, 116/51, f], [2, 116/50, f]).
step_down([2, 116/52, f], [2, 116/51, f]).
step_down([2, 116/53, f], [2, 116/52, f]).
step_down([2, 116/54, f], [2, 116/53, f]).
step_down([2, 116/57, f], [2, 116/56, f]).
step_down([2, 116/58, f], [2, 116/57, f]).
step_down([2, 116/59, f], [2, 116/58, f]).
step_down([2, 116/60, f], [2, 116/59, f]).
step_down([2, 116/63, f], [2, 116/62, f]).
step_down([2, 116/64, f], [2, 116/63, f]).
step_down([2, 116/65, f], [2, 116/64, f]).
step_down([2, 116/66, f], [2, 116/65, f]).
step_down([2, 116/67, f], [2, 116/66, f]).
step_down([2, 116/68, f], [2, 116/67, f]).
step_down([2, 116/73, f], [2, 116/72, f]).
step_down([2, 116/74, f], [2, 116/73, f]).
step_down([2, 116/77, f], [2, 116/76, f]).
step_down([2, 116/78, f], [2, 116/77, f]).
step_down([2, 116/83, f], [2, 116/82, f]).
step_down([2, 116/84, f], [2, 116/83, f]).
step_down([2, 116/85, f], [2, 116/84, f]).
step_down([2, 116/86, f], [2, 116/85, f]).
step_down([2, 116/89, f], [2, 116/88, f]).
step_down([2, 116/90, f], [2, 116/89, f]).
step_down([2, 116/93, f], [2, 116/92, f]).
step_down([2, 116/94, f], [2, 116/93, f]).
step_down([2, 116/97, f], [2, 116/96, f]).
step_down([2, 116/98, f], [2, 116/97, f]).
step_down([2, 116/99, f], [2, 116/98, f]).
step_down([2, 116/100, f], [2, 116/99, f]).
step_down([2, 116/103, f], [2, 116/102, f]).
step_down([2, 116/104, f], [2, 116/103, f]).
step_down([2, 116/105, f], [2, 116/104, f]).
step_down([2, 116/106, f], [2, 116/105, f]).
step_down([2, 116/107, f], [2, 116/106, f]).
step_down([2, 116/108, f], [2, 116/107, f]).
step_down([2, 116/109, f], [2, 116/108, f]).
step_down([2, 116/110, f], [2, 116/109, f]).
step_down([2, 116/115, f], [2, 116/114, f]).
step_down([2, 116/116, f], [2, 116/115, f]).
step_down([2, 118/1, f], [2, 118/0, e]).
step_down([2, 118/2, f], [2, 118/1, f]).
step_down([2, 118/5, f], [2, 118/4, f]).
step_down([2, 118/6, f], [2, 118/5, f]).
step_down([2, 118/7, f], [2, 118/6, f]).
step_down([2, 118/8, f], [2, 118/7, f]).
step_down([2, 118/9, f], [2, 118/8, f]).
step_down([2, 118/10, f], [2, 118/9, f]).
step_down([2, 118/11, f], [2, 118/10, f]).
step_down([2, 118/12, f], [2, 118/11, f]).
step_down([2, 118/13, f], [2, 118/12, f]).
step_down([2, 118/14, f], [2, 118/13, f]).
step_down([2, 118/17, f], [2, 118/16, f]).
step_down([2, 118/18, f], [2, 118/17, f]).
step_down([2, 118/19, f], [2, 118/18, f]).
step_down([2, 118/20, f], [2, 118/19, f]).
step_down([2, 118/23, f], [2, 118/22, f]).
step_down([2, 118/24, f], [2, 118/23, f]).
step_down([2, 118/25, f], [2, 118/24, f]).
step_down([2, 118/26, f], [2, 118/25, f]).
step_down([2, 118/27, f], [2, 118/26, f]).
step_down([2, 118/28, f], [2, 118/27, f]).
step_down([2, 118/29, f], [2, 118/28, f]).
step_down([2, 118/30, f], [2, 118/29, f]).
step_down([2, 118/33, f], [2, 118/32, f]).
step_down([2, 118/34, f], [2, 118/33, f]).
step_down([2, 118/37, f], [2, 118/36, f]).
step_down([2, 118/38, f], [2, 118/37, f]).
step_down([2, 118/39, f], [2, 118/38, f]).
step_down([2, 118/40, f], [2, 118/39, f]).
step_down([2, 118/43, f], [2, 118/42, f]).
step_down([2, 118/44, f], [2, 118/43, f]).
step_down([2, 118/47, f], [2, 118/46, f]).
step_down([2, 118/48, f], [2, 118/47, f]).
step_down([2, 118/49, f], [2, 118/48, f]).
step_down([2, 118/50, f], [2, 118/49, f]).
step_down([2, 118/51, f], [2, 118/50, f]).
step_down([2, 118/52, f], [2, 118/51, f]).
step_down([2, 118/55, f], [2, 118/54, f]).
step_down([2, 118/56, f], [2, 118/55, f]).
step_down([2, 118/57, f], [2, 118/56, f]).
step_down([2, 118/58, f], [2, 118/57, f]).
step_down([2, 118/59, f], [2, 118/58, f]).
step_down([2, 118/60, f], [2, 118/59, f]).
step_down([2, 118/61, f], [2, 118/60, f]).
step_down([2, 118/62, f], [2, 118/61, f]).
step_down([2, 118/65, f], [2, 118/64, f]).
step_down([2, 118/66, f], [2, 118/65, f]).
step_down([2, 118/67, f], [2, 118/66, f]).
step_down([2, 118/68, f], [2, 118/67, f]).
step_down([2, 118/69, f], [2, 118/68, f]).
step_down([2, 118/70, f], [2, 118/69, f]).
step_down([2, 118/71, f], [2, 118/70, f]).
step_down([2, 118/72, f], [2, 118/71, f]).
step_down([2, 118/73, f], [2, 118/72, f]).
step_down([2, 118/74, f], [2, 118/73, f]).
step_down([2, 118/77, f], [2, 118/76, f]).
step_down([2, 118/78, f], [2, 118/77, f]).
step_down([2, 118/81, f], [2, 118/80, f]).
step_down([2, 118/82, f], [2, 118/81, f]).
step_down([2, 118/83, f], [2, 118/82, f]).
step_down([2, 118/84, f], [2, 118/83, f]).
step_down([2, 118/85, f], [2, 118/84, f]).
step_down([2, 118/86, f], [2, 118/85, f]).
step_down([2, 118/87, f], [2, 118/86, f]).
step_down([2, 118/88, f], [2, 118/87, f]).
step_down([2, 118/91, f], [2, 118/90, f]).
step_down([2, 118/92, f], [2, 118/91, f]).
step_down([2, 118/93, f], [2, 118/92, f]).
step_down([2, 118/94, f], [2, 118/93, f]).
step_down([2, 118/95, f], [2, 118/94, f]).
step_down([2, 118/96, f], [2, 118/95, f]).
step_down([2, 118/97, f], [2, 118/96, f]).
step_down([2, 118/98, f], [2, 118/97, f]).
step_down([2, 118/99, f], [2, 118/98, f]).
step_down([2, 118/100, f], [2, 118/99, f]).
step_down([2, 118/101, f], [2, 118/100, f]).
step_down([2, 118/102, f], [2, 118/101, f]).
step_down([2, 118/103, f], [2, 118/102, f]).
step_down([2, 118/104, f], [2, 118/103, f]).
step_down([2, 118/105, f], [2, 118/104, f]).
step_down([2, 118/106, f], [2, 118/105, f]).
step_down([2, 118/107, f], [2, 118/106, f]).
step_down([2, 118/108, f], [2, 118/107, f]).
step_down([2, 118/109, f], [2, 118/108, f]).
step_down([2, 118/110, f], [2, 118/109, f]).
step_down([2, 118/111, f], [2, 118/110, f]).
step_down([2, 118/112, f], [2, 118/111, f]).
step_down([2, 118/113, f], [2, 118/112, f]).
step_down([2, 118/114, f], [2, 118/113, f]).
step_down([2, 118/115, f], [2, 118/114, f]).
step_down([2, 118/116, f], [2, 118/115, f]).
step_left([2, 1/0, f], [2, 0/0, f]).
step_left([2, 1/10, f], [2, 0/10, f]).
step_left([2, 1/12, f], [2, 0/12, f]).
step_left([2, 1/16, f], [2, 0/16, f]).
step_left([2, 1/20, f], [2, 0/20, f]).
step_left([2, 1/22, f], [2, 0/22, f]).
step_left([2, 1/24, f], [2, 0/24, f]).
step_left([2, 1/34, f], [2, 0/34, f]).
step_left([2, 1/36, f], [2, 0/36, f]).
step_left([2, 1/38, f], [2, 0/38, f]).
step_left([2, 1/40, f], [2, 0/40, f]).
step_left([2, 1/44, f], [2, 0/44, f]).
step_left([2, 1/48, f], [2, 0/48, f]).
step_left([2, 1/50, f], [2, 0/50, f]).
step_left([2, 1/56, f], [2, 0/56, f]).
step_left([2, 1/58, f], [2, 0/58, f]).
step_left([2, 1/64, f], [2, 0/64, f]).
step_left([2, 1/72, f], [2, 0/72, f]).
step_left([2, 1/76, f], [2, 0/76, f]).
step_left([2, 1/80, f], [2, 0/80, f]).
step_left([2, 1/82, f], [2, 0/82, f]).
step_left([2, 1/84, f], [2, 0/84, f]).
step_left([2, 1/94, f], [2, 0/94, f]).
step_left([2, 1/98, f], [2, 0/98, f]).
step_left([2, 1/100, f], [2, 0/100, f]).
step_left([2, 1/102, f], [2, 0/102, f]).
step_left([2, 1/106, f], [2, 0/106, f]).
step_left([2, 1/112, f], [2, 0/112, f]).
step_left([2, 2/0, f], [2, 1/0, f]).
step_left([2, 2/10, f], [2, 1/10, f]).
step_left([2, 2/12, f], [2, 1/12, f]).
step_left([2, 2/16, f], [2, 1/16, f]).
step_left([2, 2/20, f], [2, 1/20, f]).
step_left([2, 2/22, f], [2, 1/22, f]).
step_left([2, 2/24, f], [2, 1/24, f]).
step_left([2, 2/34, f], [2, 1/34, f]).
step_left([2, 2/36, f], [2, 1/36, f]).
step_left([2, 2/38, f], [2, 1/38, f]).
step_left([2, 2/40, f], [2, 1/40, f]).
step_left([2, 2/44, f], [2, 1/44, f]).
step_left([2, 2/48, f], [2, 1/48, f]).
step_left([2, 2/50, f], [2, 1/50, f]).
step_left([2, 2/56, f], [2, 1/56, f]).
step_left([2, 2/58, f], [2, 1/58, f]).
step_left([2, 2/64, f], [2, 1/64, f]).
step_left([2, 2/72, f], [2, 1/72, f]).
step_left([2, 2/76, f], [2, 1/76, f]).
step_left([2, 2/80, f], [2, 1/80, f]).
step_left([2, 2/82, f], [2, 1/82, f]).
step_left([2, 2/84, f], [2, 1/84, f]).
step_left([2, 2/94, f], [2, 1/94, f]).
step_left([2, 2/98, f], [2, 1/98, f]).
step_left([2, 2/100, f], [2, 1/100, f]).
step_left([2, 2/102, f], [2, 1/102, f]).
step_left([2, 2/106, f], [2, 1/106, f]).
step_left([2, 2/112, f], [2, 1/112, f]).
step_left([2, 3/4, f], [2, 2/4, f]).
step_left([2, 3/6, f], [2, 2/6, f]).
step_left([2, 3/12, f], [2, 2/12, f]).
step_left([2, 3/14, f], [2, 2/14, f]).
step_left([2, 3/22, f], [2, 2/22, f]).
step_left([2, 3/28, f], [2, 2/28, f]).
step_left([2, 3/30, f], [2, 2/30, f]).
step_left([2, 3/32, f], [2, 2/32, f]).
step_left([2, 3/46, f], [2, 2/46, f]).
step_left([2, 3/48, f], [2, 2/48, f]).
step_left([2, 3/50, f], [2, 2/50, f]).
step_left([2, 3/52, f], [2, 2/52, f]).
step_left([2, 3/58, f], [2, 2/58, f]).
step_left([2, 3/60, f], [2, 2/60, f]).
step_left([2, 3/62, f], [2, 2/62, f]).
step_left([2, 3/64, f], [2, 2/64, f]).
step_left([2, 3/66, f], [2, 2/66, f]).
step_left([2, 3/74, f], [2, 2/74, f]).
step_left([2, 3/80, f], [2, 2/80, f]).
step_left([2, 3/82, f], [2, 2/82, f]).
step_left([2, 3/88, f], [2, 2/88, f]).
step_left([2, 3/90, f], [2, 2/90, f]).
step_left([2, 3/96, f], [2, 2/96, f]).
step_left([2, 3/100, f], [2, 2/100, f]).
step_left([2, 3/102, f], [2, 2/102, f]).
step_left([2, 3/104, f], [2, 2/104, f]).
step_left([2, 3/108, f], [2, 2/108, f]).
step_left([2, 3/116, f], [2, 2/116, f]).
step_left([2, 3/118, f], [2, 2/118, f]).
step_left([2, 4/4, f], [2, 3/4, f]).
step_left([2, 4/6, f], [2, 3/6, f]).
step_left([2, 4/12, f], [2, 3/12, f]).
step_left([2, 4/14, f], [2, 3/14, f]).
step_left([2, 4/22, f], [2, 3/22, f]).
step_left([2, 4/28, f], [2, 3/28, f]).
step_left([2, 4/30, f], [2, 3/30, f]).
step_left([2, 4/32, f], [2, 3/32, f]).
step_left([2, 4/46, f], [2, 3/46, f]).
step_left([2, 4/48, f], [2, 3/48, f]).
step_left([2, 4/50, f], [2, 3/50, f]).
step_left([2, 4/52, f], [2, 3/52, f]).
step_left([2, 4/58, f], [2, 3/58, f]).
step_left([2, 4/60, f], [2, 3/60, f]).
step_left([2, 4/62, f], [2, 3/62, f]).
step_left([2, 4/64, f], [2, 3/64, f]).
step_left([2, 4/66, f], [2, 3/66, f]).
step_left([2, 4/74, f], [2, 3/74, f]).
step_left([2, 4/80, f], [2, 3/80, f]).
step_left([2, 4/82, f], [2, 3/82, f]).
step_left([2, 4/88, f], [2, 3/88, f]).
step_left([2, 4/90, f], [2, 3/90, f]).
step_left([2, 4/96, f], [2, 3/96, f]).
step_left([2, 4/100, f], [2, 3/100, f]).
step_left([2, 4/102, f], [2, 3/102, f]).
step_left([2, 4/104, f], [2, 3/104, f]).
step_left([2, 4/108, f], [2, 3/108, f]).
step_left([2, 4/116, f], [2, 3/116, f]).
step_left([2, 4/118, f], [2, 3/118, f]).
step_left([2, 5/0, f], [2, 4/0, f]).
step_left([2, 5/2, f], [2, 4/2, f]).
step_left([2, 5/6, f], [2, 4/6, f]).
step_left([2, 5/8, f], [2, 4/8, f]).
step_left([2, 5/10, f], [2, 4/10, f]).
step_left([2, 5/12, f], [2, 4/12, f]).
step_left([2, 5/14, f], [2, 4/14, f]).
step_left([2, 5/18, f], [2, 4/18, f]).
step_left([2, 5/20, f], [2, 4/20, f]).
step_left([2, 5/26, f], [2, 4/26, f]).
step_left([2, 5/28, f], [2, 4/28, f]).
step_left([2, 5/30, f], [2, 4/30, f]).
step_left([2, 5/38, f], [2, 4/38, f]).
step_left([2, 5/44, f], [2, 4/44, f]).
step_left([2, 5/46, f], [2, 4/46, f]).
step_left([2, 5/52, f], [2, 4/52, f]).
step_left([2, 5/56, f], [2, 4/56, f]).
step_left([2, 5/58, f], [2, 4/58, f]).
step_left([2, 5/60, f], [2, 4/60, f]).
step_left([2, 5/62, f], [2, 4/62, f]).
step_left([2, 5/66, f], [2, 4/66, f]).
step_left([2, 5/68, f], [2, 4/68, f]).
step_left([2, 5/76, f], [2, 4/76, f]).
step_left([2, 5/78, f], [2, 4/78, f]).
step_left([2, 5/80, f], [2, 4/80, f]).
step_left([2, 5/82, f], [2, 4/82, f]).
step_left([2, 5/84, f], [2, 4/84, f]).
step_left([2, 5/86, f], [2, 4/86, f]).
step_left([2, 5/88, f], [2, 4/88, f]).
step_left([2, 5/92, f], [2, 4/92, f]).
step_left([2, 5/94, f], [2, 4/94, f]).
step_left([2, 5/102, f], [2, 4/102, f]).
step_left([2, 5/106, f], [2, 4/106, f]).
step_left([2, 5/110, f], [2, 4/110, f]).
step_left([2, 5/112, f], [2, 4/112, f]).
step_left([2, 5/114, f], [2, 4/114, f]).
step_left([2, 5/118, f], [2, 4/118, f]).
step_left([2, 6/0, f], [2, 5/0, f]).
step_left([2, 6/2, f], [2, 5/2, f]).
step_left([2, 6/6, f], [2, 5/6, f]).
step_left([2, 6/8, f], [2, 5/8, f]).
step_left([2, 6/10, f], [2, 5/10, f]).
step_left([2, 6/12, f], [2, 5/12, f]).
step_left([2, 6/14, f], [2, 5/14, f]).
step_left([2, 6/18, f], [2, 5/18, f]).
step_left([2, 6/20, f], [2, 5/20, f]).
step_left([2, 6/26, f], [2, 5/26, f]).
step_left([2, 6/28, f], [2, 5/28, f]).
step_left([2, 6/30, f], [2, 5/30, f]).
step_left([2, 6/38, f], [2, 5/38, f]).
step_left([2, 6/44, f], [2, 5/44, f]).
step_left([2, 6/46, f], [2, 5/46, f]).
step_left([2, 6/52, f], [2, 5/52, f]).
step_left([2, 6/56, f], [2, 5/56, f]).
step_left([2, 6/58, f], [2, 5/58, f]).
step_left([2, 6/60, f], [2, 5/60, f]).
step_left([2, 6/62, f], [2, 5/62, f]).
step_left([2, 6/66, f], [2, 5/66, f]).
step_left([2, 6/68, f], [2, 5/68, f]).
step_left([2, 6/76, f], [2, 5/76, f]).
step_left([2, 6/78, f], [2, 5/78, f]).
step_left([2, 6/80, f], [2, 5/80, f]).
step_left([2, 6/82, f], [2, 5/82, f]).
step_left([2, 6/84, f], [2, 5/84, f]).
step_left([2, 6/86, f], [2, 5/86, f]).
step_left([2, 6/88, f], [2, 5/88, f]).
step_left([2, 6/92, f], [2, 5/92, f]).
step_left([2, 6/94, f], [2, 5/94, f]).
step_left([2, 6/102, f], [2, 5/102, f]).
step_left([2, 6/106, f], [2, 5/106, f]).
step_left([2, 6/110, f], [2, 5/110, f]).
step_left([2, 6/112, f], [2, 5/112, f]).
step_left([2, 6/114, f], [2, 5/114, f]).
step_left([2, 6/118, f], [2, 5/118, f]).
step_left([2, 7/0, f], [2, 6/0, f]).
step_left([2, 7/4, f], [2, 6/4, f]).
step_left([2, 7/14, f], [2, 6/14, f]).
step_left([2, 7/16, f], [2, 6/16, f]).
step_left([2, 7/20, f], [2, 6/20, f]).
step_left([2, 7/30, f], [2, 6/30, f]).
step_left([2, 7/32, f], [2, 6/32, f]).
step_left([2, 7/36, f], [2, 6/36, f]).
step_left([2, 7/40, f], [2, 6/40, f]).
step_left([2, 7/42, f], [2, 6/42, f]).
step_left([2, 7/46, f], [2, 6/46, f]).
step_left([2, 7/48, f], [2, 6/48, f]).
step_left([2, 7/54, f], [2, 6/54, f]).
step_left([2, 7/58, f], [2, 6/58, f]).
step_left([2, 7/78, f], [2, 6/78, f]).
step_left([2, 7/82, f], [2, 6/82, f]).
step_left([2, 7/84, f], [2, 6/84, f]).
step_left([2, 7/86, f], [2, 6/86, f]).
step_left([2, 7/88, f], [2, 6/88, f]).
step_left([2, 7/90, f], [2, 6/90, f]).
step_left([2, 7/104, f], [2, 6/104, f]).
step_left([2, 7/106, f], [2, 6/106, f]).
step_left([2, 7/108, f], [2, 6/108, f]).
step_left([2, 7/112, f], [2, 6/112, f]).
step_left([2, 7/118, f], [2, 6/118, f]).
step_left([2, 8/0, f], [2, 7/0, f]).
step_left([2, 8/4, f], [2, 7/4, f]).
step_left([2, 8/14, f], [2, 7/14, f]).
step_left([2, 8/16, f], [2, 7/16, f]).
step_left([2, 8/20, f], [2, 7/20, f]).
step_left([2, 8/30, f], [2, 7/30, f]).
step_left([2, 8/32, f], [2, 7/32, f]).
step_left([2, 8/36, f], [2, 7/36, f]).
step_left([2, 8/40, f], [2, 7/40, f]).
step_left([2, 8/42, f], [2, 7/42, f]).
step_left([2, 8/46, f], [2, 7/46, f]).
step_left([2, 8/48, f], [2, 7/48, f]).
step_left([2, 8/54, f], [2, 7/54, f]).
step_left([2, 8/58, f], [2, 7/58, f]).
step_left([2, 8/78, f], [2, 7/78, f]).
step_left([2, 8/82, f], [2, 7/82, f]).
step_left([2, 8/84, f], [2, 7/84, f]).
step_left([2, 8/86, f], [2, 7/86, f]).
step_left([2, 8/88, f], [2, 7/88, f]).
step_left([2, 8/90, f], [2, 7/90, f]).
step_left([2, 8/104, f], [2, 7/104, f]).
step_left([2, 8/106, f], [2, 7/106, f]).
step_left([2, 8/108, f], [2, 7/108, f]).
step_left([2, 8/112, f], [2, 7/112, f]).
step_left([2, 8/118, f], [2, 7/118, f]).
step_left([2, 9/0, f], [2, 8/0, f]).
step_left([2, 9/2, f], [2, 8/2, f]).
step_left([2, 9/6, f], [2, 8/6, f]).
step_left([2, 9/8, f], [2, 8/8, f]).
step_left([2, 9/16, f], [2, 8/16, f]).
step_left([2, 9/18, f], [2, 8/18, f]).
step_left([2, 9/22, f], [2, 8/22, f]).
step_left([2, 9/24, f], [2, 8/24, f]).
step_left([2, 9/30, f], [2, 8/30, f]).
step_left([2, 9/34, f], [2, 8/34, f]).
step_left([2, 9/38, f], [2, 8/38, f]).
step_left([2, 9/44, f], [2, 8/44, f]).
step_left([2, 9/46, f], [2, 8/46, f]).
step_left([2, 9/52, f], [2, 8/52, f]).
step_left([2, 9/56, f], [2, 8/56, f]).
step_left([2, 9/58, f], [2, 8/58, f]).
step_left([2, 9/60, f], [2, 8/60, f]).
step_left([2, 9/62, f], [2, 8/62, f]).
step_left([2, 9/64, f], [2, 8/64, f]).
step_left([2, 9/76, f], [2, 8/76, f]).
step_left([2, 9/78, f], [2, 8/78, f]).
step_left([2, 9/84, f], [2, 8/84, f]).
step_left([2, 9/86, f], [2, 8/86, f]).
step_left([2, 9/88, f], [2, 8/88, f]).
step_left([2, 9/94, f], [2, 8/94, f]).
step_left([2, 9/98, f], [2, 8/98, f]).
step_left([2, 9/100, f], [2, 8/100, f]).
step_left([2, 9/102, f], [2, 8/102, f]).
step_left([2, 9/108, f], [2, 8/108, f]).
step_left([2, 9/112, f], [2, 8/112, f]).
step_left([2, 9/114, f], [2, 8/114, f]).
step_left([2, 10/0, f], [2, 9/0, f]).
step_left([2, 10/2, f], [2, 9/2, f]).
step_left([2, 10/6, f], [2, 9/6, f]).
step_left([2, 10/8, f], [2, 9/8, f]).
step_left([2, 10/16, f], [2, 9/16, f]).
step_left([2, 10/18, f], [2, 9/18, f]).
step_left([2, 10/22, f], [2, 9/22, f]).
step_left([2, 10/24, f], [2, 9/24, f]).
step_left([2, 10/30, f], [2, 9/30, f]).
step_left([2, 10/34, f], [2, 9/34, f]).
step_left([2, 10/38, f], [2, 9/38, f]).
step_left([2, 10/44, f], [2, 9/44, f]).
step_left([2, 10/46, f], [2, 9/46, f]).
step_left([2, 10/52, f], [2, 9/52, f]).
step_left([2, 10/56, f], [2, 9/56, f]).
step_left([2, 10/58, f], [2, 9/58, f]).
step_left([2, 10/60, f], [2, 9/60, f]).
step_left([2, 10/62, f], [2, 9/62, f]).
step_left([2, 10/64, f], [2, 9/64, f]).
step_left([2, 10/76, f], [2, 9/76, f]).
step_left([2, 10/78, f], [2, 9/78, f]).
step_left([2, 10/84, f], [2, 9/84, f]).
step_left([2, 10/86, f], [2, 9/86, f]).
step_left([2, 10/88, f], [2, 9/88, f]).
step_left([2, 10/94, f], [2, 9/94, f]).
step_left([2, 10/98, f], [2, 9/98, f]).
step_left([2, 10/100, f], [2, 9/100, f]).
step_left([2, 10/102, f], [2, 9/102, f]).
step_left([2, 10/108, f], [2, 9/108, f]).
step_left([2, 10/112, f], [2, 9/112, f]).
step_left([2, 10/114, f], [2, 9/114, f]).
step_left([2, 11/0, f], [2, 10/0, f]).
step_left([2, 11/6, f], [2, 10/6, f]).
step_left([2, 11/8, f], [2, 10/8, f]).
step_left([2, 11/10, f], [2, 10/10, f]).
step_left([2, 11/12, f], [2, 10/12, f]).
step_left([2, 11/14, f], [2, 10/14, f]).
step_left([2, 11/18, f], [2, 10/18, f]).
step_left([2, 11/20, f], [2, 10/20, f]).
step_left([2, 11/26, f], [2, 10/26, f]).
step_left([2, 11/28, f], [2, 10/28, f]).
step_left([2, 11/34, f], [2, 10/34, f]).
step_left([2, 11/38, f], [2, 10/38, f]).
step_left([2, 11/40, f], [2, 10/40, f]).
step_left([2, 11/48, f], [2, 10/48, f]).
step_left([2, 11/54, f], [2, 10/54, f]).
step_left([2, 11/56, f], [2, 10/56, f]).
step_left([2, 11/62, f], [2, 10/62, f]).
step_left([2, 11/66, f], [2, 10/66, f]).
step_left([2, 11/74, f], [2, 10/74, f]).
step_left([2, 11/82, f], [2, 10/82, f]).
step_left([2, 11/84, f], [2, 10/84, f]).
step_left([2, 11/90, f], [2, 10/90, f]).
step_left([2, 11/92, f], [2, 10/92, f]).
step_left([2, 11/96, f], [2, 10/96, f]).
step_left([2, 11/100, f], [2, 10/100, f]).
step_left([2, 11/104, f], [2, 10/104, f]).
step_left([2, 11/106, f], [2, 10/106, f]).
step_left([2, 11/110, f], [2, 10/110, f]).
step_left([2, 11/118, f], [2, 10/118, f]).
step_left([2, 12/0, f], [2, 11/0, f]).
step_left([2, 12/6, f], [2, 11/6, f]).
step_left([2, 12/8, f], [2, 11/8, f]).
step_left([2, 12/10, f], [2, 11/10, f]).
step_left([2, 12/12, f], [2, 11/12, f]).
step_left([2, 12/14, f], [2, 11/14, f]).
step_left([2, 12/18, f], [2, 11/18, f]).
step_left([2, 12/20, f], [2, 11/20, f]).
step_left([2, 12/26, f], [2, 11/26, f]).
step_left([2, 12/28, f], [2, 11/28, f]).
step_left([2, 12/34, f], [2, 11/34, f]).
step_left([2, 12/38, f], [2, 11/38, f]).
step_left([2, 12/40, f], [2, 11/40, f]).
step_left([2, 12/48, f], [2, 11/48, f]).
step_left([2, 12/54, f], [2, 11/54, f]).
step_left([2, 12/56, f], [2, 11/56, f]).
step_left([2, 12/62, f], [2, 11/62, f]).
step_left([2, 12/66, f], [2, 11/66, f]).
step_left([2, 12/74, f], [2, 11/74, f]).
step_left([2, 12/82, f], [2, 11/82, f]).
step_left([2, 12/84, f], [2, 11/84, f]).
step_left([2, 12/90, f], [2, 11/90, f]).
step_left([2, 12/92, f], [2, 11/92, f]).
step_left([2, 12/96, f], [2, 11/96, f]).
step_left([2, 12/100, f], [2, 11/100, f]).
step_left([2, 12/104, f], [2, 11/104, f]).
step_left([2, 12/106, f], [2, 11/106, f]).
step_left([2, 12/110, f], [2, 11/110, f]).
step_left([2, 12/118, f], [2, 11/118, f]).
step_left([2, 13/0, f], [2, 12/0, f]).
step_left([2, 13/2, f], [2, 12/2, f]).
step_left([2, 13/12, f], [2, 12/12, f]).
step_left([2, 13/16, f], [2, 12/16, f]).
step_left([2, 13/18, f], [2, 12/18, f]).
step_left([2, 13/22, f], [2, 12/22, f]).
step_left([2, 13/24, f], [2, 12/24, f]).
step_left([2, 13/26, f], [2, 12/26, f]).
step_left([2, 13/28, f], [2, 12/28, f]).
step_left([2, 13/30, f], [2, 12/30, f]).
step_left([2, 13/32, f], [2, 12/32, f]).
step_left([2, 13/36, f], [2, 12/36, f]).
step_left([2, 13/42, f], [2, 12/42, f]).
step_left([2, 13/44, f], [2, 12/44, f]).
step_left([2, 13/50, f], [2, 12/50, f]).
step_left([2, 13/56, f], [2, 12/56, f]).
step_left([2, 13/58, f], [2, 12/58, f]).
step_left([2, 13/60, f], [2, 12/60, f]).
step_left([2, 13/64, f], [2, 12/64, f]).
step_left([2, 13/68, f], [2, 12/68, f]).
step_left([2, 13/72, f], [2, 12/72, f]).
step_left([2, 13/76, f], [2, 12/76, f]).
step_left([2, 13/78, f], [2, 12/78, f]).
step_left([2, 13/82, f], [2, 12/82, f]).
step_left([2, 13/84, f], [2, 12/84, f]).
step_left([2, 13/88, f], [2, 12/88, f]).
step_left([2, 13/90, f], [2, 12/90, f]).
step_left([2, 13/94, f], [2, 12/94, f]).
step_left([2, 13/96, f], [2, 12/96, f]).
step_left([2, 13/98, f], [2, 12/98, f]).
step_left([2, 13/102, f], [2, 12/102, f]).
step_left([2, 13/104, f], [2, 12/104, f]).
step_left([2, 13/106, f], [2, 12/106, f]).
step_left([2, 13/108, f], [2, 12/108, f]).
step_left([2, 13/112, f], [2, 12/112, f]).
step_left([2, 13/114, f], [2, 12/114, f]).
step_left([2, 13/116, f], [2, 12/116, f]).
step_left([2, 13/118, f], [2, 12/118, f]).
step_left([2, 14/0, f], [2, 13/0, f]).
step_left([2, 14/2, f], [2, 13/2, f]).
step_left([2, 14/12, f], [2, 13/12, f]).
step_left([2, 14/16, f], [2, 13/16, f]).
step_left([2, 14/18, f], [2, 13/18, f]).
step_left([2, 14/22, f], [2, 13/22, f]).
step_left([2, 14/24, f], [2, 13/24, f]).
step_left([2, 14/26, f], [2, 13/26, f]).
step_left([2, 14/28, f], [2, 13/28, f]).
step_left([2, 14/30, f], [2, 13/30, f]).
step_left([2, 14/32, f], [2, 13/32, f]).
step_left([2, 14/36, f], [2, 13/36, f]).
step_left([2, 14/42, f], [2, 13/42, f]).
step_left([2, 14/44, f], [2, 13/44, f]).
step_left([2, 14/50, f], [2, 13/50, f]).
step_left([2, 14/56, f], [2, 13/56, f]).
step_left([2, 14/58, f], [2, 13/58, f]).
step_left([2, 14/60, f], [2, 13/60, f]).
step_left([2, 14/64, f], [2, 13/64, f]).
step_left([2, 14/68, f], [2, 13/68, f]).
step_left([2, 14/72, f], [2, 13/72, f]).
step_left([2, 14/76, f], [2, 13/76, f]).
step_left([2, 14/78, f], [2, 13/78, f]).
step_left([2, 14/82, f], [2, 13/82, f]).
step_left([2, 14/84, f], [2, 13/84, f]).
step_left([2, 14/88, f], [2, 13/88, f]).
step_left([2, 14/90, f], [2, 13/90, f]).
step_left([2, 14/94, f], [2, 13/94, f]).
step_left([2, 14/96, f], [2, 13/96, f]).
step_left([2, 14/98, f], [2, 13/98, f]).
step_left([2, 14/102, f], [2, 13/102, f]).
step_left([2, 14/104, f], [2, 13/104, f]).
step_left([2, 14/106, f], [2, 13/106, f]).
step_left([2, 14/108, f], [2, 13/108, f]).
step_left([2, 14/112, f], [2, 13/112, f]).
step_left([2, 14/114, f], [2, 13/114, f]).
step_left([2, 14/116, f], [2, 13/116, f]).
step_left([2, 14/118, f], [2, 13/118, f]).
step_left([2, 15/0, f], [2, 14/0, f]).
step_left([2, 15/2, f], [2, 14/2, f]).
step_left([2, 15/6, f], [2, 14/6, f]).
step_left([2, 15/14, f], [2, 14/14, f]).
step_left([2, 15/16, f], [2, 14/16, f]).
step_left([2, 15/18, f], [2, 14/18, f]).
step_left([2, 15/20, f], [2, 14/20, f]).
step_left([2, 15/24, f], [2, 14/24, f]).
step_left([2, 15/26, f], [2, 14/26, f]).
step_left([2, 15/30, f], [2, 14/30, f]).
step_left([2, 15/32, f], [2, 14/32, f]).
step_left([2, 15/34, f], [2, 14/34, f]).
step_left([2, 15/36, f], [2, 14/36, f]).
step_left([2, 15/38, f], [2, 14/38, f]).
step_left([2, 15/40, f], [2, 14/40, f]).
step_left([2, 15/44, f], [2, 14/44, f]).
step_left([2, 15/46, f], [2, 14/46, f]).
step_left([2, 15/48, f], [2, 14/48, f]).
step_left([2, 15/50, f], [2, 14/50, f]).
step_left([2, 15/52, f], [2, 14/52, f]).
step_left([2, 15/58, f], [2, 14/58, f]).
step_left([2, 15/64, f], [2, 14/64, f]).
step_left([2, 15/66, f], [2, 14/66, f]).
step_left([2, 15/78, f], [2, 14/78, f]).
step_left([2, 15/80, f], [2, 14/80, f]).
step_left([2, 15/82, f], [2, 14/82, f]).
step_left([2, 15/86, f], [2, 14/86, f]).
step_left([2, 15/88, f], [2, 14/88, f]).
step_left([2, 15/92, f], [2, 14/92, f]).
step_left([2, 15/98, f], [2, 14/98, f]).
step_left([2, 15/102, f], [2, 14/102, f]).
step_left([2, 15/110, f], [2, 14/110, f]).
step_left([2, 15/112, f], [2, 14/112, f]).
step_left([2, 15/114, f], [2, 14/114, f]).
step_left([2, 15/118, f], [2, 14/118, f]).
step_left([2, 16/0, f], [2, 15/0, f]).
step_left([2, 16/2, f], [2, 15/2, f]).
step_left([2, 16/6, f], [2, 15/6, f]).
step_left([2, 16/14, f], [2, 15/14, f]).
step_left([2, 16/16, f], [2, 15/16, f]).
step_left([2, 16/18, f], [2, 15/18, f]).
step_left([2, 16/20, f], [2, 15/20, f]).
step_left([2, 16/24, f], [2, 15/24, f]).
step_left([2, 16/26, f], [2, 15/26, f]).
step_left([2, 16/30, f], [2, 15/30, f]).
step_left([2, 16/32, f], [2, 15/32, f]).
step_left([2, 16/34, f], [2, 15/34, f]).
step_left([2, 16/36, f], [2, 15/36, f]).
step_left([2, 16/38, f], [2, 15/38, f]).
step_left([2, 16/40, f], [2, 15/40, f]).
step_left([2, 16/44, f], [2, 15/44, f]).
step_left([2, 16/46, f], [2, 15/46, f]).
step_left([2, 16/48, f], [2, 15/48, f]).
step_left([2, 16/50, f], [2, 15/50, f]).
step_left([2, 16/52, f], [2, 15/52, f]).
step_left([2, 16/58, f], [2, 15/58, f]).
step_left([2, 16/64, f], [2, 15/64, f]).
step_left([2, 16/66, f], [2, 15/66, f]).
step_left([2, 16/78, f], [2, 15/78, f]).
step_left([2, 16/80, f], [2, 15/80, f]).
step_left([2, 16/82, f], [2, 15/82, f]).
step_left([2, 16/86, f], [2, 15/86, f]).
step_left([2, 16/88, f], [2, 15/88, f]).
step_left([2, 16/92, f], [2, 15/92, f]).
step_left([2, 16/98, f], [2, 15/98, f]).
step_left([2, 16/102, f], [2, 15/102, f]).
step_left([2, 16/110, f], [2, 15/110, f]).
step_left([2, 16/112, f], [2, 15/112, f]).
step_left([2, 16/114, f], [2, 15/114, f]).
step_left([2, 16/118, f], [2, 15/118, f]).
step_left([2, 17/0, f], [2, 16/0, f]).
step_left([2, 17/4, f], [2, 16/4, f]).
step_left([2, 17/10, f], [2, 16/10, f]).
step_left([2, 17/18, f], [2, 16/18, f]).
step_left([2, 17/20, f], [2, 16/20, f]).
step_left([2, 17/22, f], [2, 16/22, f]).
step_left([2, 17/24, f], [2, 16/24, f]).
step_left([2, 17/26, f], [2, 16/26, f]).
step_left([2, 17/28, f], [2, 16/28, f]).
step_left([2, 17/32, f], [2, 16/32, f]).
step_left([2, 17/38, f], [2, 16/38, f]).
step_left([2, 17/42, f], [2, 16/42, f]).
step_left([2, 17/48, f], [2, 16/48, f]).
step_left([2, 17/52, f], [2, 16/52, f]).
step_left([2, 17/54, f], [2, 16/54, f]).
step_left([2, 17/58, f], [2, 16/58, f]).
step_left([2, 17/60, f], [2, 16/60, f]).
step_left([2, 17/66, f], [2, 16/66, f]).
step_left([2, 17/68, f], [2, 16/68, f]).
step_left([2, 17/78, f], [2, 16/78, f]).
step_left([2, 17/90, f], [2, 16/90, f]).
step_left([2, 17/96, f], [2, 16/96, f]).
step_left([2, 17/98, f], [2, 16/98, f]).
step_left([2, 17/100, f], [2, 16/100, f]).
step_left([2, 17/104, f], [2, 16/104, f]).
step_left([2, 18/0, f], [2, 17/0, f]).
step_left([2, 18/4, f], [2, 17/4, f]).
step_left([2, 18/10, f], [2, 17/10, f]).
step_left([2, 18/18, f], [2, 17/18, f]).
step_left([2, 18/20, f], [2, 17/20, f]).
step_left([2, 18/22, f], [2, 17/22, f]).
step_left([2, 18/24, f], [2, 17/24, f]).
step_left([2, 18/26, f], [2, 17/26, f]).
step_left([2, 18/28, f], [2, 17/28, f]).
step_left([2, 18/32, f], [2, 17/32, f]).
step_left([2, 18/38, f], [2, 17/38, f]).
step_left([2, 18/42, f], [2, 17/42, f]).
step_left([2, 18/48, f], [2, 17/48, f]).
step_left([2, 18/52, f], [2, 17/52, f]).
step_left([2, 18/54, f], [2, 17/54, f]).
step_left([2, 18/58, f], [2, 17/58, f]).
step_left([2, 18/60, f], [2, 17/60, f]).
step_left([2, 18/66, f], [2, 17/66, f]).
step_left([2, 18/68, f], [2, 17/68, f]).
step_left([2, 18/78, f], [2, 17/78, f]).
step_left([2, 18/90, f], [2, 17/90, f]).
step_left([2, 18/96, f], [2, 17/96, f]).
step_left([2, 18/98, f], [2, 17/98, f]).
step_left([2, 18/100, f], [2, 17/100, f]).
step_left([2, 18/104, f], [2, 17/104, f]).
step_left([2, 19/2, f], [2, 18/2, f]).
step_left([2, 19/4, f], [2, 18/4, f]).
step_left([2, 19/8, f], [2, 18/8, f]).
step_left([2, 19/12, f], [2, 18/12, f]).
step_left([2, 19/14, f], [2, 18/14, f]).
step_left([2, 19/16, f], [2, 18/16, f]).
step_left([2, 19/18, f], [2, 18/18, f]).
step_left([2, 19/24, f], [2, 18/24, f]).
step_left([2, 19/30, f], [2, 18/30, f]).
step_left([2, 19/34, f], [2, 18/34, f]).
step_left([2, 19/36, f], [2, 18/36, f]).
step_left([2, 19/40, f], [2, 18/40, f]).
step_left([2, 19/44, f], [2, 18/44, f]).
step_left([2, 19/50, f], [2, 18/50, f]).
step_left([2, 19/54, f], [2, 18/54, f]).
step_left([2, 19/56, f], [2, 18/56, f]).
step_left([2, 19/60, f], [2, 18/60, f]).
step_left([2, 19/62, f], [2, 18/62, f]).
step_left([2, 19/66, f], [2, 18/66, f]).
step_left([2, 19/68, f], [2, 18/68, f]).
step_left([2, 19/70, f], [2, 18/70, f]).
step_left([2, 19/72, f], [2, 18/72, f]).
step_left([2, 19/80, f], [2, 18/80, f]).
step_left([2, 19/84, f], [2, 18/84, f]).
step_left([2, 19/86, f], [2, 18/86, f]).
step_left([2, 19/88, f], [2, 18/88, f]).
step_left([2, 19/92, f], [2, 18/92, f]).
step_left([2, 19/94, f], [2, 18/94, f]).
step_left([2, 19/98, f], [2, 18/98, f]).
step_left([2, 19/102, f], [2, 18/102, f]).
step_left([2, 19/110, f], [2, 18/110, f]).
step_left([2, 19/118, f], [2, 18/118, f]).
step_left([2, 20/2, f], [2, 19/2, f]).
step_left([2, 20/4, f], [2, 19/4, f]).
step_left([2, 20/8, f], [2, 19/8, f]).
step_left([2, 20/12, f], [2, 19/12, f]).
step_left([2, 20/14, f], [2, 19/14, f]).
step_left([2, 20/16, f], [2, 19/16, f]).
step_left([2, 20/18, f], [2, 19/18, f]).
step_left([2, 20/24, f], [2, 19/24, f]).
step_left([2, 20/30, f], [2, 19/30, f]).
step_left([2, 20/34, f], [2, 19/34, f]).
step_left([2, 20/36, f], [2, 19/36, f]).
step_left([2, 20/40, f], [2, 19/40, f]).
step_left([2, 20/44, f], [2, 19/44, f]).
step_left([2, 20/50, f], [2, 19/50, f]).
step_left([2, 20/54, f], [2, 19/54, f]).
step_left([2, 20/56, f], [2, 19/56, f]).
step_left([2, 20/60, f], [2, 19/60, f]).
step_left([2, 20/62, f], [2, 19/62, f]).
step_left([2, 20/66, f], [2, 19/66, f]).
step_left([2, 20/68, f], [2, 19/68, f]).
step_left([2, 20/70, f], [2, 19/70, f]).
step_left([2, 20/72, f], [2, 19/72, f]).
step_left([2, 20/80, f], [2, 19/80, f]).
step_left([2, 20/84, f], [2, 19/84, f]).
step_left([2, 20/86, f], [2, 19/86, f]).
step_left([2, 20/88, f], [2, 19/88, f]).
step_left([2, 20/92, f], [2, 19/92, f]).
step_left([2, 20/94, f], [2, 19/94, f]).
step_left([2, 20/98, f], [2, 19/98, f]).
step_left([2, 20/102, f], [2, 19/102, f]).
step_left([2, 20/110, f], [2, 19/110, f]).
step_left([2, 20/118, f], [2, 19/118, f]).
step_left([2, 21/0, f], [2, 20/0, f]).
step_left([2, 21/2, f], [2, 20/2, f]).
step_left([2, 21/6, f], [2, 20/6, f]).
step_left([2, 21/8, f], [2, 20/8, f]).
step_left([2, 21/12, f], [2, 20/12, f]).
step_left([2, 21/14, f], [2, 20/14, f]).
step_left([2, 21/18, f], [2, 20/18, f]).
step_left([2, 21/20, f], [2, 20/20, f]).
step_left([2, 21/22, f], [2, 20/22, f]).
step_left([2, 21/26, f], [2, 20/26, f]).
step_left([2, 21/32, f], [2, 20/32, f]).
step_left([2, 21/34, f], [2, 20/34, f]).
step_left([2, 21/42, f], [2, 20/42, f]).
step_left([2, 21/46, f], [2, 20/46, f]).
step_left([2, 21/52, f], [2, 20/52, f]).
step_left([2, 21/60, f], [2, 20/60, f]).
step_left([2, 21/64, f], [2, 20/64, f]).
step_left([2, 21/66, f], [2, 20/66, f]).
step_left([2, 21/68, f], [2, 20/68, f]).
step_left([2, 21/72, f], [2, 20/72, f]).
step_left([2, 21/74, f], [2, 20/74, f]).
step_left([2, 21/82, f], [2, 20/82, f]).
step_left([2, 21/84, f], [2, 20/84, f]).
step_left([2, 21/90, f], [2, 20/90, f]).
step_left([2, 21/92, f], [2, 20/92, f]).
step_left([2, 21/94, f], [2, 20/94, f]).
step_left([2, 21/98, f], [2, 20/98, f]).
step_left([2, 21/100, f], [2, 20/100, f]).
step_left([2, 21/106, f], [2, 20/106, f]).
step_left([2, 21/112, f], [2, 20/112, f]).
step_left([2, 22/0, f], [2, 21/0, f]).
step_left([2, 22/2, f], [2, 21/2, f]).
step_left([2, 22/6, f], [2, 21/6, f]).
step_left([2, 22/8, f], [2, 21/8, f]).
step_left([2, 22/12, f], [2, 21/12, f]).
step_left([2, 22/14, f], [2, 21/14, f]).
step_left([2, 22/18, f], [2, 21/18, f]).
step_left([2, 22/20, f], [2, 21/20, f]).
step_left([2, 22/22, f], [2, 21/22, f]).
step_left([2, 22/26, f], [2, 21/26, f]).
step_left([2, 22/32, f], [2, 21/32, f]).
step_left([2, 22/34, f], [2, 21/34, f]).
step_left([2, 22/42, f], [2, 21/42, f]).
step_left([2, 22/46, f], [2, 21/46, f]).
step_left([2, 22/52, f], [2, 21/52, f]).
step_left([2, 22/60, f], [2, 21/60, f]).
step_left([2, 22/64, f], [2, 21/64, f]).
step_left([2, 22/66, f], [2, 21/66, f]).
step_left([2, 22/68, f], [2, 21/68, f]).
step_left([2, 22/72, f], [2, 21/72, f]).
step_left([2, 22/74, f], [2, 21/74, f]).
step_left([2, 22/82, f], [2, 21/82, f]).
step_left([2, 22/84, f], [2, 21/84, f]).
step_left([2, 22/90, f], [2, 21/90, f]).
step_left([2, 22/92, f], [2, 21/92, f]).
step_left([2, 22/94, f], [2, 21/94, f]).
step_left([2, 22/98, f], [2, 21/98, f]).
step_left([2, 22/100, f], [2, 21/100, f]).
step_left([2, 22/106, f], [2, 21/106, f]).
step_left([2, 22/112, f], [2, 21/112, f]).
step_left([2, 23/0, f], [2, 22/0, f]).
step_left([2, 23/2, f], [2, 22/2, f]).
step_left([2, 23/4, f], [2, 22/4, f]).
step_left([2, 23/8, f], [2, 22/8, f]).
step_left([2, 23/10, f], [2, 22/10, f]).
step_left([2, 23/14, f], [2, 22/14, f]).
step_left([2, 23/16, f], [2, 22/16, f]).
step_left([2, 23/20, f], [2, 22/20, f]).
step_left([2, 23/22, f], [2, 22/22, f]).
step_left([2, 23/24, f], [2, 22/24, f]).
step_left([2, 23/28, f], [2, 22/28, f]).
step_left([2, 23/30, f], [2, 22/30, f]).
step_left([2, 23/32, f], [2, 22/32, f]).
step_left([2, 23/34, f], [2, 22/34, f]).
step_left([2, 23/38, f], [2, 22/38, f]).
step_left([2, 23/40, f], [2, 22/40, f]).
step_left([2, 23/48, f], [2, 22/48, f]).
step_left([2, 23/50, f], [2, 22/50, f]).
step_left([2, 23/54, f], [2, 22/54, f]).
step_left([2, 23/58, f], [2, 22/58, f]).
step_left([2, 23/62, f], [2, 22/62, f]).
step_left([2, 23/64, f], [2, 22/64, f]).
step_left([2, 23/66, f], [2, 22/66, f]).
step_left([2, 23/70, f], [2, 22/70, f]).
step_left([2, 23/72, f], [2, 22/72, f]).
step_left([2, 23/74, f], [2, 22/74, f]).
step_left([2, 23/76, f], [2, 22/76, f]).
step_left([2, 23/78, f], [2, 22/78, f]).
step_left([2, 23/92, f], [2, 22/92, f]).
step_left([2, 23/96, f], [2, 22/96, f]).
step_left([2, 23/100, f], [2, 22/100, f]).
step_left([2, 23/104, f], [2, 22/104, f]).
step_left([2, 23/108, f], [2, 22/108, f]).
step_left([2, 23/110, f], [2, 22/110, f]).
step_left([2, 23/112, f], [2, 22/112, f]).
step_left([2, 23/114, f], [2, 22/114, f]).
step_left([2, 23/116, f], [2, 22/116, f]).
step_left([2, 24/0, f], [2, 23/0, f]).
step_left([2, 24/2, f], [2, 23/2, f]).
step_left([2, 24/4, f], [2, 23/4, f]).
step_left([2, 24/8, f], [2, 23/8, f]).
step_left([2, 24/10, f], [2, 23/10, f]).
step_left([2, 24/14, f], [2, 23/14, f]).
step_left([2, 24/16, f], [2, 23/16, f]).
step_left([2, 24/20, f], [2, 23/20, f]).
step_left([2, 24/22, f], [2, 23/22, f]).
step_left([2, 24/24, f], [2, 23/24, f]).
step_left([2, 24/28, f], [2, 23/28, f]).
step_left([2, 24/30, f], [2, 23/30, f]).
step_left([2, 24/32, f], [2, 23/32, f]).
step_left([2, 24/34, f], [2, 23/34, f]).
step_left([2, 24/38, f], [2, 23/38, f]).
step_left([2, 24/40, f], [2, 23/40, f]).
step_left([2, 24/48, f], [2, 23/48, f]).
step_left([2, 24/50, f], [2, 23/50, f]).
step_left([2, 24/54, f], [2, 23/54, f]).
step_left([2, 24/58, f], [2, 23/58, f]).
step_left([2, 24/62, f], [2, 23/62, f]).
step_left([2, 24/64, f], [2, 23/64, f]).
step_left([2, 24/66, f], [2, 23/66, f]).
step_left([2, 24/70, f], [2, 23/70, f]).
step_left([2, 24/72, f], [2, 23/72, f]).
step_left([2, 24/74, f], [2, 23/74, f]).
step_left([2, 24/76, f], [2, 23/76, f]).
step_left([2, 24/78, f], [2, 23/78, f]).
step_left([2, 24/92, f], [2, 23/92, f]).
step_left([2, 24/96, f], [2, 23/96, f]).
step_left([2, 24/100, f], [2, 23/100, f]).
step_left([2, 24/104, f], [2, 23/104, f]).
step_left([2, 24/108, f], [2, 23/108, f]).
step_left([2, 24/110, f], [2, 23/110, f]).
step_left([2, 24/112, f], [2, 23/112, f]).
step_left([2, 24/114, f], [2, 23/114, f]).
step_left([2, 24/116, f], [2, 23/116, f]).
step_left([2, 25/0, f], [2, 24/0, f]).
step_left([2, 25/4, f], [2, 24/4, f]).
step_left([2, 25/12, f], [2, 24/12, f]).
step_left([2, 25/14, f], [2, 24/14, f]).
step_left([2, 25/18, f], [2, 24/18, f]).
step_left([2, 25/26, f], [2, 24/26, f]).
step_left([2, 25/28, f], [2, 24/28, f]).
step_left([2, 25/36, f], [2, 24/36, f]).
step_left([2, 25/38, f], [2, 24/38, f]).
step_left([2, 25/42, f], [2, 24/42, f]).
step_left([2, 25/48, f], [2, 24/48, f]).
step_left([2, 25/50, f], [2, 24/50, f]).
step_left([2, 25/52, f], [2, 24/52, f]).
step_left([2, 25/60, f], [2, 24/60, f]).
step_left([2, 25/62, f], [2, 24/62, f]).
step_left([2, 25/64, f], [2, 24/64, f]).
step_left([2, 25/68, f], [2, 24/68, f]).
step_left([2, 25/72, f], [2, 24/72, f]).
step_left([2, 25/74, f], [2, 24/74, f]).
step_left([2, 25/76, f], [2, 24/76, f]).
step_left([2, 25/84, f], [2, 24/84, f]).
step_left([2, 25/92, f], [2, 24/92, f]).
step_left([2, 25/98, f], [2, 24/98, f]).
step_left([2, 25/106, f], [2, 24/106, f]).
step_left([2, 25/110, f], [2, 24/110, f]).
step_left([2, 25/112, f], [2, 24/112, f]).
step_left([2, 25/118, f], [2, 24/118, f]).
step_left([2, 26/0, f], [2, 25/0, f]).
step_left([2, 26/4, f], [2, 25/4, f]).
step_left([2, 26/12, f], [2, 25/12, f]).
step_left([2, 26/14, f], [2, 25/14, f]).
step_left([2, 26/18, f], [2, 25/18, f]).
step_left([2, 26/26, f], [2, 25/26, f]).
step_left([2, 26/28, f], [2, 25/28, f]).
step_left([2, 26/36, f], [2, 25/36, f]).
step_left([2, 26/38, f], [2, 25/38, f]).
step_left([2, 26/42, f], [2, 25/42, f]).
step_left([2, 26/48, f], [2, 25/48, f]).
step_left([2, 26/50, f], [2, 25/50, f]).
step_left([2, 26/52, f], [2, 25/52, f]).
step_left([2, 26/60, f], [2, 25/60, f]).
step_left([2, 26/62, f], [2, 25/62, f]).
step_left([2, 26/64, f], [2, 25/64, f]).
step_left([2, 26/68, f], [2, 25/68, f]).
step_left([2, 26/72, f], [2, 25/72, f]).
step_left([2, 26/74, f], [2, 25/74, f]).
step_left([2, 26/76, f], [2, 25/76, f]).
step_left([2, 26/84, f], [2, 25/84, f]).
step_left([2, 26/92, f], [2, 25/92, f]).
step_left([2, 26/98, f], [2, 25/98, f]).
step_left([2, 26/106, f], [2, 25/106, f]).
step_left([2, 26/110, f], [2, 25/110, f]).
step_left([2, 26/112, f], [2, 25/112, f]).
step_left([2, 26/118, f], [2, 25/118, f]).
step_left([2, 27/2, f], [2, 26/2, f]).
step_left([2, 27/4, f], [2, 26/4, f]).
step_left([2, 27/6, f], [2, 26/6, f]).
step_left([2, 27/8, f], [2, 26/8, f]).
step_left([2, 27/10, f], [2, 26/10, f]).
step_left([2, 27/16, f], [2, 26/16, f]).
step_left([2, 27/20, f], [2, 26/20, f]).
step_left([2, 27/32, f], [2, 26/32, f]).
step_left([2, 27/34, f], [2, 26/34, f]).
step_left([2, 27/36, f], [2, 26/36, f]).
step_left([2, 27/40, f], [2, 26/40, f]).
step_left([2, 27/44, f], [2, 26/44, f]).
step_left([2, 27/46, f], [2, 26/46, f]).
step_left([2, 27/56, f], [2, 26/56, f]).
step_left([2, 27/58, f], [2, 26/58, f]).
step_left([2, 27/60, f], [2, 26/60, f]).
step_left([2, 27/66, f], [2, 26/66, f]).
step_left([2, 27/72, f], [2, 26/72, f]).
step_left([2, 27/74, f], [2, 26/74, f]).
step_left([2, 27/76, f], [2, 26/76, f]).
step_left([2, 27/78, f], [2, 26/78, f]).
step_left([2, 27/82, f], [2, 26/82, f]).
step_left([2, 27/88, f], [2, 26/88, f]).
step_left([2, 27/94, f], [2, 26/94, f]).
step_left([2, 27/96, f], [2, 26/96, f]).
step_left([2, 27/100, f], [2, 26/100, f]).
step_left([2, 27/102, f], [2, 26/102, f]).
step_left([2, 27/104, f], [2, 26/104, f]).
step_left([2, 27/108, f], [2, 26/108, f]).
step_left([2, 27/110, f], [2, 26/110, f]).
step_left([2, 27/118, f], [2, 26/118, f]).
step_left([2, 28/2, f], [2, 27/2, f]).
step_left([2, 28/4, f], [2, 27/4, f]).
step_left([2, 28/6, f], [2, 27/6, f]).
step_left([2, 28/8, f], [2, 27/8, f]).
step_left([2, 28/10, f], [2, 27/10, f]).
step_left([2, 28/16, f], [2, 27/16, f]).
step_left([2, 28/20, f], [2, 27/20, f]).
step_left([2, 28/32, f], [2, 27/32, f]).
step_left([2, 28/34, f], [2, 27/34, f]).
step_left([2, 28/36, f], [2, 27/36, f]).
step_left([2, 28/40, f], [2, 27/40, f]).
step_left([2, 28/44, f], [2, 27/44, f]).
step_left([2, 28/46, f], [2, 27/46, f]).
step_left([2, 28/56, f], [2, 27/56, f]).
step_left([2, 28/58, f], [2, 27/58, f]).
step_left([2, 28/60, f], [2, 27/60, f]).
step_left([2, 28/66, f], [2, 27/66, f]).
step_left([2, 28/72, f], [2, 27/72, f]).
step_left([2, 28/74, f], [2, 27/74, f]).
step_left([2, 28/76, f], [2, 27/76, f]).
step_left([2, 28/78, f], [2, 27/78, f]).
step_left([2, 28/82, f], [2, 27/82, f]).
step_left([2, 28/88, f], [2, 27/88, f]).
step_left([2, 28/94, f], [2, 27/94, f]).
step_left([2, 28/96, f], [2, 27/96, f]).
step_left([2, 28/100, f], [2, 27/100, f]).
step_left([2, 28/102, f], [2, 27/102, f]).
step_left([2, 28/104, f], [2, 27/104, f]).
step_left([2, 28/108, f], [2, 27/108, f]).
step_left([2, 28/110, f], [2, 27/110, f]).
step_left([2, 28/118, f], [2, 27/118, f]).
step_left([2, 29/0, f], [2, 28/0, f]).
step_left([2, 29/2, f], [2, 28/2, f]).
step_left([2, 29/6, f], [2, 28/6, f]).
step_left([2, 29/8, f], [2, 28/8, f]).
step_left([2, 29/14, f], [2, 28/14, f]).
step_left([2, 29/20, f], [2, 28/20, f]).
step_left([2, 29/22, f], [2, 28/22, f]).
step_left([2, 29/28, f], [2, 28/28, f]).
step_left([2, 29/30, f], [2, 28/30, f]).
step_left([2, 29/34, f], [2, 28/34, f]).
step_left([2, 29/36, f], [2, 28/36, f]).
step_left([2, 29/38, f], [2, 28/38, f]).
step_left([2, 29/42, f], [2, 28/42, f]).
step_left([2, 29/48, f], [2, 28/48, f]).
step_left([2, 29/52, f], [2, 28/52, f]).
step_left([2, 29/54, f], [2, 28/54, f]).
step_left([2, 29/58, f], [2, 28/58, f]).
step_left([2, 29/64, f], [2, 28/64, f]).
step_left([2, 29/66, f], [2, 28/66, f]).
step_left([2, 29/70, f], [2, 28/70, f]).
step_left([2, 29/72, f], [2, 28/72, f]).
step_left([2, 29/74, f], [2, 28/74, f]).
step_left([2, 29/80, f], [2, 28/80, f]).
step_left([2, 29/88, f], [2, 28/88, f]).
step_left([2, 29/98, f], [2, 28/98, f]).
step_left([2, 29/112, f], [2, 28/112, f]).
step_left([2, 29/116, f], [2, 28/116, f]).
step_left([2, 30/0, f], [2, 29/0, f]).
step_left([2, 30/2, f], [2, 29/2, f]).
step_left([2, 30/6, f], [2, 29/6, f]).
step_left([2, 30/8, f], [2, 29/8, f]).
step_left([2, 30/14, f], [2, 29/14, f]).
step_left([2, 30/20, f], [2, 29/20, f]).
step_left([2, 30/22, f], [2, 29/22, f]).
step_left([2, 30/28, f], [2, 29/28, f]).
step_left([2, 30/30, f], [2, 29/30, f]).
step_left([2, 30/34, f], [2, 29/34, f]).
step_left([2, 30/36, f], [2, 29/36, f]).
step_left([2, 30/38, f], [2, 29/38, f]).
step_left([2, 30/42, f], [2, 29/42, f]).
step_left([2, 30/48, f], [2, 29/48, f]).
step_left([2, 30/52, f], [2, 29/52, f]).
step_left([2, 30/54, f], [2, 29/54, f]).
step_left([2, 30/58, f], [2, 29/58, f]).
step_left([2, 30/64, f], [2, 29/64, f]).
step_left([2, 30/66, f], [2, 29/66, f]).
step_left([2, 30/70, f], [2, 29/70, f]).
step_left([2, 30/72, f], [2, 29/72, f]).
step_left([2, 30/74, f], [2, 29/74, f]).
step_left([2, 30/80, f], [2, 29/80, f]).
step_left([2, 30/88, f], [2, 29/88, f]).
step_left([2, 30/98, f], [2, 29/98, f]).
step_left([2, 30/112, f], [2, 29/112, f]).
step_left([2, 30/116, f], [2, 29/116, f]).
step_left([2, 31/0, f], [2, 30/0, f]).
step_left([2, 31/8, f], [2, 30/8, f]).
step_left([2, 31/10, f], [2, 30/10, f]).
step_left([2, 31/12, f], [2, 30/12, f]).
step_left([2, 31/16, f], [2, 30/16, f]).
step_left([2, 31/18, f], [2, 30/18, f]).
step_left([2, 31/26, f], [2, 30/26, f]).
step_left([2, 31/28, f], [2, 30/28, f]).
step_left([2, 31/32, f], [2, 30/32, f]).
step_left([2, 31/34, f], [2, 30/34, f]).
step_left([2, 31/36, f], [2, 30/36, f]).
step_left([2, 31/38, f], [2, 30/38, f]).
step_left([2, 31/48, f], [2, 30/48, f]).
step_left([2, 31/50, f], [2, 30/50, f]).
step_left([2, 31/56, f], [2, 30/56, f]).
step_left([2, 31/58, f], [2, 30/58, f]).
step_left([2, 31/60, f], [2, 30/60, f]).
step_left([2, 31/62, f], [2, 30/62, f]).
step_left([2, 31/64, f], [2, 30/64, f]).
step_left([2, 31/66, f], [2, 30/66, f]).
step_left([2, 31/68, f], [2, 30/68, f]).
step_left([2, 31/70, f], [2, 30/70, f]).
step_left([2, 31/72, f], [2, 30/72, f]).
step_left([2, 31/76, f], [2, 30/76, f]).
step_left([2, 31/80, f], [2, 30/80, f]).
step_left([2, 31/82, f], [2, 30/82, f]).
step_left([2, 31/84, f], [2, 30/84, f]).
step_left([2, 31/86, f], [2, 30/86, f]).
step_left([2, 31/92, f], [2, 30/92, f]).
step_left([2, 31/94, f], [2, 30/94, f]).
step_left([2, 31/96, f], [2, 30/96, f]).
step_left([2, 31/102, f], [2, 30/102, f]).
step_left([2, 31/108, f], [2, 30/108, f]).
step_left([2, 31/118, f], [2, 30/118, f]).
step_left([2, 32/0, f], [2, 31/0, f]).
step_left([2, 32/8, f], [2, 31/8, f]).
step_left([2, 32/10, f], [2, 31/10, f]).
step_left([2, 32/12, f], [2, 31/12, f]).
step_left([2, 32/16, f], [2, 31/16, f]).
step_left([2, 32/18, f], [2, 31/18, f]).
step_left([2, 32/26, f], [2, 31/26, f]).
step_left([2, 32/28, f], [2, 31/28, f]).
step_left([2, 32/32, f], [2, 31/32, f]).
step_left([2, 32/34, f], [2, 31/34, f]).
step_left([2, 32/36, f], [2, 31/36, f]).
step_left([2, 32/38, f], [2, 31/38, f]).
step_left([2, 32/48, f], [2, 31/48, f]).
step_left([2, 32/50, f], [2, 31/50, f]).
step_left([2, 32/56, f], [2, 31/56, f]).
step_left([2, 32/58, f], [2, 31/58, f]).
step_left([2, 32/60, f], [2, 31/60, f]).
step_left([2, 32/62, f], [2, 31/62, f]).
step_left([2, 32/64, f], [2, 31/64, f]).
step_left([2, 32/66, f], [2, 31/66, f]).
step_left([2, 32/68, f], [2, 31/68, f]).
step_left([2, 32/70, f], [2, 31/70, f]).
step_left([2, 32/72, f], [2, 31/72, f]).
step_left([2, 32/76, f], [2, 31/76, f]).
step_left([2, 32/80, f], [2, 31/80, f]).
step_left([2, 32/82, f], [2, 31/82, f]).
step_left([2, 32/84, f], [2, 31/84, f]).
step_left([2, 32/86, f], [2, 31/86, f]).
step_left([2, 32/92, f], [2, 31/92, f]).
step_left([2, 32/94, f], [2, 31/94, f]).
step_left([2, 32/96, f], [2, 31/96, f]).
step_left([2, 32/102, f], [2, 31/102, f]).
step_left([2, 32/108, f], [2, 31/108, f]).
step_left([2, 32/118, f], [2, 31/118, f]).
step_left([2, 33/0, f], [2, 32/0, f]).
step_left([2, 33/4, f], [2, 32/4, f]).
step_left([2, 33/8, f], [2, 32/8, f]).
step_left([2, 33/10, f], [2, 32/10, f]).
step_left([2, 33/18, f], [2, 32/18, f]).
step_left([2, 33/20, f], [2, 32/20, f]).
step_left([2, 33/22, f], [2, 32/22, f]).
step_left([2, 33/24, f], [2, 32/24, f]).
step_left([2, 33/28, f], [2, 32/28, f]).
step_left([2, 33/30, f], [2, 32/30, f]).
step_left([2, 33/32, f], [2, 32/32, f]).
step_left([2, 33/34, f], [2, 32/34, f]).
step_left([2, 33/36, f], [2, 32/36, f]).
step_left([2, 33/44, f], [2, 32/44, f]).
step_left([2, 33/46, f], [2, 32/46, f]).
step_left([2, 33/48, f], [2, 32/48, f]).
step_left([2, 33/54, f], [2, 32/54, f]).
step_left([2, 33/60, f], [2, 32/60, f]).
step_left([2, 33/62, f], [2, 32/62, f]).
step_left([2, 33/64, f], [2, 32/64, f]).
step_left([2, 33/68, f], [2, 32/68, f]).
step_left([2, 33/70, f], [2, 32/70, f]).
step_left([2, 33/74, f], [2, 32/74, f]).
step_left([2, 33/78, f], [2, 32/78, f]).
step_left([2, 33/80, f], [2, 32/80, f]).
step_left([2, 33/82, f], [2, 32/82, f]).
step_left([2, 33/84, f], [2, 32/84, f]).
step_left([2, 33/98, f], [2, 32/98, f]).
step_left([2, 33/100, f], [2, 32/100, f]).
step_left([2, 33/102, f], [2, 32/102, f]).
step_left([2, 33/104, f], [2, 32/104, f]).
step_left([2, 33/110, f], [2, 32/110, f]).
step_left([2, 33/112, f], [2, 32/112, f]).
step_left([2, 33/116, f], [2, 32/116, f]).
step_left([2, 33/118, f], [2, 32/118, f]).
step_left([2, 34/0, f], [2, 33/0, f]).
step_left([2, 34/4, f], [2, 33/4, f]).
step_left([2, 34/8, f], [2, 33/8, f]).
step_left([2, 34/10, f], [2, 33/10, f]).
step_left([2, 34/18, f], [2, 33/18, f]).
step_left([2, 34/20, f], [2, 33/20, f]).
step_left([2, 34/22, f], [2, 33/22, f]).
step_left([2, 34/24, f], [2, 33/24, f]).
step_left([2, 34/28, f], [2, 33/28, f]).
step_left([2, 34/30, f], [2, 33/30, f]).
step_left([2, 34/32, f], [2, 33/32, f]).
step_left([2, 34/34, f], [2, 33/34, f]).
step_left([2, 34/36, f], [2, 33/36, f]).
step_left([2, 34/44, f], [2, 33/44, f]).
step_left([2, 34/46, f], [2, 33/46, f]).
step_left([2, 34/48, f], [2, 33/48, f]).
step_left([2, 34/54, f], [2, 33/54, f]).
step_left([2, 34/60, f], [2, 33/60, f]).
step_left([2, 34/62, f], [2, 33/62, f]).
step_left([2, 34/64, f], [2, 33/64, f]).
step_left([2, 34/68, f], [2, 33/68, f]).
step_left([2, 34/70, f], [2, 33/70, f]).
step_left([2, 34/74, f], [2, 33/74, f]).
step_left([2, 34/78, f], [2, 33/78, f]).
step_left([2, 34/80, f], [2, 33/80, f]).
step_left([2, 34/82, f], [2, 33/82, f]).
step_left([2, 34/84, f], [2, 33/84, f]).
step_left([2, 34/98, f], [2, 33/98, f]).
step_left([2, 34/100, f], [2, 33/100, f]).
step_left([2, 34/102, f], [2, 33/102, f]).
step_left([2, 34/104, f], [2, 33/104, f]).
step_left([2, 34/110, f], [2, 33/110, f]).
step_left([2, 34/112, f], [2, 33/112, f]).
step_left([2, 34/116, f], [2, 33/116, f]).
step_left([2, 34/118, f], [2, 33/118, f]).
step_left([2, 35/2, f], [2, 34/2, f]).
step_left([2, 35/4, f], [2, 34/4, f]).
step_left([2, 35/6, f], [2, 34/6, f]).
step_left([2, 35/10, f], [2, 34/10, f]).
step_left([2, 35/12, f], [2, 34/12, f]).
step_left([2, 35/14, f], [2, 34/14, f]).
step_left([2, 35/20, f], [2, 34/20, f]).
step_left([2, 35/22, f], [2, 34/22, f]).
step_left([2, 35/24, f], [2, 34/24, f]).
step_left([2, 35/26, f], [2, 34/26, f]).
step_left([2, 35/28, f], [2, 34/28, f]).
step_left([2, 35/32, f], [2, 34/32, f]).
step_left([2, 35/34, f], [2, 34/34, f]).
step_left([2, 35/36, f], [2, 34/36, f]).
step_left([2, 35/40, f], [2, 34/40, f]).
step_left([2, 35/50, f], [2, 34/50, f]).
step_left([2, 35/52, f], [2, 34/52, f]).
step_left([2, 35/54, f], [2, 34/54, f]).
step_left([2, 35/56, f], [2, 34/56, f]).
step_left([2, 35/58, f], [2, 34/58, f]).
step_left([2, 35/60, f], [2, 34/60, f]).
step_left([2, 35/62, f], [2, 34/62, f]).
step_left([2, 35/64, f], [2, 34/64, f]).
step_left([2, 35/70, f], [2, 34/70, f]).
step_left([2, 35/72, f], [2, 34/72, f]).
step_left([2, 35/76, f], [2, 34/76, f]).
step_left([2, 35/78, f], [2, 34/78, f]).
step_left([2, 35/82, f], [2, 34/82, f]).
step_left([2, 35/84, f], [2, 34/84, f]).
step_left([2, 35/88, f], [2, 34/88, f]).
step_left([2, 35/90, f], [2, 34/90, f]).
step_left([2, 35/94, f], [2, 34/94, f]).
step_left([2, 35/96, f], [2, 34/96, f]).
step_left([2, 35/98, f], [2, 34/98, f]).
step_left([2, 35/102, f], [2, 34/102, f]).
step_left([2, 35/104, f], [2, 34/104, f]).
step_left([2, 35/114, f], [2, 34/114, f]).
step_left([2, 35/116, f], [2, 34/116, f]).
step_left([2, 35/118, f], [2, 34/118, f]).
step_left([2, 36/2, f], [2, 35/2, f]).
step_left([2, 36/4, f], [2, 35/4, f]).
step_left([2, 36/6, f], [2, 35/6, f]).
step_left([2, 36/10, f], [2, 35/10, f]).
step_left([2, 36/12, f], [2, 35/12, f]).
step_left([2, 36/14, f], [2, 35/14, f]).
step_left([2, 36/20, f], [2, 35/20, f]).
step_left([2, 36/22, f], [2, 35/22, f]).
step_left([2, 36/24, f], [2, 35/24, f]).
step_left([2, 36/26, f], [2, 35/26, f]).
step_left([2, 36/28, f], [2, 35/28, f]).
step_left([2, 36/32, f], [2, 35/32, f]).
step_left([2, 36/34, f], [2, 35/34, f]).
step_left([2, 36/36, f], [2, 35/36, f]).
step_left([2, 36/40, f], [2, 35/40, f]).
step_left([2, 36/50, f], [2, 35/50, f]).
step_left([2, 36/52, f], [2, 35/52, f]).
step_left([2, 36/54, f], [2, 35/54, f]).
step_left([2, 36/56, f], [2, 35/56, f]).
step_left([2, 36/58, f], [2, 35/58, f]).
step_left([2, 36/60, f], [2, 35/60, f]).
step_left([2, 36/62, f], [2, 35/62, f]).
step_left([2, 36/64, f], [2, 35/64, f]).
step_left([2, 36/70, f], [2, 35/70, f]).
step_left([2, 36/72, f], [2, 35/72, f]).
step_left([2, 36/76, f], [2, 35/76, f]).
step_left([2, 36/78, f], [2, 35/78, f]).
step_left([2, 36/82, f], [2, 35/82, f]).
step_left([2, 36/84, f], [2, 35/84, f]).
step_left([2, 36/88, f], [2, 35/88, f]).
step_left([2, 36/90, f], [2, 35/90, f]).
step_left([2, 36/94, f], [2, 35/94, f]).
step_left([2, 36/96, f], [2, 35/96, f]).
step_left([2, 36/98, f], [2, 35/98, f]).
step_left([2, 36/102, f], [2, 35/102, f]).
step_left([2, 36/104, f], [2, 35/104, f]).
step_left([2, 36/114, f], [2, 35/114, f]).
step_left([2, 36/116, f], [2, 35/116, f]).
step_left([2, 36/118, f], [2, 35/118, f]).
step_left([2, 37/0, f], [2, 36/0, f]).
step_left([2, 37/2, f], [2, 36/2, f]).
step_left([2, 37/8, f], [2, 36/8, f]).
step_left([2, 37/16, f], [2, 36/16, f]).
step_left([2, 37/18, f], [2, 36/18, f]).
step_left([2, 37/26, f], [2, 36/26, f]).
step_left([2, 37/28, f], [2, 36/28, f]).
step_left([2, 37/30, f], [2, 36/30, f]).
step_left([2, 37/34, f], [2, 36/34, f]).
step_left([2, 37/38, f], [2, 36/38, f]).
step_left([2, 37/52, f], [2, 36/52, f]).
step_left([2, 37/56, f], [2, 36/56, f]).
step_left([2, 37/72, f], [2, 36/72, f]).
step_left([2, 37/74, f], [2, 36/74, f]).
step_left([2, 37/80, f], [2, 36/80, f]).
step_left([2, 37/82, f], [2, 36/82, f]).
step_left([2, 37/86, f], [2, 36/86, f]).
step_left([2, 37/88, f], [2, 36/88, f]).
step_left([2, 37/90, f], [2, 36/90, f]).
step_left([2, 37/94, f], [2, 36/94, f]).
step_left([2, 37/98, f], [2, 36/98, f]).
step_left([2, 37/100, f], [2, 36/100, f]).
step_left([2, 37/104, f], [2, 36/104, f]).
step_left([2, 37/106, f], [2, 36/106, f]).
step_left([2, 37/108, f], [2, 36/108, f]).
step_left([2, 37/110, f], [2, 36/110, f]).
step_left([2, 37/112, f], [2, 36/112, f]).
step_left([2, 37/118, f], [2, 36/118, f]).
step_left([2, 38/0, f], [2, 37/0, f]).
step_left([2, 38/2, f], [2, 37/2, f]).
step_left([2, 38/8, f], [2, 37/8, f]).
step_left([2, 38/16, f], [2, 37/16, f]).
step_left([2, 38/18, f], [2, 37/18, f]).
step_left([2, 38/26, f], [2, 37/26, f]).
step_left([2, 38/28, f], [2, 37/28, f]).
step_left([2, 38/30, f], [2, 37/30, f]).
step_left([2, 38/34, f], [2, 37/34, f]).
step_left([2, 38/38, f], [2, 37/38, f]).
step_left([2, 38/52, f], [2, 37/52, f]).
step_left([2, 38/56, f], [2, 37/56, f]).
step_left([2, 38/72, f], [2, 37/72, f]).
step_left([2, 38/74, f], [2, 37/74, f]).
step_left([2, 38/80, f], [2, 37/80, f]).
step_left([2, 38/82, f], [2, 37/82, f]).
step_left([2, 38/86, f], [2, 37/86, f]).
step_left([2, 38/88, f], [2, 37/88, f]).
step_left([2, 38/90, f], [2, 37/90, f]).
step_left([2, 38/94, f], [2, 37/94, f]).
step_left([2, 38/98, f], [2, 37/98, f]).
step_left([2, 38/100, f], [2, 37/100, f]).
step_left([2, 38/104, f], [2, 37/104, f]).
step_left([2, 38/106, f], [2, 37/106, f]).
step_left([2, 38/108, f], [2, 37/108, f]).
step_left([2, 38/110, f], [2, 37/110, f]).
step_left([2, 38/112, f], [2, 37/112, f]).
step_left([2, 38/118, f], [2, 37/118, f]).
step_left([2, 39/0, f], [2, 38/0, f]).
step_left([2, 39/2, f], [2, 38/2, f]).
step_left([2, 39/4, f], [2, 38/4, f]).
step_left([2, 39/8, f], [2, 38/8, f]).
step_left([2, 39/10, f], [2, 38/10, f]).
step_left([2, 39/12, f], [2, 38/12, f]).
step_left([2, 39/14, f], [2, 38/14, f]).
step_left([2, 39/18, f], [2, 38/18, f]).
step_left([2, 39/20, f], [2, 38/20, f]).
step_left([2, 39/22, f], [2, 38/22, f]).
step_left([2, 39/24, f], [2, 38/24, f]).
step_left([2, 39/30, f], [2, 38/30, f]).
step_left([2, 39/32, f], [2, 38/32, f]).
step_left([2, 39/34, f], [2, 38/34, f]).
step_left([2, 39/36, f], [2, 38/36, f]).
step_left([2, 39/44, f], [2, 38/44, f]).
step_left([2, 39/50, f], [2, 38/50, f]).
step_left([2, 39/54, f], [2, 38/54, f]).
step_left([2, 39/56, f], [2, 38/56, f]).
step_left([2, 39/58, f], [2, 38/58, f]).
step_left([2, 39/70, f], [2, 38/70, f]).
step_left([2, 39/72, f], [2, 38/72, f]).
step_left([2, 39/74, f], [2, 38/74, f]).
step_left([2, 39/76, f], [2, 38/76, f]).
step_left([2, 39/82, f], [2, 38/82, f]).
step_left([2, 39/84, f], [2, 38/84, f]).
step_left([2, 39/88, f], [2, 38/88, f]).
step_left([2, 39/92, f], [2, 38/92, f]).
step_left([2, 39/102, f], [2, 38/102, f]).
step_left([2, 39/108, f], [2, 38/108, f]).
step_left([2, 39/110, f], [2, 38/110, f]).
step_left([2, 39/114, f], [2, 38/114, f]).
step_left([2, 39/116, f], [2, 38/116, f]).
step_left([2, 39/118, f], [2, 38/118, f]).
step_left([2, 40/0, f], [2, 39/0, f]).
step_left([2, 40/2, f], [2, 39/2, f]).
step_left([2, 40/4, f], [2, 39/4, f]).
step_left([2, 40/8, f], [2, 39/8, f]).
step_left([2, 40/10, f], [2, 39/10, f]).
step_left([2, 40/12, f], [2, 39/12, f]).
step_left([2, 40/14, f], [2, 39/14, f]).
step_left([2, 40/18, f], [2, 39/18, f]).
step_left([2, 40/20, f], [2, 39/20, f]).
step_left([2, 40/22, f], [2, 39/22, f]).
step_left([2, 40/24, f], [2, 39/24, f]).
step_left([2, 40/30, f], [2, 39/30, f]).
step_left([2, 40/32, f], [2, 39/32, f]).
step_left([2, 40/34, f], [2, 39/34, f]).
step_left([2, 40/36, f], [2, 39/36, f]).
step_left([2, 40/44, f], [2, 39/44, f]).
step_left([2, 40/50, f], [2, 39/50, f]).
step_left([2, 40/54, f], [2, 39/54, f]).
step_left([2, 40/56, f], [2, 39/56, f]).
step_left([2, 40/58, f], [2, 39/58, f]).
step_left([2, 40/70, f], [2, 39/70, f]).
step_left([2, 40/72, f], [2, 39/72, f]).
step_left([2, 40/74, f], [2, 39/74, f]).
step_left([2, 40/76, f], [2, 39/76, f]).
step_left([2, 40/82, f], [2, 39/82, f]).
step_left([2, 40/84, f], [2, 39/84, f]).
step_left([2, 40/88, f], [2, 39/88, f]).
step_left([2, 40/92, f], [2, 39/92, f]).
step_left([2, 40/102, f], [2, 39/102, f]).
step_left([2, 40/108, f], [2, 39/108, f]).
step_left([2, 40/110, f], [2, 39/110, f]).
step_left([2, 40/114, f], [2, 39/114, f]).
step_left([2, 40/116, f], [2, 39/116, f]).
step_left([2, 40/118, f], [2, 39/118, f]).
step_left([2, 41/0, f], [2, 40/0, f]).
step_left([2, 41/2, f], [2, 40/2, f]).
step_left([2, 41/4, f], [2, 40/4, f]).
step_left([2, 41/6, f], [2, 40/6, f]).
step_left([2, 41/8, f], [2, 40/8, f]).
step_left([2, 41/10, f], [2, 40/10, f]).
step_left([2, 41/12, f], [2, 40/12, f]).
step_left([2, 41/14, f], [2, 40/14, f]).
step_left([2, 41/16, f], [2, 40/16, f]).
step_left([2, 41/20, f], [2, 40/20, f]).
step_left([2, 41/28, f], [2, 40/28, f]).
step_left([2, 41/32, f], [2, 40/32, f]).
step_left([2, 41/36, f], [2, 40/36, f]).
step_left([2, 41/38, f], [2, 40/38, f]).
step_left([2, 41/40, f], [2, 40/40, f]).
step_left([2, 41/48, f], [2, 40/48, f]).
step_left([2, 41/52, f], [2, 40/52, f]).
step_left([2, 41/54, f], [2, 40/54, f]).
step_left([2, 41/56, f], [2, 40/56, f]).
step_left([2, 41/60, f], [2, 40/60, f]).
step_left([2, 41/72, f], [2, 40/72, f]).
step_left([2, 41/76, f], [2, 40/76, f]).
step_left([2, 41/78, f], [2, 40/78, f]).
step_left([2, 41/80, f], [2, 40/80, f]).
step_left([2, 41/82, f], [2, 40/82, f]).
step_left([2, 41/86, f], [2, 40/86, f]).
step_left([2, 41/90, f], [2, 40/90, f]).
step_left([2, 41/96, f], [2, 40/96, f]).
step_left([2, 41/98, f], [2, 40/98, f]).
step_left([2, 41/104, f], [2, 40/104, f]).
step_left([2, 41/106, f], [2, 40/106, f]).
step_left([2, 41/110, f], [2, 40/110, f]).
step_left([2, 41/112, f], [2, 40/112, f]).
step_left([2, 42/0, f], [2, 41/0, f]).
step_left([2, 42/2, f], [2, 41/2, f]).
step_left([2, 42/4, f], [2, 41/4, f]).
step_left([2, 42/6, f], [2, 41/6, f]).
step_left([2, 42/8, f], [2, 41/8, f]).
step_left([2, 42/10, f], [2, 41/10, f]).
step_left([2, 42/12, f], [2, 41/12, f]).
step_left([2, 42/14, f], [2, 41/14, f]).
step_left([2, 42/16, f], [2, 41/16, f]).
step_left([2, 42/20, f], [2, 41/20, f]).
step_left([2, 42/28, f], [2, 41/28, f]).
step_left([2, 42/32, f], [2, 41/32, f]).
step_left([2, 42/36, f], [2, 41/36, f]).
step_left([2, 42/38, f], [2, 41/38, f]).
step_left([2, 42/40, f], [2, 41/40, f]).
step_left([2, 42/48, f], [2, 41/48, f]).
step_left([2, 42/52, f], [2, 41/52, f]).
step_left([2, 42/54, f], [2, 41/54, f]).
step_left([2, 42/56, f], [2, 41/56, f]).
step_left([2, 42/60, f], [2, 41/60, f]).
step_left([2, 42/72, f], [2, 41/72, f]).
step_left([2, 42/76, f], [2, 41/76, f]).
step_left([2, 42/78, f], [2, 41/78, f]).
step_left([2, 42/80, f], [2, 41/80, f]).
step_left([2, 42/82, f], [2, 41/82, f]).
step_left([2, 42/86, f], [2, 41/86, f]).
step_left([2, 42/90, f], [2, 41/90, f]).
step_left([2, 42/96, f], [2, 41/96, f]).
step_left([2, 42/98, f], [2, 41/98, f]).
step_left([2, 42/104, f], [2, 41/104, f]).
step_left([2, 42/106, f], [2, 41/106, f]).
step_left([2, 42/110, f], [2, 41/110, f]).
step_left([2, 42/112, f], [2, 41/112, f]).
step_left([2, 43/0, f], [2, 42/0, f]).
step_left([2, 43/6, f], [2, 42/6, f]).
step_left([2, 43/8, f], [2, 42/8, f]).
step_left([2, 43/10, f], [2, 42/10, f]).
step_left([2, 43/12, f], [2, 42/12, f]).
step_left([2, 43/14, f], [2, 42/14, f]).
step_left([2, 43/16, f], [2, 42/16, f]).
step_left([2, 43/18, f], [2, 42/18, f]).
step_left([2, 43/26, f], [2, 42/26, f]).
step_left([2, 43/32, f], [2, 42/32, f]).
step_left([2, 43/34, f], [2, 42/34, f]).
step_left([2, 43/36, f], [2, 42/36, f]).
step_left([2, 43/38, f], [2, 42/38, f]).
step_left([2, 43/46, f], [2, 42/46, f]).
step_left([2, 43/50, f], [2, 42/50, f]).
step_left([2, 43/52, f], [2, 42/52, f]).
step_left([2, 43/54, f], [2, 42/54, f]).
step_left([2, 43/56, f], [2, 42/56, f]).
step_left([2, 43/60, f], [2, 42/60, f]).
step_left([2, 43/62, f], [2, 42/62, f]).
step_left([2, 43/68, f], [2, 42/68, f]).
step_left([2, 43/72, f], [2, 42/72, f]).
step_left([2, 43/74, f], [2, 42/74, f]).
step_left([2, 43/86, f], [2, 42/86, f]).
step_left([2, 43/88, f], [2, 42/88, f]).
step_left([2, 43/92, f], [2, 42/92, f]).
step_left([2, 43/94, f], [2, 42/94, f]).
step_left([2, 43/96, f], [2, 42/96, f]).
step_left([2, 43/98, f], [2, 42/98, f]).
step_left([2, 43/102, f], [2, 42/102, f]).
step_left([2, 43/108, f], [2, 42/108, f]).
step_left([2, 43/110, f], [2, 42/110, f]).
step_left([2, 43/112, f], [2, 42/112, f]).
step_left([2, 43/114, f], [2, 42/114, f]).
step_left([2, 43/118, f], [2, 42/118, f]).
step_left([2, 44/0, f], [2, 43/0, f]).
step_left([2, 44/6, f], [2, 43/6, f]).
step_left([2, 44/8, f], [2, 43/8, f]).
step_left([2, 44/10, f], [2, 43/10, f]).
step_left([2, 44/12, f], [2, 43/12, f]).
step_left([2, 44/14, f], [2, 43/14, f]).
step_left([2, 44/16, f], [2, 43/16, f]).
step_left([2, 44/18, f], [2, 43/18, f]).
step_left([2, 44/26, f], [2, 43/26, f]).
step_left([2, 44/32, f], [2, 43/32, f]).
step_left([2, 44/34, f], [2, 43/34, f]).
step_left([2, 44/36, f], [2, 43/36, f]).
step_left([2, 44/38, f], [2, 43/38, f]).
step_left([2, 44/46, f], [2, 43/46, f]).
step_left([2, 44/50, f], [2, 43/50, f]).
step_left([2, 44/52, f], [2, 43/52, f]).
step_left([2, 44/54, f], [2, 43/54, f]).
step_left([2, 44/56, f], [2, 43/56, f]).
step_left([2, 44/60, f], [2, 43/60, f]).
step_left([2, 44/62, f], [2, 43/62, f]).
step_left([2, 44/68, f], [2, 43/68, f]).
step_left([2, 44/72, f], [2, 43/72, f]).
step_left([2, 44/74, f], [2, 43/74, f]).
step_left([2, 44/86, f], [2, 43/86, f]).
step_left([2, 44/88, f], [2, 43/88, f]).
step_left([2, 44/92, f], [2, 43/92, f]).
step_left([2, 44/94, f], [2, 43/94, f]).
step_left([2, 44/96, f], [2, 43/96, f]).
step_left([2, 44/98, f], [2, 43/98, f]).
step_left([2, 44/102, f], [2, 43/102, f]).
step_left([2, 44/108, f], [2, 43/108, f]).
step_left([2, 44/110, f], [2, 43/110, f]).
step_left([2, 44/112, f], [2, 43/112, f]).
step_left([2, 44/114, f], [2, 43/114, f]).
step_left([2, 44/118, f], [2, 43/118, f]).
step_left([2, 45/4, f], [2, 44/4, f]).
step_left([2, 45/6, f], [2, 44/6, f]).
step_left([2, 45/10, f], [2, 44/10, f]).
step_left([2, 45/12, f], [2, 44/12, f]).
step_left([2, 45/16, f], [2, 44/16, f]).
step_left([2, 45/20, f], [2, 44/20, f]).
step_left([2, 45/28, f], [2, 44/28, f]).
step_left([2, 45/30, f], [2, 44/30, f]).
step_left([2, 45/32, f], [2, 44/32, f]).
step_left([2, 45/44, f], [2, 44/44, f]).
step_left([2, 45/46, f], [2, 44/46, f]).
step_left([2, 45/48, f], [2, 44/48, f]).
step_left([2, 45/50, f], [2, 44/50, f]).
step_left([2, 45/52, f], [2, 44/52, f]).
step_left([2, 45/56, f], [2, 44/56, f]).
step_left([2, 45/58, f], [2, 44/58, f]).
step_left([2, 45/62, f], [2, 44/62, f]).
step_left([2, 45/64, f], [2, 44/64, f]).
step_left([2, 45/68, f], [2, 44/68, f]).
step_left([2, 45/70, f], [2, 44/70, f]).
step_left([2, 45/74, f], [2, 44/74, f]).
step_left([2, 45/78, f], [2, 44/78, f]).
step_left([2, 45/80, f], [2, 44/80, f]).
step_left([2, 45/82, f], [2, 44/82, f]).
step_left([2, 45/88, f], [2, 44/88, f]).
step_left([2, 45/92, f], [2, 44/92, f]).
step_left([2, 45/94, f], [2, 44/94, f]).
step_left([2, 45/96, f], [2, 44/96, f]).
step_left([2, 45/100, f], [2, 44/100, f]).
step_left([2, 45/102, f], [2, 44/102, f]).
step_left([2, 45/104, f], [2, 44/104, f]).
step_left([2, 45/110, f], [2, 44/110, f]).
step_left([2, 45/118, f], [2, 44/118, f]).
step_left([2, 46/4, f], [2, 45/4, f]).
step_left([2, 46/6, f], [2, 45/6, f]).
step_left([2, 46/10, f], [2, 45/10, f]).
step_left([2, 46/12, f], [2, 45/12, f]).
step_left([2, 46/16, f], [2, 45/16, f]).
step_left([2, 46/20, f], [2, 45/20, f]).
step_left([2, 46/28, f], [2, 45/28, f]).
step_left([2, 46/30, f], [2, 45/30, f]).
step_left([2, 46/32, f], [2, 45/32, f]).
step_left([2, 46/44, f], [2, 45/44, f]).
step_left([2, 46/46, f], [2, 45/46, f]).
step_left([2, 46/48, f], [2, 45/48, f]).
step_left([2, 46/50, f], [2, 45/50, f]).
step_left([2, 46/52, f], [2, 45/52, f]).
step_left([2, 46/56, f], [2, 45/56, f]).
step_left([2, 46/58, f], [2, 45/58, f]).
step_left([2, 46/62, f], [2, 45/62, f]).
step_left([2, 46/64, f], [2, 45/64, f]).
step_left([2, 46/68, f], [2, 45/68, f]).
step_left([2, 46/70, f], [2, 45/70, f]).
step_left([2, 46/74, f], [2, 45/74, f]).
step_left([2, 46/78, f], [2, 45/78, f]).
step_left([2, 46/80, f], [2, 45/80, f]).
step_left([2, 46/82, f], [2, 45/82, f]).
step_left([2, 46/88, f], [2, 45/88, f]).
step_left([2, 46/92, f], [2, 45/92, f]).
step_left([2, 46/94, f], [2, 45/94, f]).
step_left([2, 46/96, f], [2, 45/96, f]).
step_left([2, 46/100, f], [2, 45/100, f]).
step_left([2, 46/102, f], [2, 45/102, f]).
step_left([2, 46/104, f], [2, 45/104, f]).
step_left([2, 46/110, f], [2, 45/110, f]).
step_left([2, 46/118, f], [2, 45/118, f]).
step_left([2, 47/0, f], [2, 46/0, f]).
step_left([2, 47/4, f], [2, 46/4, f]).
step_left([2, 47/14, f], [2, 46/14, f]).
step_left([2, 47/18, f], [2, 46/18, f]).
step_left([2, 47/30, f], [2, 46/30, f]).
step_left([2, 47/34, f], [2, 46/34, f]).
step_left([2, 47/46, f], [2, 46/46, f]).
step_left([2, 47/48, f], [2, 46/48, f]).
step_left([2, 47/50, f], [2, 46/50, f]).
step_left([2, 47/54, f], [2, 46/54, f]).
step_left([2, 47/58, f], [2, 46/58, f]).
step_left([2, 47/60, f], [2, 46/60, f]).
step_left([2, 47/64, f], [2, 46/64, f]).
step_left([2, 47/70, f], [2, 46/70, f]).
step_left([2, 47/72, f], [2, 46/72, f]).
step_left([2, 47/74, f], [2, 46/74, f]).
step_left([2, 47/76, f], [2, 46/76, f]).
step_left([2, 47/84, f], [2, 46/84, f]).
step_left([2, 47/86, f], [2, 46/86, f]).
step_left([2, 47/90, f], [2, 46/90, f]).
step_left([2, 47/92, f], [2, 46/92, f]).
step_left([2, 47/94, f], [2, 46/94, f]).
step_left([2, 47/98, f], [2, 46/98, f]).
step_left([2, 47/106, f], [2, 46/106, f]).
step_left([2, 47/108, f], [2, 46/108, f]).
step_left([2, 47/116, f], [2, 46/116, f]).
step_left([2, 47/118, f], [2, 46/118, f]).
step_left([2, 48/0, f], [2, 47/0, f]).
step_left([2, 48/4, f], [2, 47/4, f]).
step_left([2, 48/14, f], [2, 47/14, f]).
step_left([2, 48/18, f], [2, 47/18, f]).
step_left([2, 48/30, f], [2, 47/30, f]).
step_left([2, 48/34, f], [2, 47/34, f]).
step_left([2, 48/46, f], [2, 47/46, f]).
step_left([2, 48/48, f], [2, 47/48, f]).
step_left([2, 48/50, f], [2, 47/50, f]).
step_left([2, 48/54, f], [2, 47/54, f]).
step_left([2, 48/58, f], [2, 47/58, f]).
step_left([2, 48/60, f], [2, 47/60, f]).
step_left([2, 48/64, f], [2, 47/64, f]).
step_left([2, 48/70, f], [2, 47/70, f]).
step_left([2, 48/72, f], [2, 47/72, f]).
step_left([2, 48/74, f], [2, 47/74, f]).
step_left([2, 48/76, f], [2, 47/76, f]).
step_left([2, 48/84, f], [2, 47/84, f]).
step_left([2, 48/86, f], [2, 47/86, f]).
step_left([2, 48/90, f], [2, 47/90, f]).
step_left([2, 48/92, f], [2, 47/92, f]).
step_left([2, 48/94, f], [2, 47/94, f]).
step_left([2, 48/98, f], [2, 47/98, f]).
step_left([2, 48/106, f], [2, 47/106, f]).
step_left([2, 48/108, f], [2, 47/108, f]).
step_left([2, 48/116, f], [2, 47/116, f]).
step_left([2, 48/118, f], [2, 47/118, f]).
step_left([2, 49/2, f], [2, 48/2, f]).
step_left([2, 49/10, f], [2, 48/10, f]).
step_left([2, 49/12, f], [2, 48/12, f]).
step_left([2, 49/16, f], [2, 48/16, f]).
step_left([2, 49/22, f], [2, 48/22, f]).
step_left([2, 49/24, f], [2, 48/24, f]).
step_left([2, 49/26, f], [2, 48/26, f]).
step_left([2, 49/32, f], [2, 48/32, f]).
step_left([2, 49/40, f], [2, 48/40, f]).
step_left([2, 49/42, f], [2, 48/42, f]).
step_left([2, 49/44, f], [2, 48/44, f]).
step_left([2, 49/50, f], [2, 48/50, f]).
step_left([2, 49/52, f], [2, 48/52, f]).
step_left([2, 49/56, f], [2, 48/56, f]).
step_left([2, 49/62, f], [2, 48/62, f]).
step_left([2, 49/66, f], [2, 48/66, f]).
step_left([2, 49/68, f], [2, 48/68, f]).
step_left([2, 49/72, f], [2, 48/72, f]).
step_left([2, 49/78, f], [2, 48/78, f]).
step_left([2, 49/80, f], [2, 48/80, f]).
step_left([2, 49/84, f], [2, 48/84, f]).
step_left([2, 49/88, f], [2, 48/88, f]).
step_left([2, 49/92, f], [2, 48/92, f]).
step_left([2, 49/96, f], [2, 48/96, f]).
step_left([2, 49/98, f], [2, 48/98, f]).
step_left([2, 49/100, f], [2, 48/100, f]).
step_left([2, 49/112, f], [2, 48/112, f]).
step_left([2, 49/114, f], [2, 48/114, f]).
step_left([2, 49/118, f], [2, 48/118, f]).
step_left([2, 50/2, f], [2, 49/2, f]).
step_left([2, 50/10, f], [2, 49/10, f]).
step_left([2, 50/12, f], [2, 49/12, f]).
step_left([2, 50/16, f], [2, 49/16, f]).
step_left([2, 50/22, f], [2, 49/22, f]).
step_left([2, 50/24, f], [2, 49/24, f]).
step_left([2, 50/26, f], [2, 49/26, f]).
step_left([2, 50/32, f], [2, 49/32, f]).
step_left([2, 50/40, f], [2, 49/40, f]).
step_left([2, 50/42, f], [2, 49/42, f]).
step_left([2, 50/44, f], [2, 49/44, f]).
step_left([2, 50/50, f], [2, 49/50, f]).
step_left([2, 50/52, f], [2, 49/52, f]).
step_left([2, 50/56, f], [2, 49/56, f]).
step_left([2, 50/62, f], [2, 49/62, f]).
step_left([2, 50/66, f], [2, 49/66, f]).
step_left([2, 50/68, f], [2, 49/68, f]).
step_left([2, 50/72, f], [2, 49/72, f]).
step_left([2, 50/78, f], [2, 49/78, f]).
step_left([2, 50/80, f], [2, 49/80, f]).
step_left([2, 50/84, f], [2, 49/84, f]).
step_left([2, 50/88, f], [2, 49/88, f]).
step_left([2, 50/92, f], [2, 49/92, f]).
step_left([2, 50/96, f], [2, 49/96, f]).
step_left([2, 50/98, f], [2, 49/98, f]).
step_left([2, 50/100, f], [2, 49/100, f]).
step_left([2, 50/112, f], [2, 49/112, f]).
step_left([2, 50/114, f], [2, 49/114, f]).
step_left([2, 50/118, f], [2, 49/118, f]).
step_left([2, 51/0, f], [2, 50/0, f]).
step_left([2, 51/6, f], [2, 50/6, f]).
step_left([2, 51/8, f], [2, 50/8, f]).
step_left([2, 51/14, f], [2, 50/14, f]).
step_left([2, 51/24, f], [2, 50/24, f]).
step_left([2, 51/28, f], [2, 50/28, f]).
step_left([2, 51/30, f], [2, 50/30, f]).
step_left([2, 51/34, f], [2, 50/34, f]).
step_left([2, 51/44, f], [2, 50/44, f]).
step_left([2, 51/46, f], [2, 50/46, f]).
step_left([2, 51/58, f], [2, 50/58, f]).
step_left([2, 51/60, f], [2, 50/60, f]).
step_left([2, 51/62, f], [2, 50/62, f]).
step_left([2, 51/64, f], [2, 50/64, f]).
step_left([2, 51/68, f], [2, 50/68, f]).
step_left([2, 51/70, f], [2, 50/70, f]).
step_left([2, 51/74, f], [2, 50/74, f]).
step_left([2, 51/82, f], [2, 50/82, f]).
step_left([2, 51/86, f], [2, 50/86, f]).
step_left([2, 51/88, f], [2, 50/88, f]).
step_left([2, 51/92, f], [2, 50/92, f]).
step_left([2, 51/94, f], [2, 50/94, f]).
step_left([2, 51/96, f], [2, 50/96, f]).
step_left([2, 51/102, f], [2, 50/102, f]).
step_left([2, 51/108, f], [2, 50/108, f]).
step_left([2, 51/110, f], [2, 50/110, f]).
step_left([2, 51/114, f], [2, 50/114, f]).
step_left([2, 51/116, f], [2, 50/116, f]).
step_left([2, 51/118, f], [2, 50/118, f]).
step_left([2, 52/0, f], [2, 51/0, f]).
step_left([2, 52/6, f], [2, 51/6, f]).
step_left([2, 52/8, f], [2, 51/8, f]).
step_left([2, 52/14, f], [2, 51/14, f]).
step_left([2, 52/24, f], [2, 51/24, f]).
step_left([2, 52/28, f], [2, 51/28, f]).
step_left([2, 52/30, f], [2, 51/30, f]).
step_left([2, 52/34, f], [2, 51/34, f]).
step_left([2, 52/44, f], [2, 51/44, f]).
step_left([2, 52/46, f], [2, 51/46, f]).
step_left([2, 52/58, f], [2, 51/58, f]).
step_left([2, 52/60, f], [2, 51/60, f]).
step_left([2, 52/62, f], [2, 51/62, f]).
step_left([2, 52/64, f], [2, 51/64, f]).
step_left([2, 52/68, f], [2, 51/68, f]).
step_left([2, 52/70, f], [2, 51/70, f]).
step_left([2, 52/74, f], [2, 51/74, f]).
step_left([2, 52/82, f], [2, 51/82, f]).
step_left([2, 52/86, f], [2, 51/86, f]).
step_left([2, 52/88, f], [2, 51/88, f]).
step_left([2, 52/92, f], [2, 51/92, f]).
step_left([2, 52/94, f], [2, 51/94, f]).
step_left([2, 52/96, f], [2, 51/96, f]).
step_left([2, 52/102, f], [2, 51/102, f]).
step_left([2, 52/108, f], [2, 51/108, f]).
step_left([2, 52/110, f], [2, 51/110, f]).
step_left([2, 52/114, f], [2, 51/114, f]).
step_left([2, 52/116, f], [2, 51/116, f]).
step_left([2, 52/118, f], [2, 51/118, f]).
step_left([2, 53/0, f], [2, 52/0, f]).
step_left([2, 53/2, f], [2, 52/2, f]).
step_left([2, 53/4, f], [2, 52/4, f]).
step_left([2, 53/6, f], [2, 52/6, f]).
step_left([2, 53/12, f], [2, 52/12, f]).
step_left([2, 53/16, f], [2, 52/16, f]).
step_left([2, 53/20, f], [2, 52/20, f]).
step_left([2, 53/22, f], [2, 52/22, f]).
step_left([2, 53/24, f], [2, 52/24, f]).
step_left([2, 53/26, f], [2, 52/26, f]).
step_left([2, 53/32, f], [2, 52/32, f]).
step_left([2, 53/34, f], [2, 52/34, f]).
step_left([2, 53/36, f], [2, 52/36, f]).
step_left([2, 53/40, f], [2, 52/40, f]).
step_left([2, 53/42, f], [2, 52/42, f]).
step_left([2, 53/44, f], [2, 52/44, f]).
step_left([2, 53/48, f], [2, 52/48, f]).
step_left([2, 53/50, f], [2, 52/50, f]).
step_left([2, 53/52, f], [2, 52/52, f]).
step_left([2, 53/64, f], [2, 52/64, f]).
step_left([2, 53/66, f], [2, 52/66, f]).
step_left([2, 53/76, f], [2, 52/76, f]).
step_left([2, 53/82, f], [2, 52/82, f]).
step_left([2, 53/84, f], [2, 52/84, f]).
step_left([2, 53/90, f], [2, 52/90, f]).
step_left([2, 53/94, f], [2, 52/94, f]).
step_left([2, 53/106, f], [2, 52/106, f]).
step_left([2, 53/108, f], [2, 52/108, f]).
step_left([2, 53/112, f], [2, 52/112, f]).
step_left([2, 53/116, f], [2, 52/116, f]).
step_left([2, 54/0, f], [2, 53/0, f]).
step_left([2, 54/2, f], [2, 53/2, f]).
step_left([2, 54/4, f], [2, 53/4, f]).
step_left([2, 54/6, f], [2, 53/6, f]).
step_left([2, 54/12, f], [2, 53/12, f]).
step_left([2, 54/16, f], [2, 53/16, f]).
step_left([2, 54/20, f], [2, 53/20, f]).
step_left([2, 54/22, f], [2, 53/22, f]).
step_left([2, 54/24, f], [2, 53/24, f]).
step_left([2, 54/26, f], [2, 53/26, f]).
step_left([2, 54/32, f], [2, 53/32, f]).
step_left([2, 54/34, f], [2, 53/34, f]).
step_left([2, 54/36, f], [2, 53/36, f]).
step_left([2, 54/40, f], [2, 53/40, f]).
step_left([2, 54/42, f], [2, 53/42, f]).
step_left([2, 54/44, f], [2, 53/44, f]).
step_left([2, 54/48, f], [2, 53/48, f]).
step_left([2, 54/50, f], [2, 53/50, f]).
step_left([2, 54/52, f], [2, 53/52, f]).
step_left([2, 54/64, f], [2, 53/64, f]).
step_left([2, 54/66, f], [2, 53/66, f]).
step_left([2, 54/76, f], [2, 53/76, f]).
step_left([2, 54/82, f], [2, 53/82, f]).
step_left([2, 54/84, f], [2, 53/84, f]).
step_left([2, 54/90, f], [2, 53/90, f]).
step_left([2, 54/94, f], [2, 53/94, f]).
step_left([2, 54/106, f], [2, 53/106, f]).
step_left([2, 54/108, f], [2, 53/108, f]).
step_left([2, 54/112, f], [2, 53/112, f]).
step_left([2, 54/116, f], [2, 53/116, f]).
step_left([2, 55/4, f], [2, 54/4, f]).
step_left([2, 55/6, f], [2, 54/6, f]).
step_left([2, 55/8, f], [2, 54/8, f]).
step_left([2, 55/10, f], [2, 54/10, f]).
step_left([2, 55/14, f], [2, 54/14, f]).
step_left([2, 55/18, f], [2, 54/18, f]).
step_left([2, 55/28, f], [2, 54/28, f]).
step_left([2, 55/46, f], [2, 54/46, f]).
step_left([2, 55/50, f], [2, 54/50, f]).
step_left([2, 55/54, f], [2, 54/54, f]).
step_left([2, 55/56, f], [2, 54/56, f]).
step_left([2, 55/60, f], [2, 54/60, f]).
step_left([2, 55/62, f], [2, 54/62, f]).
step_left([2, 55/70, f], [2, 54/70, f]).
step_left([2, 55/76, f], [2, 54/76, f]).
step_left([2, 55/78, f], [2, 54/78, f]).
step_left([2, 55/80, f], [2, 54/80, f]).
step_left([2, 55/84, f], [2, 54/84, f]).
step_left([2, 55/86, f], [2, 54/86, f]).
step_left([2, 55/88, f], [2, 54/88, f]).
step_left([2, 55/92, f], [2, 54/92, f]).
step_left([2, 55/100, f], [2, 54/100, f]).
step_left([2, 55/102, f], [2, 54/102, f]).
step_left([2, 55/108, f], [2, 54/108, f]).
step_left([2, 55/110, f], [2, 54/110, f]).
step_left([2, 55/114, f], [2, 54/114, f]).
step_left([2, 55/116, f], [2, 54/116, f]).
step_left([2, 55/118, f], [2, 54/118, f]).
step_left([2, 56/4, f], [2, 55/4, f]).
step_left([2, 56/6, f], [2, 55/6, f]).
step_left([2, 56/8, f], [2, 55/8, f]).
step_left([2, 56/10, f], [2, 55/10, f]).
step_left([2, 56/14, f], [2, 55/14, f]).
step_left([2, 56/18, f], [2, 55/18, f]).
step_left([2, 56/28, f], [2, 55/28, f]).
step_left([2, 56/46, f], [2, 55/46, f]).
step_left([2, 56/50, f], [2, 55/50, f]).
step_left([2, 56/54, f], [2, 55/54, f]).
step_left([2, 56/56, f], [2, 55/56, f]).
step_left([2, 56/60, f], [2, 55/60, f]).
step_left([2, 56/62, f], [2, 55/62, f]).
step_left([2, 56/70, f], [2, 55/70, f]).
step_left([2, 56/76, f], [2, 55/76, f]).
step_left([2, 56/78, f], [2, 55/78, f]).
step_left([2, 56/80, f], [2, 55/80, f]).
step_left([2, 56/84, f], [2, 55/84, f]).
step_left([2, 56/86, f], [2, 55/86, f]).
step_left([2, 56/88, f], [2, 55/88, f]).
step_left([2, 56/92, f], [2, 55/92, f]).
step_left([2, 56/100, f], [2, 55/100, f]).
step_left([2, 56/102, f], [2, 55/102, f]).
step_left([2, 56/108, f], [2, 55/108, f]).
step_left([2, 56/110, f], [2, 55/110, f]).
step_left([2, 56/114, f], [2, 55/114, f]).
step_left([2, 56/116, f], [2, 55/116, f]).
step_left([2, 56/118, f], [2, 55/118, f]).
step_left([2, 57/0, f], [2, 56/0, f]).
step_left([2, 57/2, f], [2, 56/2, f]).
step_left([2, 57/8, f], [2, 56/8, f]).
step_left([2, 57/16, f], [2, 56/16, f]).
step_left([2, 57/22, f], [2, 56/22, f]).
step_left([2, 57/26, f], [2, 56/26, f]).
step_left([2, 57/28, f], [2, 56/28, f]).
step_left([2, 57/36, f], [2, 56/36, f]).
step_left([2, 57/38, f], [2, 56/38, f]).
step_left([2, 57/40, f], [2, 56/40, f]).
step_left([2, 57/42, f], [2, 56/42, f]).
step_left([2, 57/52, f], [2, 56/52, f]).
step_left([2, 57/64, f], [2, 56/64, f]).
step_left([2, 57/66, f], [2, 56/66, f]).
step_left([2, 57/68, f], [2, 56/68, f]).
step_left([2, 57/82, f], [2, 56/82, f]).
step_left([2, 57/84, f], [2, 56/84, f]).
step_left([2, 57/86, f], [2, 56/86, f]).
step_left([2, 57/88, f], [2, 56/88, f]).
step_left([2, 57/90, f], [2, 56/90, f]).
step_left([2, 57/92, f], [2, 56/92, f]).
step_left([2, 57/94, f], [2, 56/94, f]).
step_left([2, 57/106, f], [2, 56/106, f]).
step_left([2, 57/110, f], [2, 56/110, f]).
step_left([2, 57/112, f], [2, 56/112, f]).
step_left([2, 57/118, f], [2, 56/118, f]).
step_left([2, 58/0, f], [2, 57/0, f]).
step_left([2, 58/2, f], [2, 57/2, f]).
step_left([2, 58/8, f], [2, 57/8, f]).
step_left([2, 58/16, f], [2, 57/16, f]).
step_left([2, 58/22, f], [2, 57/22, f]).
step_left([2, 58/26, f], [2, 57/26, f]).
step_left([2, 58/28, f], [2, 57/28, f]).
step_left([2, 58/36, f], [2, 57/36, f]).
step_left([2, 58/38, f], [2, 57/38, f]).
step_left([2, 58/40, f], [2, 57/40, f]).
step_left([2, 58/42, f], [2, 57/42, f]).
step_left([2, 58/52, f], [2, 57/52, f]).
step_left([2, 58/64, f], [2, 57/64, f]).
step_left([2, 58/66, f], [2, 57/66, f]).
step_left([2, 58/68, f], [2, 57/68, f]).
step_left([2, 58/82, f], [2, 57/82, f]).
step_left([2, 58/84, f], [2, 57/84, f]).
step_left([2, 58/86, f], [2, 57/86, f]).
step_left([2, 58/88, f], [2, 57/88, f]).
step_left([2, 58/90, f], [2, 57/90, f]).
step_left([2, 58/92, f], [2, 57/92, f]).
step_left([2, 58/94, f], [2, 57/94, f]).
step_left([2, 58/106, f], [2, 57/106, f]).
step_left([2, 58/110, f], [2, 57/110, f]).
step_left([2, 58/112, f], [2, 57/112, f]).
step_left([2, 58/118, f], [2, 57/118, f]).
step_left([2, 59/0, f], [2, 58/0, f]).
step_left([2, 59/4, f], [2, 58/4, f]).
step_left([2, 59/6, f], [2, 58/6, f]).
step_left([2, 59/10, f], [2, 58/10, f]).
step_left([2, 59/12, f], [2, 58/12, f]).
step_left([2, 59/24, f], [2, 58/24, f]).
step_left([2, 59/32, f], [2, 58/32, f]).
step_left([2, 59/40, f], [2, 58/40, f]).
step_left([2, 59/50, f], [2, 58/50, f]).
step_left([2, 59/52, f], [2, 58/52, f]).
step_left([2, 59/54, f], [2, 58/54, f]).
step_left([2, 59/60, f], [2, 58/60, f]).
step_left([2, 59/62, f], [2, 58/62, f]).
step_left([2, 59/70, f], [2, 58/70, f]).
step_left([2, 59/72, f], [2, 58/72, f]).
step_left([2, 59/78, f], [2, 58/78, f]).
step_left([2, 59/80, f], [2, 58/80, f]).
step_left([2, 59/86, f], [2, 58/86, f]).
step_left([2, 59/88, f], [2, 58/88, f]).
step_left([2, 59/92, f], [2, 58/92, f]).
step_left([2, 59/96, f], [2, 58/96, f]).
step_left([2, 59/98, f], [2, 58/98, f]).
step_left([2, 59/100, f], [2, 58/100, f]).
step_left([2, 59/102, f], [2, 58/102, f]).
step_left([2, 59/104, f], [2, 58/104, f]).
step_left([2, 59/108, f], [2, 58/108, f]).
step_left([2, 59/114, f], [2, 58/114, f]).
step_left([2, 59/116, f], [2, 58/116, f]).
step_left([2, 59/118, f], [2, 58/118, f]).
step_left([2, 60/0, f], [2, 59/0, f]).
step_left([2, 60/4, f], [2, 59/4, f]).
step_left([2, 60/6, f], [2, 59/6, f]).
step_left([2, 60/10, f], [2, 59/10, f]).
step_left([2, 60/12, f], [2, 59/12, f]).
step_left([2, 60/24, f], [2, 59/24, f]).
step_left([2, 60/32, f], [2, 59/32, f]).
step_left([2, 60/40, f], [2, 59/40, f]).
step_left([2, 60/50, f], [2, 59/50, f]).
step_left([2, 60/52, f], [2, 59/52, f]).
step_left([2, 60/54, f], [2, 59/54, f]).
step_left([2, 60/60, f], [2, 59/60, f]).
step_left([2, 60/62, f], [2, 59/62, f]).
step_left([2, 60/70, f], [2, 59/70, f]).
step_left([2, 60/72, f], [2, 59/72, f]).
step_left([2, 60/78, f], [2, 59/78, f]).
step_left([2, 60/80, f], [2, 59/80, f]).
step_left([2, 60/86, f], [2, 59/86, f]).
step_left([2, 60/88, f], [2, 59/88, f]).
step_left([2, 60/92, f], [2, 59/92, f]).
step_left([2, 60/96, f], [2, 59/96, f]).
step_left([2, 60/98, f], [2, 59/98, f]).
step_left([2, 60/100, f], [2, 59/100, f]).
step_left([2, 60/102, f], [2, 59/102, f]).
step_left([2, 60/104, f], [2, 59/104, f]).
step_left([2, 60/108, f], [2, 59/108, f]).
step_left([2, 60/114, f], [2, 59/114, f]).
step_left([2, 60/116, f], [2, 59/116, f]).
step_left([2, 60/118, f], [2, 59/118, f]).
step_left([2, 61/0, f], [2, 60/0, f]).
step_left([2, 61/2, f], [2, 60/2, f]).
step_left([2, 61/4, f], [2, 60/4, f]).
step_left([2, 61/8, f], [2, 60/8, f]).
step_left([2, 61/10, f], [2, 60/10, f]).
step_left([2, 61/14, f], [2, 60/14, f]).
step_left([2, 61/22, f], [2, 60/22, f]).
step_left([2, 61/28, f], [2, 60/28, f]).
step_left([2, 61/34, f], [2, 60/34, f]).
step_left([2, 61/38, f], [2, 60/38, f]).
step_left([2, 61/42, f], [2, 60/42, f]).
step_left([2, 61/46, f], [2, 60/46, f]).
step_left([2, 61/54, f], [2, 60/54, f]).
step_left([2, 61/58, f], [2, 60/58, f]).
step_left([2, 61/60, f], [2, 60/60, f]).
step_left([2, 61/64, f], [2, 60/64, f]).
step_left([2, 61/66, f], [2, 60/66, f]).
step_left([2, 61/68, f], [2, 60/68, f]).
step_left([2, 61/74, f], [2, 60/74, f]).
step_left([2, 61/80, f], [2, 60/80, f]).
step_left([2, 61/82, f], [2, 60/82, f]).
step_left([2, 61/90, f], [2, 60/90, f]).
step_left([2, 61/100, f], [2, 60/100, f]).
step_left([2, 61/106, f], [2, 60/106, f]).
step_left([2, 61/112, f], [2, 60/112, f]).
step_left([2, 61/118, f], [2, 60/118, f]).
step_left([2, 62/0, f], [2, 61/0, f]).
step_left([2, 62/2, f], [2, 61/2, f]).
step_left([2, 62/4, f], [2, 61/4, f]).
step_left([2, 62/8, f], [2, 61/8, f]).
step_left([2, 62/10, f], [2, 61/10, f]).
step_left([2, 62/14, f], [2, 61/14, f]).
step_left([2, 62/22, f], [2, 61/22, f]).
step_left([2, 62/28, f], [2, 61/28, f]).
step_left([2, 62/34, f], [2, 61/34, f]).
step_left([2, 62/38, f], [2, 61/38, f]).
step_left([2, 62/42, f], [2, 61/42, f]).
step_left([2, 62/46, f], [2, 61/46, f]).
step_left([2, 62/54, f], [2, 61/54, f]).
step_left([2, 62/58, f], [2, 61/58, f]).
step_left([2, 62/60, f], [2, 61/60, f]).
step_left([2, 62/64, f], [2, 61/64, f]).
step_left([2, 62/66, f], [2, 61/66, f]).
step_left([2, 62/68, f], [2, 61/68, f]).
step_left([2, 62/74, f], [2, 61/74, f]).
step_left([2, 62/80, f], [2, 61/80, f]).
step_left([2, 62/82, f], [2, 61/82, f]).
step_left([2, 62/90, f], [2, 61/90, f]).
step_left([2, 62/100, f], [2, 61/100, f]).
step_left([2, 62/106, f], [2, 61/106, f]).
step_left([2, 62/112, f], [2, 61/112, f]).
step_left([2, 62/118, f], [2, 61/118, f]).
step_left([2, 63/0, f], [2, 62/0, f]).
step_left([2, 63/6, f], [2, 62/6, f]).
step_left([2, 63/8, f], [2, 62/8, f]).
step_left([2, 63/10, f], [2, 62/10, f]).
step_left([2, 63/12, f], [2, 62/12, f]).
step_left([2, 63/14, f], [2, 62/14, f]).
step_left([2, 63/16, f], [2, 62/16, f]).
step_left([2, 63/18, f], [2, 62/18, f]).
step_left([2, 63/24, f], [2, 62/24, f]).
step_left([2, 63/26, f], [2, 62/26, f]).
step_left([2, 63/30, f], [2, 62/30, f]).
step_left([2, 63/32, f], [2, 62/32, f]).
step_left([2, 63/36, f], [2, 62/36, f]).
step_left([2, 63/40, f], [2, 62/40, f]).
step_left([2, 63/44, f], [2, 62/44, f]).
step_left([2, 63/46, f], [2, 62/46, f]).
step_left([2, 63/48, f], [2, 62/48, f]).
step_left([2, 63/56, f], [2, 62/56, f]).
step_left([2, 63/58, f], [2, 62/58, f]).
step_left([2, 63/60, f], [2, 62/60, f]).
step_left([2, 63/70, f], [2, 62/70, f]).
step_left([2, 63/76, f], [2, 62/76, f]).
step_left([2, 63/82, f], [2, 62/82, f]).
step_left([2, 63/84, f], [2, 62/84, f]).
step_left([2, 63/92, f], [2, 62/92, f]).
step_left([2, 63/98, f], [2, 62/98, f]).
step_left([2, 63/100, f], [2, 62/100, f]).
step_left([2, 63/102, f], [2, 62/102, f]).
step_left([2, 63/106, f], [2, 62/106, f]).
step_left([2, 63/108, f], [2, 62/108, f]).
step_left([2, 63/110, f], [2, 62/110, f]).
step_left([2, 63/114, f], [2, 62/114, f]).
step_left([2, 63/116, f], [2, 62/116, f]).
step_left([2, 63/118, f], [2, 62/118, f]).
step_left([2, 64/0, f], [2, 63/0, f]).
step_left([2, 64/6, f], [2, 63/6, f]).
step_left([2, 64/8, f], [2, 63/8, f]).
step_left([2, 64/10, f], [2, 63/10, f]).
step_left([2, 64/12, f], [2, 63/12, f]).
step_left([2, 64/14, f], [2, 63/14, f]).
step_left([2, 64/16, f], [2, 63/16, f]).
step_left([2, 64/18, f], [2, 63/18, f]).
step_left([2, 64/24, f], [2, 63/24, f]).
step_left([2, 64/26, f], [2, 63/26, f]).
step_left([2, 64/30, f], [2, 63/30, f]).
step_left([2, 64/32, f], [2, 63/32, f]).
step_left([2, 64/36, f], [2, 63/36, f]).
step_left([2, 64/40, f], [2, 63/40, f]).
step_left([2, 64/44, f], [2, 63/44, f]).
step_left([2, 64/46, f], [2, 63/46, f]).
step_left([2, 64/48, f], [2, 63/48, f]).
step_left([2, 64/56, f], [2, 63/56, f]).
step_left([2, 64/58, f], [2, 63/58, f]).
step_left([2, 64/60, f], [2, 63/60, f]).
step_left([2, 64/70, f], [2, 63/70, f]).
step_left([2, 64/76, f], [2, 63/76, f]).
step_left([2, 64/82, f], [2, 63/82, f]).
step_left([2, 64/84, f], [2, 63/84, f]).
step_left([2, 64/92, f], [2, 63/92, f]).
step_left([2, 64/98, f], [2, 63/98, f]).
step_left([2, 64/100, f], [2, 63/100, f]).
step_left([2, 64/102, f], [2, 63/102, f]).
step_left([2, 64/106, f], [2, 63/106, f]).
step_left([2, 64/108, f], [2, 63/108, f]).
step_left([2, 64/110, f], [2, 63/110, f]).
step_left([2, 64/114, f], [2, 63/114, f]).
step_left([2, 64/116, f], [2, 63/116, f]).
step_left([2, 64/118, f], [2, 63/118, f]).
step_left([2, 65/6, f], [2, 64/6, f]).
step_left([2, 65/12, f], [2, 64/12, f]).
step_left([2, 65/22, f], [2, 64/22, f]).
step_left([2, 65/28, f], [2, 64/28, f]).
step_left([2, 65/30, f], [2, 64/30, f]).
step_left([2, 65/32, f], [2, 64/32, f]).
step_left([2, 65/36, f], [2, 64/36, f]).
step_left([2, 65/38, f], [2, 64/38, f]).
step_left([2, 65/44, f], [2, 64/44, f]).
step_left([2, 65/46, f], [2, 64/46, f]).
step_left([2, 65/48, f], [2, 64/48, f]).
step_left([2, 65/50, f], [2, 64/50, f]).
step_left([2, 65/52, f], [2, 64/52, f]).
step_left([2, 65/54, f], [2, 64/54, f]).
step_left([2, 65/58, f], [2, 64/58, f]).
step_left([2, 65/68, f], [2, 64/68, f]).
step_left([2, 65/70, f], [2, 64/70, f]).
step_left([2, 65/72, f], [2, 64/72, f]).
step_left([2, 65/74, f], [2, 64/74, f]).
step_left([2, 65/76, f], [2, 64/76, f]).
step_left([2, 65/78, f], [2, 64/78, f]).
step_left([2, 65/80, f], [2, 64/80, f]).
step_left([2, 65/82, f], [2, 64/82, f]).
step_left([2, 65/84, f], [2, 64/84, f]).
step_left([2, 65/88, f], [2, 64/88, f]).
step_left([2, 65/94, f], [2, 64/94, f]).
step_left([2, 65/98, f], [2, 64/98, f]).
step_left([2, 65/104, f], [2, 64/104, f]).
step_left([2, 65/108, f], [2, 64/108, f]).
step_left([2, 65/112, f], [2, 64/112, f]).
step_left([2, 65/114, f], [2, 64/114, f]).
step_left([2, 65/116, f], [2, 64/116, f]).
step_left([2, 65/118, f], [2, 64/118, f]).
step_left([2, 66/6, f], [2, 65/6, f]).
step_left([2, 66/12, f], [2, 65/12, f]).
step_left([2, 66/22, f], [2, 65/22, f]).
step_left([2, 66/28, f], [2, 65/28, f]).
step_left([2, 66/30, f], [2, 65/30, f]).
step_left([2, 66/32, f], [2, 65/32, f]).
step_left([2, 66/36, f], [2, 65/36, f]).
step_left([2, 66/38, f], [2, 65/38, f]).
step_left([2, 66/44, f], [2, 65/44, f]).
step_left([2, 66/46, f], [2, 65/46, f]).
step_left([2, 66/48, f], [2, 65/48, f]).
step_left([2, 66/50, f], [2, 65/50, f]).
step_left([2, 66/52, f], [2, 65/52, f]).
step_left([2, 66/54, f], [2, 65/54, f]).
step_left([2, 66/58, f], [2, 65/58, f]).
step_left([2, 66/68, f], [2, 65/68, f]).
step_left([2, 66/70, f], [2, 65/70, f]).
step_left([2, 66/72, f], [2, 65/72, f]).
step_left([2, 66/74, f], [2, 65/74, f]).
step_left([2, 66/76, f], [2, 65/76, f]).
step_left([2, 66/78, f], [2, 65/78, f]).
step_left([2, 66/80, f], [2, 65/80, f]).
step_left([2, 66/82, f], [2, 65/82, f]).
step_left([2, 66/84, f], [2, 65/84, f]).
step_left([2, 66/88, f], [2, 65/88, f]).
step_left([2, 66/94, f], [2, 65/94, f]).
step_left([2, 66/98, f], [2, 65/98, f]).
step_left([2, 66/104, f], [2, 65/104, f]).
step_left([2, 66/108, f], [2, 65/108, f]).
step_left([2, 66/112, f], [2, 65/112, f]).
step_left([2, 66/114, f], [2, 65/114, f]).
step_left([2, 66/116, f], [2, 65/116, f]).
step_left([2, 66/118, f], [2, 65/118, f]).
step_left([2, 67/0, f], [2, 66/0, f]).
step_left([2, 67/4, f], [2, 66/4, f]).
step_left([2, 67/6, f], [2, 66/6, f]).
step_left([2, 67/8, f], [2, 66/8, f]).
step_left([2, 67/10, f], [2, 66/10, f]).
step_left([2, 67/14, f], [2, 66/14, f]).
step_left([2, 67/20, f], [2, 66/20, f]).
step_left([2, 67/26, f], [2, 66/26, f]).
step_left([2, 67/30, f], [2, 66/30, f]).
step_left([2, 67/32, f], [2, 66/32, f]).
step_left([2, 67/34, f], [2, 66/34, f]).
step_left([2, 67/36, f], [2, 66/36, f]).
step_left([2, 67/38, f], [2, 66/38, f]).
step_left([2, 67/40, f], [2, 66/40, f]).
step_left([2, 67/42, f], [2, 66/42, f]).
step_left([2, 67/44, f], [2, 66/44, f]).
step_left([2, 67/50, f], [2, 66/50, f]).
step_left([2, 67/60, f], [2, 66/60, f]).
step_left([2, 67/66, f], [2, 66/66, f]).
step_left([2, 67/72, f], [2, 66/72, f]).
step_left([2, 67/74, f], [2, 66/74, f]).
step_left([2, 67/86, f], [2, 66/86, f]).
step_left([2, 67/88, f], [2, 66/88, f]).
step_left([2, 67/92, f], [2, 66/92, f]).
step_left([2, 67/94, f], [2, 66/94, f]).
step_left([2, 67/96, f], [2, 66/96, f]).
step_left([2, 67/100, f], [2, 66/100, f]).
step_left([2, 67/102, f], [2, 66/102, f]).
step_left([2, 67/106, f], [2, 66/106, f]).
step_left([2, 67/108, f], [2, 66/108, f]).
step_left([2, 67/110, f], [2, 66/110, f]).
step_left([2, 67/114, f], [2, 66/114, f]).
step_left([2, 67/116, f], [2, 66/116, f]).
step_left([2, 67/118, f], [2, 66/118, f]).
step_left([2, 68/0, f], [2, 67/0, f]).
step_left([2, 68/4, f], [2, 67/4, f]).
step_left([2, 68/6, f], [2, 67/6, f]).
step_left([2, 68/8, f], [2, 67/8, f]).
step_left([2, 68/10, f], [2, 67/10, f]).
step_left([2, 68/14, f], [2, 67/14, f]).
step_left([2, 68/20, f], [2, 67/20, f]).
step_left([2, 68/26, f], [2, 67/26, f]).
step_left([2, 68/30, f], [2, 67/30, f]).
step_left([2, 68/32, f], [2, 67/32, f]).
step_left([2, 68/34, f], [2, 67/34, f]).
step_left([2, 68/36, f], [2, 67/36, f]).
step_left([2, 68/38, f], [2, 67/38, f]).
step_left([2, 68/40, f], [2, 67/40, f]).
step_left([2, 68/42, f], [2, 67/42, f]).
step_left([2, 68/44, f], [2, 67/44, f]).
step_left([2, 68/50, f], [2, 67/50, f]).
step_left([2, 68/60, f], [2, 67/60, f]).
step_left([2, 68/66, f], [2, 67/66, f]).
step_left([2, 68/72, f], [2, 67/72, f]).
step_left([2, 68/74, f], [2, 67/74, f]).
step_left([2, 68/86, f], [2, 67/86, f]).
step_left([2, 68/88, f], [2, 67/88, f]).
step_left([2, 68/92, f], [2, 67/92, f]).
step_left([2, 68/94, f], [2, 67/94, f]).
step_left([2, 68/96, f], [2, 67/96, f]).
step_left([2, 68/100, f], [2, 67/100, f]).
step_left([2, 68/102, f], [2, 67/102, f]).
step_left([2, 68/106, f], [2, 67/106, f]).
step_left([2, 68/108, f], [2, 67/108, f]).
step_left([2, 68/110, f], [2, 67/110, f]).
step_left([2, 68/114, f], [2, 67/114, f]).
step_left([2, 68/116, f], [2, 67/116, f]).
step_left([2, 68/118, f], [2, 67/118, f]).
step_left([2, 69/2, f], [2, 68/2, f]).
step_left([2, 69/6, f], [2, 68/6, f]).
step_left([2, 69/12, f], [2, 68/12, f]).
step_left([2, 69/18, f], [2, 68/18, f]).
step_left([2, 69/20, f], [2, 68/20, f]).
step_left([2, 69/22, f], [2, 68/22, f]).
step_left([2, 69/28, f], [2, 68/28, f]).
step_left([2, 69/34, f], [2, 68/34, f]).
step_left([2, 69/36, f], [2, 68/36, f]).
step_left([2, 69/46, f], [2, 68/46, f]).
step_left([2, 69/52, f], [2, 68/52, f]).
step_left([2, 69/62, f], [2, 68/62, f]).
step_left([2, 69/68, f], [2, 68/68, f]).
step_left([2, 69/84, f], [2, 68/84, f]).
step_left([2, 69/86, f], [2, 68/86, f]).
step_left([2, 69/88, f], [2, 68/88, f]).
step_left([2, 69/90, f], [2, 68/90, f]).
step_left([2, 69/92, f], [2, 68/92, f]).
step_left([2, 69/94, f], [2, 68/94, f]).
step_left([2, 69/104, f], [2, 68/104, f]).
step_left([2, 69/106, f], [2, 68/106, f]).
step_left([2, 69/108, f], [2, 68/108, f]).
step_left([2, 69/112, f], [2, 68/112, f]).
step_left([2, 69/118, f], [2, 68/118, f]).
step_left([2, 70/2, f], [2, 69/2, f]).
step_left([2, 70/6, f], [2, 69/6, f]).
step_left([2, 70/12, f], [2, 69/12, f]).
step_left([2, 70/18, f], [2, 69/18, f]).
step_left([2, 70/20, f], [2, 69/20, f]).
step_left([2, 70/22, f], [2, 69/22, f]).
step_left([2, 70/28, f], [2, 69/28, f]).
step_left([2, 70/34, f], [2, 69/34, f]).
step_left([2, 70/36, f], [2, 69/36, f]).
step_left([2, 70/46, f], [2, 69/46, f]).
step_left([2, 70/52, f], [2, 69/52, f]).
step_left([2, 70/62, f], [2, 69/62, f]).
step_left([2, 70/68, f], [2, 69/68, f]).
step_left([2, 70/84, f], [2, 69/84, f]).
step_left([2, 70/86, f], [2, 69/86, f]).
step_left([2, 70/88, f], [2, 69/88, f]).
step_left([2, 70/90, f], [2, 69/90, f]).
step_left([2, 70/92, f], [2, 69/92, f]).
step_left([2, 70/94, f], [2, 69/94, f]).
step_left([2, 70/104, f], [2, 69/104, f]).
step_left([2, 70/106, f], [2, 69/106, f]).
step_left([2, 70/108, f], [2, 69/108, f]).
step_left([2, 70/112, f], [2, 69/112, f]).
step_left([2, 70/118, f], [2, 69/118, f]).
step_left([2, 71/0, f], [2, 70/0, f]).
step_left([2, 71/4, f], [2, 70/4, f]).
step_left([2, 71/8, f], [2, 70/8, f]).
step_left([2, 71/10, f], [2, 70/10, f]).
step_left([2, 71/14, f], [2, 70/14, f]).
step_left([2, 71/16, f], [2, 70/16, f]).
step_left([2, 71/18, f], [2, 70/18, f]).
step_left([2, 71/20, f], [2, 70/20, f]).
step_left([2, 71/22, f], [2, 70/22, f]).
step_left([2, 71/24, f], [2, 70/24, f]).
step_left([2, 71/32, f], [2, 70/32, f]).
step_left([2, 71/40, f], [2, 70/40, f]).
step_left([2, 71/50, f], [2, 70/50, f]).
step_left([2, 71/56, f], [2, 70/56, f]).
step_left([2, 71/58, f], [2, 70/58, f]).
step_left([2, 71/64, f], [2, 70/64, f]).
step_left([2, 71/66, f], [2, 70/66, f]).
step_left([2, 71/68, f], [2, 70/68, f]).
step_left([2, 71/70, f], [2, 70/70, f]).
step_left([2, 71/74, f], [2, 70/74, f]).
step_left([2, 71/76, f], [2, 70/76, f]).
step_left([2, 71/82, f], [2, 70/82, f]).
step_left([2, 71/90, f], [2, 70/90, f]).
step_left([2, 71/92, f], [2, 70/92, f]).
step_left([2, 71/94, f], [2, 70/94, f]).
step_left([2, 71/98, f], [2, 70/98, f]).
step_left([2, 71/100, f], [2, 70/100, f]).
step_left([2, 71/102, f], [2, 70/102, f]).
step_left([2, 71/106, f], [2, 70/106, f]).
step_left([2, 71/108, f], [2, 70/108, f]).
step_left([2, 71/110, f], [2, 70/110, f]).
step_left([2, 71/112, f], [2, 70/112, f]).
step_left([2, 71/114, f], [2, 70/114, f]).
step_left([2, 72/0, f], [2, 71/0, f]).
step_left([2, 72/4, f], [2, 71/4, f]).
step_left([2, 72/8, f], [2, 71/8, f]).
step_left([2, 72/10, f], [2, 71/10, f]).
step_left([2, 72/14, f], [2, 71/14, f]).
step_left([2, 72/16, f], [2, 71/16, f]).
step_left([2, 72/18, f], [2, 71/18, f]).
step_left([2, 72/20, f], [2, 71/20, f]).
step_left([2, 72/22, f], [2, 71/22, f]).
step_left([2, 72/24, f], [2, 71/24, f]).
step_left([2, 72/32, f], [2, 71/32, f]).
step_left([2, 72/40, f], [2, 71/40, f]).
step_left([2, 72/50, f], [2, 71/50, f]).
step_left([2, 72/56, f], [2, 71/56, f]).
step_left([2, 72/58, f], [2, 71/58, f]).
step_left([2, 72/64, f], [2, 71/64, f]).
step_left([2, 72/66, f], [2, 71/66, f]).
step_left([2, 72/68, f], [2, 71/68, f]).
step_left([2, 72/70, f], [2, 71/70, f]).
step_left([2, 72/74, f], [2, 71/74, f]).
step_left([2, 72/76, f], [2, 71/76, f]).
step_left([2, 72/82, f], [2, 71/82, f]).
step_left([2, 72/90, f], [2, 71/90, f]).
step_left([2, 72/92, f], [2, 71/92, f]).
step_left([2, 72/94, f], [2, 71/94, f]).
step_left([2, 72/98, f], [2, 71/98, f]).
step_left([2, 72/100, f], [2, 71/100, f]).
step_left([2, 72/102, f], [2, 71/102, f]).
step_left([2, 72/106, f], [2, 71/106, f]).
step_left([2, 72/108, f], [2, 71/108, f]).
step_left([2, 72/110, f], [2, 71/110, f]).
step_left([2, 72/112, f], [2, 71/112, f]).
step_left([2, 72/114, f], [2, 71/114, f]).
step_left([2, 73/0, f], [2, 72/0, f]).
step_left([2, 73/2, f], [2, 72/2, f]).
step_left([2, 73/16, f], [2, 72/16, f]).
step_left([2, 73/18, f], [2, 72/18, f]).
step_left([2, 73/22, f], [2, 72/22, f]).
step_left([2, 73/24, f], [2, 72/24, f]).
step_left([2, 73/28, f], [2, 72/28, f]).
step_left([2, 73/36, f], [2, 72/36, f]).
step_left([2, 73/38, f], [2, 72/38, f]).
step_left([2, 73/48, f], [2, 72/48, f]).
step_left([2, 73/50, f], [2, 72/50, f]).
step_left([2, 73/52, f], [2, 72/52, f]).
step_left([2, 73/54, f], [2, 72/54, f]).
step_left([2, 73/58, f], [2, 72/58, f]).
step_left([2, 73/60, f], [2, 72/60, f]).
step_left([2, 73/66, f], [2, 72/66, f]).
step_left([2, 73/72, f], [2, 72/72, f]).
step_left([2, 73/74, f], [2, 72/74, f]).
step_left([2, 73/78, f], [2, 72/78, f]).
step_left([2, 73/80, f], [2, 72/80, f]).
step_left([2, 73/88, f], [2, 72/88, f]).
step_left([2, 73/92, f], [2, 72/92, f]).
step_left([2, 73/96, f], [2, 72/96, f]).
step_left([2, 73/102, f], [2, 72/102, f]).
step_left([2, 73/104, f], [2, 72/104, f]).
step_left([2, 73/106, f], [2, 72/106, f]).
step_left([2, 73/110, f], [2, 72/110, f]).
step_left([2, 73/118, f], [2, 72/118, f]).
step_left([2, 74/0, f], [2, 73/0, f]).
step_left([2, 74/2, f], [2, 73/2, f]).
step_left([2, 74/16, f], [2, 73/16, f]).
step_left([2, 74/18, f], [2, 73/18, f]).
step_left([2, 74/22, f], [2, 73/22, f]).
step_left([2, 74/24, f], [2, 73/24, f]).
step_left([2, 74/28, f], [2, 73/28, f]).
step_left([2, 74/36, f], [2, 73/36, f]).
step_left([2, 74/38, f], [2, 73/38, f]).
step_left([2, 74/48, f], [2, 73/48, f]).
step_left([2, 74/50, f], [2, 73/50, f]).
step_left([2, 74/52, f], [2, 73/52, f]).
step_left([2, 74/54, f], [2, 73/54, f]).
step_left([2, 74/58, f], [2, 73/58, f]).
step_left([2, 74/60, f], [2, 73/60, f]).
step_left([2, 74/66, f], [2, 73/66, f]).
step_left([2, 74/72, f], [2, 73/72, f]).
step_left([2, 74/74, f], [2, 73/74, f]).
step_left([2, 74/78, f], [2, 73/78, f]).
step_left([2, 74/80, f], [2, 73/80, f]).
step_left([2, 74/88, f], [2, 73/88, f]).
step_left([2, 74/92, f], [2, 73/92, f]).
step_left([2, 74/96, f], [2, 73/96, f]).
step_left([2, 74/102, f], [2, 73/102, f]).
step_left([2, 74/104, f], [2, 73/104, f]).
step_left([2, 74/106, f], [2, 73/106, f]).
step_left([2, 74/110, f], [2, 73/110, f]).
step_left([2, 74/118, f], [2, 73/118, f]).
step_left([2, 75/0, f], [2, 74/0, f]).
step_left([2, 75/6, f], [2, 74/6, f]).
step_left([2, 75/10, f], [2, 74/10, f]).
step_left([2, 75/12, f], [2, 74/12, f]).
step_left([2, 75/14, f], [2, 74/14, f]).
step_left([2, 75/16, f], [2, 74/16, f]).
step_left([2, 75/18, f], [2, 74/18, f]).
step_left([2, 75/20, f], [2, 74/20, f]).
step_left([2, 75/22, f], [2, 74/22, f]).
step_left([2, 75/34, f], [2, 74/34, f]).
step_left([2, 75/38, f], [2, 74/38, f]).
step_left([2, 75/40, f], [2, 74/40, f]).
step_left([2, 75/42, f], [2, 74/42, f]).
step_left([2, 75/44, f], [2, 74/44, f]).
step_left([2, 75/46, f], [2, 74/46, f]).
step_left([2, 75/52, f], [2, 74/52, f]).
step_left([2, 75/54, f], [2, 74/54, f]).
step_left([2, 75/56, f], [2, 74/56, f]).
step_left([2, 75/58, f], [2, 74/58, f]).
step_left([2, 75/62, f], [2, 74/62, f]).
step_left([2, 75/64, f], [2, 74/64, f]).
step_left([2, 75/66, f], [2, 74/66, f]).
step_left([2, 75/68, f], [2, 74/68, f]).
step_left([2, 75/76, f], [2, 74/76, f]).
step_left([2, 75/80, f], [2, 74/80, f]).
step_left([2, 75/84, f], [2, 74/84, f]).
step_left([2, 75/86, f], [2, 74/86, f]).
step_left([2, 75/90, f], [2, 74/90, f]).
step_left([2, 75/92, f], [2, 74/92, f]).
step_left([2, 75/94, f], [2, 74/94, f]).
step_left([2, 75/96, f], [2, 74/96, f]).
step_left([2, 75/108, f], [2, 74/108, f]).
step_left([2, 75/116, f], [2, 74/116, f]).
step_left([2, 75/118, f], [2, 74/118, f]).
step_left([2, 76/0, f], [2, 75/0, f]).
step_left([2, 76/6, f], [2, 75/6, f]).
step_left([2, 76/10, f], [2, 75/10, f]).
step_left([2, 76/12, f], [2, 75/12, f]).
step_left([2, 76/14, f], [2, 75/14, f]).
step_left([2, 76/16, f], [2, 75/16, f]).
step_left([2, 76/18, f], [2, 75/18, f]).
step_left([2, 76/20, f], [2, 75/20, f]).
step_left([2, 76/22, f], [2, 75/22, f]).
step_left([2, 76/34, f], [2, 75/34, f]).
step_left([2, 76/38, f], [2, 75/38, f]).
step_left([2, 76/40, f], [2, 75/40, f]).
step_left([2, 76/42, f], [2, 75/42, f]).
step_left([2, 76/44, f], [2, 75/44, f]).
step_left([2, 76/46, f], [2, 75/46, f]).
step_left([2, 76/52, f], [2, 75/52, f]).
step_left([2, 76/54, f], [2, 75/54, f]).
step_left([2, 76/56, f], [2, 75/56, f]).
step_left([2, 76/58, f], [2, 75/58, f]).
step_left([2, 76/62, f], [2, 75/62, f]).
step_left([2, 76/64, f], [2, 75/64, f]).
step_left([2, 76/66, f], [2, 75/66, f]).
step_left([2, 76/68, f], [2, 75/68, f]).
step_left([2, 76/76, f], [2, 75/76, f]).
step_left([2, 76/80, f], [2, 75/80, f]).
step_left([2, 76/84, f], [2, 75/84, f]).
step_left([2, 76/86, f], [2, 75/86, f]).
step_left([2, 76/90, f], [2, 75/90, f]).
step_left([2, 76/92, f], [2, 75/92, f]).
step_left([2, 76/94, f], [2, 75/94, f]).
step_left([2, 76/96, f], [2, 75/96, f]).
step_left([2, 76/108, f], [2, 75/108, f]).
step_left([2, 76/116, f], [2, 75/116, f]).
step_left([2, 76/118, f], [2, 75/118, f]).
step_left([2, 77/6, f], [2, 76/6, f]).
step_left([2, 77/8, f], [2, 76/8, f]).
step_left([2, 77/12, f], [2, 76/12, f]).
step_left([2, 77/16, f], [2, 76/16, f]).
step_left([2, 77/18, f], [2, 76/18, f]).
step_left([2, 77/24, f], [2, 76/24, f]).
step_left([2, 77/26, f], [2, 76/26, f]).
step_left([2, 77/28, f], [2, 76/28, f]).
step_left([2, 77/30, f], [2, 76/30, f]).
step_left([2, 77/32, f], [2, 76/32, f]).
step_left([2, 77/34, f], [2, 76/34, f]).
step_left([2, 77/36, f], [2, 76/36, f]).
step_left([2, 77/38, f], [2, 76/38, f]).
step_left([2, 77/44, f], [2, 76/44, f]).
step_left([2, 77/46, f], [2, 76/46, f]).
step_left([2, 77/52, f], [2, 76/52, f]).
step_left([2, 77/54, f], [2, 76/54, f]).
step_left([2, 77/56, f], [2, 76/56, f]).
step_left([2, 77/60, f], [2, 76/60, f]).
step_left([2, 77/62, f], [2, 76/62, f]).
step_left([2, 77/68, f], [2, 76/68, f]).
step_left([2, 77/70, f], [2, 76/70, f]).
step_left([2, 77/74, f], [2, 76/74, f]).
step_left([2, 77/76, f], [2, 76/76, f]).
step_left([2, 77/80, f], [2, 76/80, f]).
step_left([2, 77/84, f], [2, 76/84, f]).
step_left([2, 77/86, f], [2, 76/86, f]).
step_left([2, 77/88, f], [2, 76/88, f]).
step_left([2, 77/94, f], [2, 76/94, f]).
step_left([2, 77/100, f], [2, 76/100, f]).
step_left([2, 77/104, f], [2, 76/104, f]).
step_left([2, 77/110, f], [2, 76/110, f]).
step_left([2, 77/114, f], [2, 76/114, f]).
step_left([2, 78/6, f], [2, 77/6, f]).
step_left([2, 78/8, f], [2, 77/8, f]).
step_left([2, 78/12, f], [2, 77/12, f]).
step_left([2, 78/16, f], [2, 77/16, f]).
step_left([2, 78/18, f], [2, 77/18, f]).
step_left([2, 78/24, f], [2, 77/24, f]).
step_left([2, 78/26, f], [2, 77/26, f]).
step_left([2, 78/28, f], [2, 77/28, f]).
step_left([2, 78/30, f], [2, 77/30, f]).
step_left([2, 78/32, f], [2, 77/32, f]).
step_left([2, 78/34, f], [2, 77/34, f]).
step_left([2, 78/36, f], [2, 77/36, f]).
step_left([2, 78/38, f], [2, 77/38, f]).
step_left([2, 78/44, f], [2, 77/44, f]).
step_left([2, 78/46, f], [2, 77/46, f]).
step_left([2, 78/52, f], [2, 77/52, f]).
step_left([2, 78/54, f], [2, 77/54, f]).
step_left([2, 78/56, f], [2, 77/56, f]).
step_left([2, 78/60, f], [2, 77/60, f]).
step_left([2, 78/62, f], [2, 77/62, f]).
step_left([2, 78/68, f], [2, 77/68, f]).
step_left([2, 78/70, f], [2, 77/70, f]).
step_left([2, 78/74, f], [2, 77/74, f]).
step_left([2, 78/76, f], [2, 77/76, f]).
step_left([2, 78/80, f], [2, 77/80, f]).
step_left([2, 78/84, f], [2, 77/84, f]).
step_left([2, 78/86, f], [2, 77/86, f]).
step_left([2, 78/88, f], [2, 77/88, f]).
step_left([2, 78/94, f], [2, 77/94, f]).
step_left([2, 78/100, f], [2, 77/100, f]).
step_left([2, 78/104, f], [2, 77/104, f]).
step_left([2, 78/110, f], [2, 77/110, f]).
step_left([2, 78/114, f], [2, 77/114, f]).
step_left([2, 79/0, f], [2, 78/0, f]).
step_left([2, 79/6, f], [2, 78/6, f]).
step_left([2, 79/14, f], [2, 78/14, f]).
step_left([2, 79/22, f], [2, 78/22, f]).
step_left([2, 79/28, f], [2, 78/28, f]).
step_left([2, 79/36, f], [2, 78/36, f]).
step_left([2, 79/38, f], [2, 78/38, f]).
step_left([2, 79/40, f], [2, 78/40, f]).
step_left([2, 79/54, f], [2, 78/54, f]).
step_left([2, 79/60, f], [2, 78/60, f]).
step_left([2, 79/66, f], [2, 78/66, f]).
step_left([2, 79/72, f], [2, 78/72, f]).
step_left([2, 79/74, f], [2, 78/74, f]).
step_left([2, 79/76, f], [2, 78/76, f]).
step_left([2, 79/78, f], [2, 78/78, f]).
step_left([2, 79/82, f], [2, 78/82, f]).
step_left([2, 79/90, f], [2, 78/90, f]).
step_left([2, 79/92, f], [2, 78/92, f]).
step_left([2, 79/98, f], [2, 78/98, f]).
step_left([2, 79/102, f], [2, 78/102, f]).
step_left([2, 79/104, f], [2, 78/104, f]).
step_left([2, 79/106, f], [2, 78/106, f]).
step_left([2, 79/108, f], [2, 78/108, f]).
step_left([2, 79/112, f], [2, 78/112, f]).
step_left([2, 79/114, f], [2, 78/114, f]).
step_left([2, 79/118, f], [2, 78/118, f]).
step_left([2, 80/0, f], [2, 79/0, f]).
step_left([2, 80/6, f], [2, 79/6, f]).
step_left([2, 80/14, f], [2, 79/14, f]).
step_left([2, 80/22, f], [2, 79/22, f]).
step_left([2, 80/28, f], [2, 79/28, f]).
step_left([2, 80/36, f], [2, 79/36, f]).
step_left([2, 80/38, f], [2, 79/38, f]).
step_left([2, 80/40, f], [2, 79/40, f]).
step_left([2, 80/54, f], [2, 79/54, f]).
step_left([2, 80/60, f], [2, 79/60, f]).
step_left([2, 80/66, f], [2, 79/66, f]).
step_left([2, 80/72, f], [2, 79/72, f]).
step_left([2, 80/74, f], [2, 79/74, f]).
step_left([2, 80/76, f], [2, 79/76, f]).
step_left([2, 80/78, f], [2, 79/78, f]).
step_left([2, 80/82, f], [2, 79/82, f]).
step_left([2, 80/90, f], [2, 79/90, f]).
step_left([2, 80/92, f], [2, 79/92, f]).
step_left([2, 80/98, f], [2, 79/98, f]).
step_left([2, 80/102, f], [2, 79/102, f]).
step_left([2, 80/104, f], [2, 79/104, f]).
step_left([2, 80/106, f], [2, 79/106, f]).
step_left([2, 80/108, f], [2, 79/108, f]).
step_left([2, 80/112, f], [2, 79/112, f]).
step_left([2, 80/114, f], [2, 79/114, f]).
step_left([2, 80/118, f], [2, 79/118, f]).
step_left([2, 81/0, f], [2, 80/0, f]).
step_left([2, 81/8, f], [2, 80/8, f]).
step_left([2, 81/12, f], [2, 80/12, f]).
step_left([2, 81/16, f], [2, 80/16, f]).
step_left([2, 81/18, f], [2, 80/18, f]).
step_left([2, 81/26, f], [2, 80/26, f]).
step_left([2, 81/30, f], [2, 80/30, f]).
step_left([2, 81/32, f], [2, 80/32, f]).
step_left([2, 81/48, f], [2, 80/48, f]).
step_left([2, 81/50, f], [2, 80/50, f]).
step_left([2, 81/58, f], [2, 80/58, f]).
step_left([2, 81/64, f], [2, 80/64, f]).
step_left([2, 81/66, f], [2, 80/66, f]).
step_left([2, 81/72, f], [2, 80/72, f]).
step_left([2, 81/74, f], [2, 80/74, f]).
step_left([2, 81/78, f], [2, 80/78, f]).
step_left([2, 81/80, f], [2, 80/80, f]).
step_left([2, 81/82, f], [2, 80/82, f]).
step_left([2, 81/92, f], [2, 80/92, f]).
step_left([2, 81/96, f], [2, 80/96, f]).
step_left([2, 81/98, f], [2, 80/98, f]).
step_left([2, 81/100, f], [2, 80/100, f]).
step_left([2, 81/104, f], [2, 80/104, f]).
step_left([2, 81/108, f], [2, 80/108, f]).
step_left([2, 81/110, f], [2, 80/110, f]).
step_left([2, 81/118, f], [2, 80/118, f]).
step_left([2, 82/0, f], [2, 81/0, f]).
step_left([2, 82/8, f], [2, 81/8, f]).
step_left([2, 82/12, f], [2, 81/12, f]).
step_left([2, 82/16, f], [2, 81/16, f]).
step_left([2, 82/18, f], [2, 81/18, f]).
step_left([2, 82/26, f], [2, 81/26, f]).
step_left([2, 82/30, f], [2, 81/30, f]).
step_left([2, 82/32, f], [2, 81/32, f]).
step_left([2, 82/48, f], [2, 81/48, f]).
step_left([2, 82/50, f], [2, 81/50, f]).
step_left([2, 82/58, f], [2, 81/58, f]).
step_left([2, 82/64, f], [2, 81/64, f]).
step_left([2, 82/66, f], [2, 81/66, f]).
step_left([2, 82/72, f], [2, 81/72, f]).
step_left([2, 82/74, f], [2, 81/74, f]).
step_left([2, 82/78, f], [2, 81/78, f]).
step_left([2, 82/80, f], [2, 81/80, f]).
step_left([2, 82/82, f], [2, 81/82, f]).
step_left([2, 82/92, f], [2, 81/92, f]).
step_left([2, 82/96, f], [2, 81/96, f]).
step_left([2, 82/98, f], [2, 81/98, f]).
step_left([2, 82/100, f], [2, 81/100, f]).
step_left([2, 82/104, f], [2, 81/104, f]).
step_left([2, 82/108, f], [2, 81/108, f]).
step_left([2, 82/110, f], [2, 81/110, f]).
step_left([2, 82/118, f], [2, 81/118, f]).
step_left([2, 83/0, f], [2, 82/0, f]).
step_left([2, 83/10, f], [2, 82/10, f]).
step_left([2, 83/16, f], [2, 82/16, f]).
step_left([2, 83/20, f], [2, 82/20, f]).
step_left([2, 83/22, f], [2, 82/22, f]).
step_left([2, 83/24, f], [2, 82/24, f]).
step_left([2, 83/28, f], [2, 82/28, f]).
step_left([2, 83/30, f], [2, 82/30, f]).
step_left([2, 83/34, f], [2, 82/34, f]).
step_left([2, 83/36, f], [2, 82/36, f]).
step_left([2, 83/40, f], [2, 82/40, f]).
step_left([2, 83/42, f], [2, 82/42, f]).
step_left([2, 83/56, f], [2, 82/56, f]).
step_left([2, 83/60, f], [2, 82/60, f]).
step_left([2, 83/68, f], [2, 82/68, f]).
step_left([2, 83/70, f], [2, 82/70, f]).
step_left([2, 83/76, f], [2, 82/76, f]).
step_left([2, 83/78, f], [2, 82/78, f]).
step_left([2, 83/80, f], [2, 82/80, f]).
step_left([2, 83/82, f], [2, 82/82, f]).
step_left([2, 83/84, f], [2, 82/84, f]).
step_left([2, 83/88, f], [2, 82/88, f]).
step_left([2, 83/94, f], [2, 82/94, f]).
step_left([2, 83/96, f], [2, 82/96, f]).
step_left([2, 83/98, f], [2, 82/98, f]).
step_left([2, 83/100, f], [2, 82/100, f]).
step_left([2, 83/102, f], [2, 82/102, f]).
step_left([2, 83/106, f], [2, 82/106, f]).
step_left([2, 83/110, f], [2, 82/110, f]).
step_left([2, 83/112, f], [2, 82/112, f]).
step_left([2, 83/118, f], [2, 82/118, f]).
step_left([2, 84/0, f], [2, 83/0, f]).
step_left([2, 84/10, f], [2, 83/10, f]).
step_left([2, 84/16, f], [2, 83/16, f]).
step_left([2, 84/20, f], [2, 83/20, f]).
step_left([2, 84/22, f], [2, 83/22, f]).
step_left([2, 84/24, f], [2, 83/24, f]).
step_left([2, 84/28, f], [2, 83/28, f]).
step_left([2, 84/30, f], [2, 83/30, f]).
step_left([2, 84/34, f], [2, 83/34, f]).
step_left([2, 84/36, f], [2, 83/36, f]).
step_left([2, 84/40, f], [2, 83/40, f]).
step_left([2, 84/42, f], [2, 83/42, f]).
step_left([2, 84/56, f], [2, 83/56, f]).
step_left([2, 84/60, f], [2, 83/60, f]).
step_left([2, 84/68, f], [2, 83/68, f]).
step_left([2, 84/70, f], [2, 83/70, f]).
step_left([2, 84/76, f], [2, 83/76, f]).
step_left([2, 84/78, f], [2, 83/78, f]).
step_left([2, 84/80, f], [2, 83/80, f]).
step_left([2, 84/82, f], [2, 83/82, f]).
step_left([2, 84/84, f], [2, 83/84, f]).
step_left([2, 84/88, f], [2, 83/88, f]).
step_left([2, 84/94, f], [2, 83/94, f]).
step_left([2, 84/96, f], [2, 83/96, f]).
step_left([2, 84/98, f], [2, 83/98, f]).
step_left([2, 84/100, f], [2, 83/100, f]).
step_left([2, 84/102, f], [2, 83/102, f]).
step_left([2, 84/106, f], [2, 83/106, f]).
step_left([2, 84/110, f], [2, 83/110, f]).
step_left([2, 84/112, f], [2, 83/112, f]).
step_left([2, 84/118, f], [2, 83/118, f]).
step_left([2, 85/0, f], [2, 84/0, f]).
step_left([2, 85/6, f], [2, 84/6, f]).
step_left([2, 85/14, f], [2, 84/14, f]).
step_left([2, 85/18, f], [2, 84/18, f]).
step_left([2, 85/20, f], [2, 84/20, f]).
step_left([2, 85/22, f], [2, 84/22, f]).
step_left([2, 85/24, f], [2, 84/24, f]).
step_left([2, 85/26, f], [2, 84/26, f]).
step_left([2, 85/32, f], [2, 84/32, f]).
step_left([2, 85/34, f], [2, 84/34, f]).
step_left([2, 85/36, f], [2, 84/36, f]).
step_left([2, 85/38, f], [2, 84/38, f]).
step_left([2, 85/40, f], [2, 84/40, f]).
step_left([2, 85/46, f], [2, 84/46, f]).
step_left([2, 85/48, f], [2, 84/48, f]).
step_left([2, 85/52, f], [2, 84/52, f]).
step_left([2, 85/54, f], [2, 84/54, f]).
step_left([2, 85/58, f], [2, 84/58, f]).
step_left([2, 85/72, f], [2, 84/72, f]).
step_left([2, 85/76, f], [2, 84/76, f]).
step_left([2, 85/78, f], [2, 84/78, f]).
step_left([2, 85/80, f], [2, 84/80, f]).
step_left([2, 85/82, f], [2, 84/82, f]).
step_left([2, 85/92, f], [2, 84/92, f]).
step_left([2, 85/94, f], [2, 84/94, f]).
step_left([2, 85/96, f], [2, 84/96, f]).
step_left([2, 85/104, f], [2, 84/104, f]).
step_left([2, 85/116, f], [2, 84/116, f]).
step_left([2, 86/0, f], [2, 85/0, f]).
step_left([2, 86/6, f], [2, 85/6, f]).
step_left([2, 86/14, f], [2, 85/14, f]).
step_left([2, 86/18, f], [2, 85/18, f]).
step_left([2, 86/20, f], [2, 85/20, f]).
step_left([2, 86/22, f], [2, 85/22, f]).
step_left([2, 86/24, f], [2, 85/24, f]).
step_left([2, 86/26, f], [2, 85/26, f]).
step_left([2, 86/32, f], [2, 85/32, f]).
step_left([2, 86/34, f], [2, 85/34, f]).
step_left([2, 86/36, f], [2, 85/36, f]).
step_left([2, 86/38, f], [2, 85/38, f]).
step_left([2, 86/40, f], [2, 85/40, f]).
step_left([2, 86/46, f], [2, 85/46, f]).
step_left([2, 86/48, f], [2, 85/48, f]).
step_left([2, 86/52, f], [2, 85/52, f]).
step_left([2, 86/54, f], [2, 85/54, f]).
step_left([2, 86/58, f], [2, 85/58, f]).
step_left([2, 86/72, f], [2, 85/72, f]).
step_left([2, 86/76, f], [2, 85/76, f]).
step_left([2, 86/78, f], [2, 85/78, f]).
step_left([2, 86/80, f], [2, 85/80, f]).
step_left([2, 86/82, f], [2, 85/82, f]).
step_left([2, 86/92, f], [2, 85/92, f]).
step_left([2, 86/94, f], [2, 85/94, f]).
step_left([2, 86/96, f], [2, 85/96, f]).
step_left([2, 86/104, f], [2, 85/104, f]).
step_left([2, 86/116, f], [2, 85/116, f]).
step_left([2, 87/4, f], [2, 86/4, f]).
step_left([2, 87/8, f], [2, 86/8, f]).
step_left([2, 87/14, f], [2, 86/14, f]).
step_left([2, 87/20, f], [2, 86/20, f]).
step_left([2, 87/22, f], [2, 86/22, f]).
step_left([2, 87/28, f], [2, 86/28, f]).
step_left([2, 87/38, f], [2, 86/38, f]).
step_left([2, 87/40, f], [2, 86/40, f]).
step_left([2, 87/44, f], [2, 86/44, f]).
step_left([2, 87/48, f], [2, 86/48, f]).
step_left([2, 87/50, f], [2, 86/50, f]).
step_left([2, 87/52, f], [2, 86/52, f]).
step_left([2, 87/54, f], [2, 86/54, f]).
step_left([2, 87/56, f], [2, 86/56, f]).
step_left([2, 87/60, f], [2, 86/60, f]).
step_left([2, 87/62, f], [2, 86/62, f]).
step_left([2, 87/68, f], [2, 86/68, f]).
step_left([2, 87/74, f], [2, 86/74, f]).
step_left([2, 87/76, f], [2, 86/76, f]).
step_left([2, 87/80, f], [2, 86/80, f]).
step_left([2, 87/84, f], [2, 86/84, f]).
step_left([2, 87/88, f], [2, 86/88, f]).
step_left([2, 87/96, f], [2, 86/96, f]).
step_left([2, 87/110, f], [2, 86/110, f]).
step_left([2, 87/114, f], [2, 86/114, f]).
step_left([2, 87/116, f], [2, 86/116, f]).
step_left([2, 87/118, f], [2, 86/118, f]).
step_left([2, 88/4, f], [2, 87/4, f]).
step_left([2, 88/8, f], [2, 87/8, f]).
step_left([2, 88/14, f], [2, 87/14, f]).
step_left([2, 88/20, f], [2, 87/20, f]).
step_left([2, 88/22, f], [2, 87/22, f]).
step_left([2, 88/28, f], [2, 87/28, f]).
step_left([2, 88/38, f], [2, 87/38, f]).
step_left([2, 88/40, f], [2, 87/40, f]).
step_left([2, 88/44, f], [2, 87/44, f]).
step_left([2, 88/48, f], [2, 87/48, f]).
step_left([2, 88/50, f], [2, 87/50, f]).
step_left([2, 88/52, f], [2, 87/52, f]).
step_left([2, 88/54, f], [2, 87/54, f]).
step_left([2, 88/56, f], [2, 87/56, f]).
step_left([2, 88/60, f], [2, 87/60, f]).
step_left([2, 88/62, f], [2, 87/62, f]).
step_left([2, 88/68, f], [2, 87/68, f]).
step_left([2, 88/74, f], [2, 87/74, f]).
step_left([2, 88/76, f], [2, 87/76, f]).
step_left([2, 88/80, f], [2, 87/80, f]).
step_left([2, 88/84, f], [2, 87/84, f]).
step_left([2, 88/88, f], [2, 87/88, f]).
step_left([2, 88/96, f], [2, 87/96, f]).
step_left([2, 88/110, f], [2, 87/110, f]).
step_left([2, 88/114, f], [2, 87/114, f]).
step_left([2, 88/116, f], [2, 87/116, f]).
step_left([2, 88/118, f], [2, 87/118, f]).
step_left([2, 89/0, f], [2, 88/0, f]).
step_left([2, 89/6, f], [2, 88/6, f]).
step_left([2, 89/8, f], [2, 88/8, f]).
step_left([2, 89/12, f], [2, 88/12, f]).
step_left([2, 89/16, f], [2, 88/16, f]).
step_left([2, 89/20, f], [2, 88/20, f]).
step_left([2, 89/32, f], [2, 88/32, f]).
step_left([2, 89/36, f], [2, 88/36, f]).
step_left([2, 89/38, f], [2, 88/38, f]).
step_left([2, 89/42, f], [2, 88/42, f]).
step_left([2, 89/48, f], [2, 88/48, f]).
step_left([2, 89/58, f], [2, 88/58, f]).
step_left([2, 89/72, f], [2, 88/72, f]).
step_left([2, 89/74, f], [2, 88/74, f]).
step_left([2, 89/76, f], [2, 88/76, f]).
step_left([2, 89/78, f], [2, 88/78, f]).
step_left([2, 89/82, f], [2, 88/82, f]).
step_left([2, 89/86, f], [2, 88/86, f]).
step_left([2, 89/90, f], [2, 88/90, f]).
step_left([2, 89/92, f], [2, 88/92, f]).
step_left([2, 89/96, f], [2, 88/96, f]).
step_left([2, 89/98, f], [2, 88/98, f]).
step_left([2, 89/104, f], [2, 88/104, f]).
step_left([2, 89/106, f], [2, 88/106, f]).
step_left([2, 89/112, f], [2, 88/112, f]).
step_left([2, 89/114, f], [2, 88/114, f]).
step_left([2, 89/116, f], [2, 88/116, f]).
step_left([2, 89/118, f], [2, 88/118, f]).
step_left([2, 90/0, f], [2, 89/0, f]).
step_left([2, 90/6, f], [2, 89/6, f]).
step_left([2, 90/8, f], [2, 89/8, f]).
step_left([2, 90/12, f], [2, 89/12, f]).
step_left([2, 90/16, f], [2, 89/16, f]).
step_left([2, 90/20, f], [2, 89/20, f]).
step_left([2, 90/32, f], [2, 89/32, f]).
step_left([2, 90/36, f], [2, 89/36, f]).
step_left([2, 90/38, f], [2, 89/38, f]).
step_left([2, 90/42, f], [2, 89/42, f]).
step_left([2, 90/48, f], [2, 89/48, f]).
step_left([2, 90/58, f], [2, 89/58, f]).
step_left([2, 90/72, f], [2, 89/72, f]).
step_left([2, 90/74, f], [2, 89/74, f]).
step_left([2, 90/76, f], [2, 89/76, f]).
step_left([2, 90/78, f], [2, 89/78, f]).
step_left([2, 90/82, f], [2, 89/82, f]).
step_left([2, 90/86, f], [2, 89/86, f]).
step_left([2, 90/90, f], [2, 89/90, f]).
step_left([2, 90/92, f], [2, 89/92, f]).
step_left([2, 90/96, f], [2, 89/96, f]).
step_left([2, 90/98, f], [2, 89/98, f]).
step_left([2, 90/104, f], [2, 89/104, f]).
step_left([2, 90/106, f], [2, 89/106, f]).
step_left([2, 90/112, f], [2, 89/112, f]).
step_left([2, 90/114, f], [2, 89/114, f]).
step_left([2, 90/116, f], [2, 89/116, f]).
step_left([2, 90/118, f], [2, 89/118, f]).
step_left([2, 91/0, f], [2, 90/0, f]).
step_left([2, 91/2, f], [2, 90/2, f]).
step_left([2, 91/12, f], [2, 90/12, f]).
step_left([2, 91/18, f], [2, 90/18, f]).
step_left([2, 91/24, f], [2, 90/24, f]).
step_left([2, 91/26, f], [2, 90/26, f]).
step_left([2, 91/28, f], [2, 90/28, f]).
step_left([2, 91/30, f], [2, 90/30, f]).
step_left([2, 91/34, f], [2, 90/34, f]).
step_left([2, 91/40, f], [2, 90/40, f]).
step_left([2, 91/44, f], [2, 90/44, f]).
step_left([2, 91/46, f], [2, 90/46, f]).
step_left([2, 91/50, f], [2, 90/50, f]).
step_left([2, 91/52, f], [2, 90/52, f]).
step_left([2, 91/56, f], [2, 90/56, f]).
step_left([2, 91/58, f], [2, 90/58, f]).
step_left([2, 91/60, f], [2, 90/60, f]).
step_left([2, 91/66, f], [2, 90/66, f]).
step_left([2, 91/68, f], [2, 90/68, f]).
step_left([2, 91/70, f], [2, 90/70, f]).
step_left([2, 91/72, f], [2, 90/72, f]).
step_left([2, 91/78, f], [2, 90/78, f]).
step_left([2, 91/80, f], [2, 90/80, f]).
step_left([2, 91/84, f], [2, 90/84, f]).
step_left([2, 91/88, f], [2, 90/88, f]).
step_left([2, 91/90, f], [2, 90/90, f]).
step_left([2, 91/104, f], [2, 90/104, f]).
step_left([2, 91/112, f], [2, 90/112, f]).
step_left([2, 91/118, f], [2, 90/118, f]).
step_left([2, 92/0, f], [2, 91/0, f]).
step_left([2, 92/2, f], [2, 91/2, f]).
step_left([2, 92/12, f], [2, 91/12, f]).
step_left([2, 92/18, f], [2, 91/18, f]).
step_left([2, 92/24, f], [2, 91/24, f]).
step_left([2, 92/26, f], [2, 91/26, f]).
step_left([2, 92/28, f], [2, 91/28, f]).
step_left([2, 92/30, f], [2, 91/30, f]).
step_left([2, 92/34, f], [2, 91/34, f]).
step_left([2, 92/40, f], [2, 91/40, f]).
step_left([2, 92/44, f], [2, 91/44, f]).
step_left([2, 92/46, f], [2, 91/46, f]).
step_left([2, 92/50, f], [2, 91/50, f]).
step_left([2, 92/52, f], [2, 91/52, f]).
step_left([2, 92/56, f], [2, 91/56, f]).
step_left([2, 92/58, f], [2, 91/58, f]).
step_left([2, 92/60, f], [2, 91/60, f]).
step_left([2, 92/66, f], [2, 91/66, f]).
step_left([2, 92/68, f], [2, 91/68, f]).
step_left([2, 92/70, f], [2, 91/70, f]).
step_left([2, 92/72, f], [2, 91/72, f]).
step_left([2, 92/78, f], [2, 91/78, f]).
step_left([2, 92/80, f], [2, 91/80, f]).
step_left([2, 92/84, f], [2, 91/84, f]).
step_left([2, 92/88, f], [2, 91/88, f]).
step_left([2, 92/90, f], [2, 91/90, f]).
step_left([2, 92/104, f], [2, 91/104, f]).
step_left([2, 92/112, f], [2, 91/112, f]).
step_left([2, 92/118, f], [2, 91/118, f]).
step_left([2, 93/0, f], [2, 92/0, f]).
step_left([2, 93/4, f], [2, 92/4, f]).
step_left([2, 93/10, f], [2, 92/10, f]).
step_left([2, 93/14, f], [2, 92/14, f]).
step_left([2, 93/18, f], [2, 92/18, f]).
step_left([2, 93/20, f], [2, 92/20, f]).
step_left([2, 93/30, f], [2, 92/30, f]).
step_left([2, 93/32, f], [2, 92/32, f]).
step_left([2, 93/36, f], [2, 92/36, f]).
step_left([2, 93/38, f], [2, 92/38, f]).
step_left([2, 93/42, f], [2, 92/42, f]).
step_left([2, 93/44, f], [2, 92/44, f]).
step_left([2, 93/52, f], [2, 92/52, f]).
step_left([2, 93/54, f], [2, 92/54, f]).
step_left([2, 93/62, f], [2, 92/62, f]).
step_left([2, 93/72, f], [2, 92/72, f]).
step_left([2, 93/78, f], [2, 92/78, f]).
step_left([2, 93/80, f], [2, 92/80, f]).
step_left([2, 93/86, f], [2, 92/86, f]).
step_left([2, 93/94, f], [2, 92/94, f]).
step_left([2, 93/96, f], [2, 92/96, f]).
step_left([2, 93/98, f], [2, 92/98, f]).
step_left([2, 93/100, f], [2, 92/100, f]).
step_left([2, 93/102, f], [2, 92/102, f]).
step_left([2, 93/106, f], [2, 92/106, f]).
step_left([2, 93/110, f], [2, 92/110, f]).
step_left([2, 93/118, f], [2, 92/118, f]).
step_left([2, 94/0, f], [2, 93/0, f]).
step_left([2, 94/4, f], [2, 93/4, f]).
step_left([2, 94/10, f], [2, 93/10, f]).
step_left([2, 94/14, f], [2, 93/14, f]).
step_left([2, 94/18, f], [2, 93/18, f]).
step_left([2, 94/20, f], [2, 93/20, f]).
step_left([2, 94/30, f], [2, 93/30, f]).
step_left([2, 94/32, f], [2, 93/32, f]).
step_left([2, 94/36, f], [2, 93/36, f]).
step_left([2, 94/38, f], [2, 93/38, f]).
step_left([2, 94/42, f], [2, 93/42, f]).
step_left([2, 94/44, f], [2, 93/44, f]).
step_left([2, 94/52, f], [2, 93/52, f]).
step_left([2, 94/54, f], [2, 93/54, f]).
step_left([2, 94/62, f], [2, 93/62, f]).
step_left([2, 94/72, f], [2, 93/72, f]).
step_left([2, 94/78, f], [2, 93/78, f]).
step_left([2, 94/80, f], [2, 93/80, f]).
step_left([2, 94/86, f], [2, 93/86, f]).
step_left([2, 94/94, f], [2, 93/94, f]).
step_left([2, 94/96, f], [2, 93/96, f]).
step_left([2, 94/98, f], [2, 93/98, f]).
step_left([2, 94/100, f], [2, 93/100, f]).
step_left([2, 94/102, f], [2, 93/102, f]).
step_left([2, 94/106, f], [2, 93/106, f]).
step_left([2, 94/110, f], [2, 93/110, f]).
step_left([2, 94/118, f], [2, 93/118, f]).
step_left([2, 95/6, f], [2, 94/6, f]).
step_left([2, 95/8, f], [2, 94/8, f]).
step_left([2, 95/10, f], [2, 94/10, f]).
step_left([2, 95/12, f], [2, 94/12, f]).
step_left([2, 95/16, f], [2, 94/16, f]).
step_left([2, 95/20, f], [2, 94/20, f]).
step_left([2, 95/22, f], [2, 94/22, f]).
step_left([2, 95/24, f], [2, 94/24, f]).
step_left([2, 95/34, f], [2, 94/34, f]).
step_left([2, 95/36, f], [2, 94/36, f]).
step_left([2, 95/46, f], [2, 94/46, f]).
step_left([2, 95/54, f], [2, 94/54, f]).
step_left([2, 95/56, f], [2, 94/56, f]).
step_left([2, 95/60, f], [2, 94/60, f]).
step_left([2, 95/62, f], [2, 94/62, f]).
step_left([2, 95/68, f], [2, 94/68, f]).
step_left([2, 95/70, f], [2, 94/70, f]).
step_left([2, 95/74, f], [2, 94/74, f]).
step_left([2, 95/76, f], [2, 94/76, f]).
step_left([2, 95/78, f], [2, 94/78, f]).
step_left([2, 95/82, f], [2, 94/82, f]).
step_left([2, 95/84, f], [2, 94/84, f]).
step_left([2, 95/88, f], [2, 94/88, f]).
step_left([2, 95/94, f], [2, 94/94, f]).
step_left([2, 95/96, f], [2, 94/96, f]).
step_left([2, 95/104, f], [2, 94/104, f]).
step_left([2, 95/106, f], [2, 94/106, f]).
step_left([2, 95/108, f], [2, 94/108, f]).
step_left([2, 95/112, f], [2, 94/112, f]).
step_left([2, 95/116, f], [2, 94/116, f]).
step_left([2, 95/118, f], [2, 94/118, f]).
step_left([2, 96/6, f], [2, 95/6, f]).
step_left([2, 96/8, f], [2, 95/8, f]).
step_left([2, 96/10, f], [2, 95/10, f]).
step_left([2, 96/12, f], [2, 95/12, f]).
step_left([2, 96/16, f], [2, 95/16, f]).
step_left([2, 96/20, f], [2, 95/20, f]).
step_left([2, 96/22, f], [2, 95/22, f]).
step_left([2, 96/24, f], [2, 95/24, f]).
step_left([2, 96/34, f], [2, 95/34, f]).
step_left([2, 96/36, f], [2, 95/36, f]).
step_left([2, 96/46, f], [2, 95/46, f]).
step_left([2, 96/54, f], [2, 95/54, f]).
step_left([2, 96/56, f], [2, 95/56, f]).
step_left([2, 96/60, f], [2, 95/60, f]).
step_left([2, 96/62, f], [2, 95/62, f]).
step_left([2, 96/68, f], [2, 95/68, f]).
step_left([2, 96/70, f], [2, 95/70, f]).
step_left([2, 96/74, f], [2, 95/74, f]).
step_left([2, 96/76, f], [2, 95/76, f]).
step_left([2, 96/78, f], [2, 95/78, f]).
step_left([2, 96/82, f], [2, 95/82, f]).
step_left([2, 96/84, f], [2, 95/84, f]).
step_left([2, 96/88, f], [2, 95/88, f]).
step_left([2, 96/94, f], [2, 95/94, f]).
step_left([2, 96/96, f], [2, 95/96, f]).
step_left([2, 96/104, f], [2, 95/104, f]).
step_left([2, 96/106, f], [2, 95/106, f]).
step_left([2, 96/108, f], [2, 95/108, f]).
step_left([2, 96/112, f], [2, 95/112, f]).
step_left([2, 96/116, f], [2, 95/116, f]).
step_left([2, 96/118, f], [2, 95/118, f]).
step_left([2, 97/0, f], [2, 96/0, f]).
step_left([2, 97/2, f], [2, 96/2, f]).
step_left([2, 97/6, f], [2, 96/6, f]).
step_left([2, 97/14, f], [2, 96/14, f]).
step_left([2, 97/16, f], [2, 96/16, f]).
step_left([2, 97/18, f], [2, 96/18, f]).
step_left([2, 97/22, f], [2, 96/22, f]).
step_left([2, 97/24, f], [2, 96/24, f]).
step_left([2, 97/26, f], [2, 96/26, f]).
step_left([2, 97/32, f], [2, 96/32, f]).
step_left([2, 97/34, f], [2, 96/34, f]).
step_left([2, 97/40, f], [2, 96/40, f]).
step_left([2, 97/42, f], [2, 96/42, f]).
step_left([2, 97/48, f], [2, 96/48, f]).
step_left([2, 97/50, f], [2, 96/50, f]).
step_left([2, 97/52, f], [2, 96/52, f]).
step_left([2, 97/56, f], [2, 96/56, f]).
step_left([2, 97/58, f], [2, 96/58, f]).
step_left([2, 97/62, f], [2, 96/62, f]).
step_left([2, 97/64, f], [2, 96/64, f]).
step_left([2, 97/66, f], [2, 96/66, f]).
step_left([2, 97/70, f], [2, 96/70, f]).
step_left([2, 97/72, f], [2, 96/72, f]).
step_left([2, 97/76, f], [2, 96/76, f]).
step_left([2, 97/78, f], [2, 96/78, f]).
step_left([2, 97/80, f], [2, 96/80, f]).
step_left([2, 97/84, f], [2, 96/84, f]).
step_left([2, 97/86, f], [2, 96/86, f]).
step_left([2, 97/98, f], [2, 96/98, f]).
step_left([2, 97/100, f], [2, 96/100, f]).
step_left([2, 97/108, f], [2, 96/108, f]).
step_left([2, 97/114, f], [2, 96/114, f]).
step_left([2, 97/116, f], [2, 96/116, f]).
step_left([2, 97/118, f], [2, 96/118, f]).
step_left([2, 98/0, f], [2, 97/0, f]).
step_left([2, 98/2, f], [2, 97/2, f]).
step_left([2, 98/6, f], [2, 97/6, f]).
step_left([2, 98/14, f], [2, 97/14, f]).
step_left([2, 98/16, f], [2, 97/16, f]).
step_left([2, 98/18, f], [2, 97/18, f]).
step_left([2, 98/22, f], [2, 97/22, f]).
step_left([2, 98/24, f], [2, 97/24, f]).
step_left([2, 98/26, f], [2, 97/26, f]).
step_left([2, 98/32, f], [2, 97/32, f]).
step_left([2, 98/34, f], [2, 97/34, f]).
step_left([2, 98/40, f], [2, 97/40, f]).
step_left([2, 98/42, f], [2, 97/42, f]).
step_left([2, 98/48, f], [2, 97/48, f]).
step_left([2, 98/50, f], [2, 97/50, f]).
step_left([2, 98/52, f], [2, 97/52, f]).
step_left([2, 98/56, f], [2, 97/56, f]).
step_left([2, 98/58, f], [2, 97/58, f]).
step_left([2, 98/62, f], [2, 97/62, f]).
step_left([2, 98/64, f], [2, 97/64, f]).
step_left([2, 98/66, f], [2, 97/66, f]).
step_left([2, 98/70, f], [2, 97/70, f]).
step_left([2, 98/72, f], [2, 97/72, f]).
step_left([2, 98/76, f], [2, 97/76, f]).
step_left([2, 98/78, f], [2, 97/78, f]).
step_left([2, 98/80, f], [2, 97/80, f]).
step_left([2, 98/84, f], [2, 97/84, f]).
step_left([2, 98/86, f], [2, 97/86, f]).
step_left([2, 98/98, f], [2, 97/98, f]).
step_left([2, 98/100, f], [2, 97/100, f]).
step_left([2, 98/108, f], [2, 97/108, f]).
step_left([2, 98/114, f], [2, 97/114, f]).
step_left([2, 98/116, f], [2, 97/116, f]).
step_left([2, 98/118, f], [2, 97/118, f]).
step_left([2, 99/0, f], [2, 98/0, f]).
step_left([2, 99/2, f], [2, 98/2, f]).
step_left([2, 99/4, f], [2, 98/4, f]).
step_left([2, 99/10, f], [2, 98/10, f]).
step_left([2, 99/12, f], [2, 98/12, f]).
step_left([2, 99/14, f], [2, 98/14, f]).
step_left([2, 99/18, f], [2, 98/18, f]).
step_left([2, 99/20, f], [2, 98/20, f]).
step_left([2, 99/22, f], [2, 98/22, f]).
step_left([2, 99/24, f], [2, 98/24, f]).
step_left([2, 99/26, f], [2, 98/26, f]).
step_left([2, 99/28, f], [2, 98/28, f]).
step_left([2, 99/30, f], [2, 98/30, f]).
step_left([2, 99/38, f], [2, 98/38, f]).
step_left([2, 99/40, f], [2, 98/40, f]).
step_left([2, 99/42, f], [2, 98/42, f]).
step_left([2, 99/44, f], [2, 98/44, f]).
step_left([2, 99/48, f], [2, 98/48, f]).
step_left([2, 99/50, f], [2, 98/50, f]).
step_left([2, 99/54, f], [2, 98/54, f]).
step_left([2, 99/56, f], [2, 98/56, f]).
step_left([2, 99/60, f], [2, 98/60, f]).
step_left([2, 99/64, f], [2, 98/64, f]).
step_left([2, 99/66, f], [2, 98/66, f]).
step_left([2, 99/68, f], [2, 98/68, f]).
step_left([2, 99/72, f], [2, 98/72, f]).
step_left([2, 99/82, f], [2, 98/82, f]).
step_left([2, 99/84, f], [2, 98/84, f]).
step_left([2, 99/86, f], [2, 98/86, f]).
step_left([2, 99/88, f], [2, 98/88, f]).
step_left([2, 99/92, f], [2, 98/92, f]).
step_left([2, 99/94, f], [2, 98/94, f]).
step_left([2, 99/100, f], [2, 98/100, f]).
step_left([2, 99/104, f], [2, 98/104, f]).
step_left([2, 99/106, f], [2, 98/106, f]).
step_left([2, 99/108, f], [2, 98/108, f]).
step_left([2, 99/110, f], [2, 98/110, f]).
step_left([2, 99/112, f], [2, 98/112, f]).
step_left([2, 99/114, f], [2, 98/114, f]).
step_left([2, 99/118, f], [2, 98/118, f]).
step_left([2, 100/0, f], [2, 99/0, f]).
step_left([2, 100/2, f], [2, 99/2, f]).
step_left([2, 100/4, f], [2, 99/4, f]).
step_left([2, 100/10, f], [2, 99/10, f]).
step_left([2, 100/12, f], [2, 99/12, f]).
step_left([2, 100/14, f], [2, 99/14, f]).
step_left([2, 100/18, f], [2, 99/18, f]).
step_left([2, 100/20, f], [2, 99/20, f]).
step_left([2, 100/22, f], [2, 99/22, f]).
step_left([2, 100/24, f], [2, 99/24, f]).
step_left([2, 100/26, f], [2, 99/26, f]).
step_left([2, 100/28, f], [2, 99/28, f]).
step_left([2, 100/30, f], [2, 99/30, f]).
step_left([2, 100/38, f], [2, 99/38, f]).
step_left([2, 100/40, f], [2, 99/40, f]).
step_left([2, 100/42, f], [2, 99/42, f]).
step_left([2, 100/44, f], [2, 99/44, f]).
step_left([2, 100/48, f], [2, 99/48, f]).
step_left([2, 100/50, f], [2, 99/50, f]).
step_left([2, 100/54, f], [2, 99/54, f]).
step_left([2, 100/56, f], [2, 99/56, f]).
step_left([2, 100/60, f], [2, 99/60, f]).
step_left([2, 100/64, f], [2, 99/64, f]).
step_left([2, 100/66, f], [2, 99/66, f]).
step_left([2, 100/68, f], [2, 99/68, f]).
step_left([2, 100/72, f], [2, 99/72, f]).
step_left([2, 100/82, f], [2, 99/82, f]).
step_left([2, 100/84, f], [2, 99/84, f]).
step_left([2, 100/86, f], [2, 99/86, f]).
step_left([2, 100/88, f], [2, 99/88, f]).
step_left([2, 100/92, f], [2, 99/92, f]).
step_left([2, 100/94, f], [2, 99/94, f]).
step_left([2, 100/100, f], [2, 99/100, f]).
step_left([2, 100/104, f], [2, 99/104, f]).
step_left([2, 100/106, f], [2, 99/106, f]).
step_left([2, 100/108, f], [2, 99/108, f]).
step_left([2, 100/110, f], [2, 99/110, f]).
step_left([2, 100/112, f], [2, 99/112, f]).
step_left([2, 100/114, f], [2, 99/114, f]).
step_left([2, 100/118, f], [2, 99/118, f]).
step_left([2, 101/0, f], [2, 100/0, f]).
step_left([2, 101/14, f], [2, 100/14, f]).
step_left([2, 101/16, f], [2, 100/16, f]).
step_left([2, 101/22, f], [2, 100/22, f]).
step_left([2, 101/24, f], [2, 100/24, f]).
step_left([2, 101/26, f], [2, 100/26, f]).
step_left([2, 101/28, f], [2, 100/28, f]).
step_left([2, 101/36, f], [2, 100/36, f]).
step_left([2, 101/44, f], [2, 100/44, f]).
step_left([2, 101/46, f], [2, 100/46, f]).
step_left([2, 101/52, f], [2, 100/52, f]).
step_left([2, 101/54, f], [2, 100/54, f]).
step_left([2, 101/58, f], [2, 100/58, f]).
step_left([2, 101/60, f], [2, 100/60, f]).
step_left([2, 101/62, f], [2, 100/62, f]).
step_left([2, 101/70, f], [2, 100/70, f]).
step_left([2, 101/84, f], [2, 100/84, f]).
step_left([2, 101/86, f], [2, 100/86, f]).
step_left([2, 101/88, f], [2, 100/88, f]).
step_left([2, 101/90, f], [2, 100/90, f]).
step_left([2, 101/92, f], [2, 100/92, f]).
step_left([2, 101/98, f], [2, 100/98, f]).
step_left([2, 101/104, f], [2, 100/104, f]).
step_left([2, 101/106, f], [2, 100/106, f]).
step_left([2, 101/112, f], [2, 100/112, f]).
step_left([2, 101/114, f], [2, 100/114, f]).
step_left([2, 102/0, f], [2, 101/0, f]).
step_left([2, 102/14, f], [2, 101/14, f]).
step_left([2, 102/16, f], [2, 101/16, f]).
step_left([2, 102/22, f], [2, 101/22, f]).
step_left([2, 102/24, f], [2, 101/24, f]).
step_left([2, 102/26, f], [2, 101/26, f]).
step_left([2, 102/28, f], [2, 101/28, f]).
step_left([2, 102/36, f], [2, 101/36, f]).
step_left([2, 102/44, f], [2, 101/44, f]).
step_left([2, 102/46, f], [2, 101/46, f]).
step_left([2, 102/52, f], [2, 101/52, f]).
step_left([2, 102/54, f], [2, 101/54, f]).
step_left([2, 102/58, f], [2, 101/58, f]).
step_left([2, 102/60, f], [2, 101/60, f]).
step_left([2, 102/62, f], [2, 101/62, f]).
step_left([2, 102/70, f], [2, 101/70, f]).
step_left([2, 102/84, f], [2, 101/84, f]).
step_left([2, 102/86, f], [2, 101/86, f]).
step_left([2, 102/88, f], [2, 101/88, f]).
step_left([2, 102/90, f], [2, 101/90, f]).
step_left([2, 102/92, f], [2, 101/92, f]).
step_left([2, 102/98, f], [2, 101/98, f]).
step_left([2, 102/104, f], [2, 101/104, f]).
step_left([2, 102/106, f], [2, 101/106, f]).
step_left([2, 102/112, f], [2, 101/112, f]).
step_left([2, 102/114, f], [2, 101/114, f]).
step_left([2, 103/0, f], [2, 102/0, f]).
step_left([2, 103/2, f], [2, 102/2, f]).
step_left([2, 103/6, f], [2, 102/6, f]).
step_left([2, 103/14, f], [2, 102/14, f]).
step_left([2, 103/22, f], [2, 102/22, f]).
step_left([2, 103/28, f], [2, 102/28, f]).
step_left([2, 103/32, f], [2, 102/32, f]).
step_left([2, 103/40, f], [2, 102/40, f]).
step_left([2, 103/42, f], [2, 102/42, f]).
step_left([2, 103/44, f], [2, 102/44, f]).
step_left([2, 103/48, f], [2, 102/48, f]).
step_left([2, 103/50, f], [2, 102/50, f]).
step_left([2, 103/54, f], [2, 102/54, f]).
step_left([2, 103/56, f], [2, 102/56, f]).
step_left([2, 103/62, f], [2, 102/62, f]).
step_left([2, 103/64, f], [2, 102/64, f]).
step_left([2, 103/66, f], [2, 102/66, f]).
step_left([2, 103/70, f], [2, 102/70, f]).
step_left([2, 103/80, f], [2, 102/80, f]).
step_left([2, 103/82, f], [2, 102/82, f]).
step_left([2, 103/84, f], [2, 102/84, f]).
step_left([2, 103/90, f], [2, 102/90, f]).
step_left([2, 103/92, f], [2, 102/92, f]).
step_left([2, 103/94, f], [2, 102/94, f]).
step_left([2, 103/110, f], [2, 102/110, f]).
step_left([2, 103/112, f], [2, 102/112, f]).
step_left([2, 103/118, f], [2, 102/118, f]).
step_left([2, 104/0, f], [2, 103/0, f]).
step_left([2, 104/2, f], [2, 103/2, f]).
step_left([2, 104/6, f], [2, 103/6, f]).
step_left([2, 104/14, f], [2, 103/14, f]).
step_left([2, 104/22, f], [2, 103/22, f]).
step_left([2, 104/28, f], [2, 103/28, f]).
step_left([2, 104/32, f], [2, 103/32, f]).
step_left([2, 104/40, f], [2, 103/40, f]).
step_left([2, 104/42, f], [2, 103/42, f]).
step_left([2, 104/44, f], [2, 103/44, f]).
step_left([2, 104/48, f], [2, 103/48, f]).
step_left([2, 104/50, f], [2, 103/50, f]).
step_left([2, 104/54, f], [2, 103/54, f]).
step_left([2, 104/56, f], [2, 103/56, f]).
step_left([2, 104/62, f], [2, 103/62, f]).
step_left([2, 104/64, f], [2, 103/64, f]).
step_left([2, 104/66, f], [2, 103/66, f]).
step_left([2, 104/70, f], [2, 103/70, f]).
step_left([2, 104/80, f], [2, 103/80, f]).
step_left([2, 104/82, f], [2, 103/82, f]).
step_left([2, 104/84, f], [2, 103/84, f]).
step_left([2, 104/90, f], [2, 103/90, f]).
step_left([2, 104/92, f], [2, 103/92, f]).
step_left([2, 104/94, f], [2, 103/94, f]).
step_left([2, 104/110, f], [2, 103/110, f]).
step_left([2, 104/112, f], [2, 103/112, f]).
step_left([2, 104/118, f], [2, 103/118, f]).
step_left([2, 105/0, f], [2, 104/0, f]).
step_left([2, 105/4, f], [2, 104/4, f]).
step_left([2, 105/8, f], [2, 104/8, f]).
step_left([2, 105/14, f], [2, 104/14, f]).
step_left([2, 105/16, f], [2, 104/16, f]).
step_left([2, 105/18, f], [2, 104/18, f]).
step_left([2, 105/24, f], [2, 104/24, f]).
step_left([2, 105/26, f], [2, 104/26, f]).
step_left([2, 105/28, f], [2, 104/28, f]).
step_left([2, 105/30, f], [2, 104/30, f]).
step_left([2, 105/36, f], [2, 104/36, f]).
step_left([2, 105/38, f], [2, 104/38, f]).
step_left([2, 105/42, f], [2, 104/42, f]).
step_left([2, 105/46, f], [2, 104/46, f]).
step_left([2, 105/50, f], [2, 104/50, f]).
step_left([2, 105/52, f], [2, 104/52, f]).
step_left([2, 105/56, f], [2, 104/56, f]).
step_left([2, 105/58, f], [2, 104/58, f]).
step_left([2, 105/66, f], [2, 104/66, f]).
step_left([2, 105/72, f], [2, 104/72, f]).
step_left([2, 105/82, f], [2, 104/82, f]).
step_left([2, 105/84, f], [2, 104/84, f]).
step_left([2, 105/86, f], [2, 104/86, f]).
step_left([2, 105/90, f], [2, 104/90, f]).
step_left([2, 105/98, f], [2, 104/98, f]).
step_left([2, 105/100, f], [2, 104/100, f]).
step_left([2, 105/102, f], [2, 104/102, f]).
step_left([2, 105/106, f], [2, 104/106, f]).
step_left([2, 105/108, f], [2, 104/108, f]).
step_left([2, 105/116, f], [2, 104/116, f]).
step_left([2, 105/118, f], [2, 104/118, f]).
step_left([2, 106/0, f], [2, 105/0, f]).
step_left([2, 106/4, f], [2, 105/4, f]).
step_left([2, 106/8, f], [2, 105/8, f]).
step_left([2, 106/14, f], [2, 105/14, f]).
step_left([2, 106/16, f], [2, 105/16, f]).
step_left([2, 106/18, f], [2, 105/18, f]).
step_left([2, 106/24, f], [2, 105/24, f]).
step_left([2, 106/26, f], [2, 105/26, f]).
step_left([2, 106/28, f], [2, 105/28, f]).
step_left([2, 106/30, f], [2, 105/30, f]).
step_left([2, 106/36, f], [2, 105/36, f]).
step_left([2, 106/38, f], [2, 105/38, f]).
step_left([2, 106/42, f], [2, 105/42, f]).
step_left([2, 106/46, f], [2, 105/46, f]).
step_left([2, 106/50, f], [2, 105/50, f]).
step_left([2, 106/52, f], [2, 105/52, f]).
step_left([2, 106/56, f], [2, 105/56, f]).
step_left([2, 106/58, f], [2, 105/58, f]).
step_left([2, 106/66, f], [2, 105/66, f]).
step_left([2, 106/72, f], [2, 105/72, f]).
step_left([2, 106/82, f], [2, 105/82, f]).
step_left([2, 106/84, f], [2, 105/84, f]).
step_left([2, 106/86, f], [2, 105/86, f]).
step_left([2, 106/90, f], [2, 105/90, f]).
step_left([2, 106/98, f], [2, 105/98, f]).
step_left([2, 106/100, f], [2, 105/100, f]).
step_left([2, 106/102, f], [2, 105/102, f]).
step_left([2, 106/106, f], [2, 105/106, f]).
step_left([2, 106/108, f], [2, 105/108, f]).
step_left([2, 106/116, f], [2, 105/116, f]).
step_left([2, 106/118, f], [2, 105/118, f]).
step_left([2, 107/0, f], [2, 106/0, f]).
step_left([2, 107/2, f], [2, 106/2, f]).
step_left([2, 107/6, f], [2, 106/6, f]).
step_left([2, 107/12, f], [2, 106/12, f]).
step_left([2, 107/14, f], [2, 106/14, f]).
step_left([2, 107/16, f], [2, 106/16, f]).
step_left([2, 107/18, f], [2, 106/18, f]).
step_left([2, 107/26, f], [2, 106/26, f]).
step_left([2, 107/30, f], [2, 106/30, f]).
step_left([2, 107/32, f], [2, 106/32, f]).
step_left([2, 107/34, f], [2, 106/34, f]).
step_left([2, 107/36, f], [2, 106/36, f]).
step_left([2, 107/38, f], [2, 106/38, f]).
step_left([2, 107/40, f], [2, 106/40, f]).
step_left([2, 107/44, f], [2, 106/44, f]).
step_left([2, 107/46, f], [2, 106/46, f]).
step_left([2, 107/48, f], [2, 106/48, f]).
step_left([2, 107/54, f], [2, 106/54, f]).
step_left([2, 107/56, f], [2, 106/56, f]).
step_left([2, 107/58, f], [2, 106/58, f]).
step_left([2, 107/62, f], [2, 106/62, f]).
step_left([2, 107/64, f], [2, 106/64, f]).
step_left([2, 107/66, f], [2, 106/66, f]).
step_left([2, 107/68, f], [2, 106/68, f]).
step_left([2, 107/74, f], [2, 106/74, f]).
step_left([2, 107/78, f], [2, 106/78, f]).
step_left([2, 107/82, f], [2, 106/82, f]).
step_left([2, 107/86, f], [2, 106/86, f]).
step_left([2, 107/88, f], [2, 106/88, f]).
step_left([2, 107/92, f], [2, 106/92, f]).
step_left([2, 107/94, f], [2, 106/94, f]).
step_left([2, 107/96, f], [2, 106/96, f]).
step_left([2, 107/100, f], [2, 106/100, f]).
step_left([2, 107/104, f], [2, 106/104, f]).
step_left([2, 107/106, f], [2, 106/106, f]).
step_left([2, 107/116, f], [2, 106/116, f]).
step_left([2, 107/118, f], [2, 106/118, f]).
step_left([2, 108/0, f], [2, 107/0, f]).
step_left([2, 108/2, f], [2, 107/2, f]).
step_left([2, 108/6, f], [2, 107/6, f]).
step_left([2, 108/12, f], [2, 107/12, f]).
step_left([2, 108/14, f], [2, 107/14, f]).
step_left([2, 108/16, f], [2, 107/16, f]).
step_left([2, 108/18, f], [2, 107/18, f]).
step_left([2, 108/26, f], [2, 107/26, f]).
step_left([2, 108/30, f], [2, 107/30, f]).
step_left([2, 108/32, f], [2, 107/32, f]).
step_left([2, 108/34, f], [2, 107/34, f]).
step_left([2, 108/36, f], [2, 107/36, f]).
step_left([2, 108/38, f], [2, 107/38, f]).
step_left([2, 108/40, f], [2, 107/40, f]).
step_left([2, 108/44, f], [2, 107/44, f]).
step_left([2, 108/46, f], [2, 107/46, f]).
step_left([2, 108/48, f], [2, 107/48, f]).
step_left([2, 108/54, f], [2, 107/54, f]).
step_left([2, 108/56, f], [2, 107/56, f]).
step_left([2, 108/58, f], [2, 107/58, f]).
step_left([2, 108/62, f], [2, 107/62, f]).
step_left([2, 108/64, f], [2, 107/64, f]).
step_left([2, 108/66, f], [2, 107/66, f]).
step_left([2, 108/68, f], [2, 107/68, f]).
step_left([2, 108/74, f], [2, 107/74, f]).
step_left([2, 108/78, f], [2, 107/78, f]).
step_left([2, 108/82, f], [2, 107/82, f]).
step_left([2, 108/86, f], [2, 107/86, f]).
step_left([2, 108/88, f], [2, 107/88, f]).
step_left([2, 108/92, f], [2, 107/92, f]).
step_left([2, 108/94, f], [2, 107/94, f]).
step_left([2, 108/96, f], [2, 107/96, f]).
step_left([2, 108/100, f], [2, 107/100, f]).
step_left([2, 108/104, f], [2, 107/104, f]).
step_left([2, 108/106, f], [2, 107/106, f]).
step_left([2, 108/116, f], [2, 107/116, f]).
step_left([2, 108/118, f], [2, 107/118, f]).
step_left([2, 109/0, f], [2, 108/0, f]).
step_left([2, 109/2, f], [2, 108/2, f]).
step_left([2, 109/4, f], [2, 108/4, f]).
step_left([2, 109/6, f], [2, 108/6, f]).
step_left([2, 109/10, f], [2, 108/10, f]).
step_left([2, 109/12, f], [2, 108/12, f]).
step_left([2, 109/14, f], [2, 108/14, f]).
step_left([2, 109/16, f], [2, 108/16, f]).
step_left([2, 109/24, f], [2, 108/24, f]).
step_left([2, 109/26, f], [2, 108/26, f]).
step_left([2, 109/28, f], [2, 108/28, f]).
step_left([2, 109/30, f], [2, 108/30, f]).
step_left([2, 109/32, f], [2, 108/32, f]).
step_left([2, 109/38, f], [2, 108/38, f]).
step_left([2, 109/46, f], [2, 108/46, f]).
step_left([2, 109/50, f], [2, 108/50, f]).
step_left([2, 109/52, f], [2, 108/52, f]).
step_left([2, 109/64, f], [2, 108/64, f]).
step_left([2, 109/66, f], [2, 108/66, f]).
step_left([2, 109/72, f], [2, 108/72, f]).
step_left([2, 109/74, f], [2, 108/74, f]).
step_left([2, 109/76, f], [2, 108/76, f]).
step_left([2, 109/80, f], [2, 108/80, f]).
step_left([2, 109/82, f], [2, 108/82, f]).
step_left([2, 109/84, f], [2, 108/84, f]).
step_left([2, 109/88, f], [2, 108/88, f]).
step_left([2, 109/92, f], [2, 108/92, f]).
step_left([2, 109/94, f], [2, 108/94, f]).
step_left([2, 109/96, f], [2, 108/96, f]).
step_left([2, 109/98, f], [2, 108/98, f]).
step_left([2, 109/102, f], [2, 108/102, f]).
step_left([2, 109/106, f], [2, 108/106, f]).
step_left([2, 109/108, f], [2, 108/108, f]).
step_left([2, 109/118, f], [2, 108/118, f]).
step_left([2, 110/0, f], [2, 109/0, f]).
step_left([2, 110/2, f], [2, 109/2, f]).
step_left([2, 110/4, f], [2, 109/4, f]).
step_left([2, 110/6, f], [2, 109/6, f]).
step_left([2, 110/10, f], [2, 109/10, f]).
step_left([2, 110/12, f], [2, 109/12, f]).
step_left([2, 110/14, f], [2, 109/14, f]).
step_left([2, 110/16, f], [2, 109/16, f]).
step_left([2, 110/24, f], [2, 109/24, f]).
step_left([2, 110/26, f], [2, 109/26, f]).
step_left([2, 110/28, f], [2, 109/28, f]).
step_left([2, 110/30, f], [2, 109/30, f]).
step_left([2, 110/32, f], [2, 109/32, f]).
step_left([2, 110/38, f], [2, 109/38, f]).
step_left([2, 110/46, f], [2, 109/46, f]).
step_left([2, 110/50, f], [2, 109/50, f]).
step_left([2, 110/52, f], [2, 109/52, f]).
step_left([2, 110/64, f], [2, 109/64, f]).
step_left([2, 110/66, f], [2, 109/66, f]).
step_left([2, 110/72, f], [2, 109/72, f]).
step_left([2, 110/74, f], [2, 109/74, f]).
step_left([2, 110/76, f], [2, 109/76, f]).
step_left([2, 110/80, f], [2, 109/80, f]).
step_left([2, 110/82, f], [2, 109/82, f]).
step_left([2, 110/84, f], [2, 109/84, f]).
step_left([2, 110/88, f], [2, 109/88, f]).
step_left([2, 110/92, f], [2, 109/92, f]).
step_left([2, 110/94, f], [2, 109/94, f]).
step_left([2, 110/96, f], [2, 109/96, f]).
step_left([2, 110/98, f], [2, 109/98, f]).
step_left([2, 110/102, f], [2, 109/102, f]).
step_left([2, 110/106, f], [2, 109/106, f]).
step_left([2, 110/108, f], [2, 109/108, f]).
step_left([2, 110/118, f], [2, 109/118, f]).
step_left([2, 111/0, f], [2, 110/0, f]).
step_left([2, 111/2, f], [2, 110/2, f]).
step_left([2, 111/14, f], [2, 110/14, f]).
step_left([2, 111/16, f], [2, 110/16, f]).
step_left([2, 111/22, f], [2, 110/22, f]).
step_left([2, 111/24, f], [2, 110/24, f]).
step_left([2, 111/28, f], [2, 110/28, f]).
step_left([2, 111/34, f], [2, 110/34, f]).
step_left([2, 111/36, f], [2, 110/36, f]).
step_left([2, 111/40, f], [2, 110/40, f]).
step_left([2, 111/48, f], [2, 110/48, f]).
step_left([2, 111/50, f], [2, 110/50, f]).
step_left([2, 111/54, f], [2, 110/54, f]).
step_left([2, 111/66, f], [2, 110/66, f]).
step_left([2, 111/72, f], [2, 110/72, f]).
step_left([2, 111/76, f], [2, 110/76, f]).
step_left([2, 111/78, f], [2, 110/78, f]).
step_left([2, 111/86, f], [2, 110/86, f]).
step_left([2, 111/90, f], [2, 110/90, f]).
step_left([2, 111/94, f], [2, 110/94, f]).
step_left([2, 111/96, f], [2, 110/96, f]).
step_left([2, 111/98, f], [2, 110/98, f]).
step_left([2, 111/100, f], [2, 110/100, f]).
step_left([2, 111/102, f], [2, 110/102, f]).
step_left([2, 111/104, f], [2, 110/104, f]).
step_left([2, 111/108, f], [2, 110/108, f]).
step_left([2, 111/116, f], [2, 110/116, f]).
step_left([2, 111/118, f], [2, 110/118, f]).
step_left([2, 112/0, f], [2, 111/0, f]).
step_left([2, 112/2, f], [2, 111/2, f]).
step_left([2, 112/14, f], [2, 111/14, f]).
step_left([2, 112/16, f], [2, 111/16, f]).
step_left([2, 112/22, f], [2, 111/22, f]).
step_left([2, 112/24, f], [2, 111/24, f]).
step_left([2, 112/28, f], [2, 111/28, f]).
step_left([2, 112/34, f], [2, 111/34, f]).
step_left([2, 112/36, f], [2, 111/36, f]).
step_left([2, 112/40, f], [2, 111/40, f]).
step_left([2, 112/48, f], [2, 111/48, f]).
step_left([2, 112/50, f], [2, 111/50, f]).
step_left([2, 112/54, f], [2, 111/54, f]).
step_left([2, 112/66, f], [2, 111/66, f]).
step_left([2, 112/72, f], [2, 111/72, f]).
step_left([2, 112/76, f], [2, 111/76, f]).
step_left([2, 112/78, f], [2, 111/78, f]).
step_left([2, 112/86, f], [2, 111/86, f]).
step_left([2, 112/90, f], [2, 111/90, f]).
step_left([2, 112/94, f], [2, 111/94, f]).
step_left([2, 112/96, f], [2, 111/96, f]).
step_left([2, 112/98, f], [2, 111/98, f]).
step_left([2, 112/100, f], [2, 111/100, f]).
step_left([2, 112/102, f], [2, 111/102, f]).
step_left([2, 112/104, f], [2, 111/104, f]).
step_left([2, 112/108, f], [2, 111/108, f]).
step_left([2, 112/116, f], [2, 111/116, f]).
step_left([2, 112/118, f], [2, 111/118, f]).
step_left([2, 113/0, f], [2, 112/0, f]).
step_left([2, 113/8, f], [2, 112/8, f]).
step_left([2, 113/12, f], [2, 112/12, f]).
step_left([2, 113/16, f], [2, 112/16, f]).
step_left([2, 113/18, f], [2, 112/18, f]).
step_left([2, 113/20, f], [2, 112/20, f]).
step_left([2, 113/22, f], [2, 112/22, f]).
step_left([2, 113/26, f], [2, 112/26, f]).
step_left([2, 113/30, f], [2, 112/30, f]).
step_left([2, 113/32, f], [2, 112/32, f]).
step_left([2, 113/34, f], [2, 112/34, f]).
step_left([2, 113/36, f], [2, 112/36, f]).
step_left([2, 113/38, f], [2, 112/38, f]).
step_left([2, 113/44, f], [2, 112/44, f]).
step_left([2, 113/48, f], [2, 112/48, f]).
step_left([2, 113/52, f], [2, 112/52, f]).
step_left([2, 113/54, f], [2, 112/54, f]).
step_left([2, 113/56, f], [2, 112/56, f]).
step_left([2, 113/58, f], [2, 112/58, f]).
step_left([2, 113/66, f], [2, 112/66, f]).
step_left([2, 113/68, f], [2, 112/68, f]).
step_left([2, 113/70, f], [2, 112/70, f]).
step_left([2, 113/74, f], [2, 112/74, f]).
step_left([2, 113/84, f], [2, 112/84, f]).
step_left([2, 113/86, f], [2, 112/86, f]).
step_left([2, 113/88, f], [2, 112/88, f]).
step_left([2, 113/98, f], [2, 112/98, f]).
step_left([2, 113/100, f], [2, 112/100, f]).
step_left([2, 113/104, f], [2, 112/104, f]).
step_left([2, 113/106, f], [2, 112/106, f]).
step_left([2, 113/114, f], [2, 112/114, f]).
step_left([2, 113/116, f], [2, 112/116, f]).
step_left([2, 113/118, f], [2, 112/118, f]).
step_left([2, 114/0, f], [2, 113/0, f]).
step_left([2, 114/8, f], [2, 113/8, f]).
step_left([2, 114/12, f], [2, 113/12, f]).
step_left([2, 114/16, f], [2, 113/16, f]).
step_left([2, 114/18, f], [2, 113/18, f]).
step_left([2, 114/20, f], [2, 113/20, f]).
step_left([2, 114/22, f], [2, 113/22, f]).
step_left([2, 114/26, f], [2, 113/26, f]).
step_left([2, 114/30, f], [2, 113/30, f]).
step_left([2, 114/32, f], [2, 113/32, f]).
step_left([2, 114/34, f], [2, 113/34, f]).
step_left([2, 114/36, f], [2, 113/36, f]).
step_left([2, 114/38, f], [2, 113/38, f]).
step_left([2, 114/44, f], [2, 113/44, f]).
step_left([2, 114/48, f], [2, 113/48, f]).
step_left([2, 114/52, f], [2, 113/52, f]).
step_left([2, 114/54, f], [2, 113/54, f]).
step_left([2, 114/56, f], [2, 113/56, f]).
step_left([2, 114/58, f], [2, 113/58, f]).
step_left([2, 114/66, f], [2, 113/66, f]).
step_left([2, 114/68, f], [2, 113/68, f]).
step_left([2, 114/70, f], [2, 113/70, f]).
step_left([2, 114/74, f], [2, 113/74, f]).
step_left([2, 114/84, f], [2, 113/84, f]).
step_left([2, 114/86, f], [2, 113/86, f]).
step_left([2, 114/88, f], [2, 113/88, f]).
step_left([2, 114/98, f], [2, 113/98, f]).
step_left([2, 114/100, f], [2, 113/100, f]).
step_left([2, 114/104, f], [2, 113/104, f]).
step_left([2, 114/106, f], [2, 113/106, f]).
step_left([2, 114/114, f], [2, 113/114, f]).
step_left([2, 114/116, f], [2, 113/116, f]).
step_left([2, 114/118, f], [2, 113/118, f]).
step_left([2, 115/0, f], [2, 114/0, f]).
step_left([2, 115/2, f], [2, 114/2, f]).
step_left([2, 115/4, f], [2, 114/4, f]).
step_left([2, 115/6, f], [2, 114/6, f]).
step_left([2, 115/10, f], [2, 114/10, f]).
step_left([2, 115/18, f], [2, 114/18, f]).
step_left([2, 115/20, f], [2, 114/20, f]).
step_left([2, 115/26, f], [2, 114/26, f]).
step_left([2, 115/28, f], [2, 114/28, f]).
step_left([2, 115/32, f], [2, 114/32, f]).
step_left([2, 115/34, f], [2, 114/34, f]).
step_left([2, 115/40, f], [2, 114/40, f]).
step_left([2, 115/42, f], [2, 114/42, f]).
step_left([2, 115/44, f], [2, 114/44, f]).
step_left([2, 115/46, f], [2, 114/46, f]).
step_left([2, 115/50, f], [2, 114/50, f]).
step_left([2, 115/56, f], [2, 114/56, f]).
step_left([2, 115/60, f], [2, 114/60, f]).
step_left([2, 115/68, f], [2, 114/68, f]).
step_left([2, 115/70, f], [2, 114/70, f]).
step_left([2, 115/72, f], [2, 114/72, f]).
step_left([2, 115/76, f], [2, 114/76, f]).
step_left([2, 115/80, f], [2, 114/80, f]).
step_left([2, 115/82, f], [2, 114/82, f]).
step_left([2, 115/90, f], [2, 114/90, f]).
step_left([2, 115/92, f], [2, 114/92, f]).
step_left([2, 115/94, f], [2, 114/94, f]).
step_left([2, 115/102, f], [2, 114/102, f]).
step_left([2, 115/110, f], [2, 114/110, f]).
step_left([2, 115/112, f], [2, 114/112, f]).
step_left([2, 115/114, f], [2, 114/114, f]).
step_left([2, 115/118, f], [2, 114/118, f]).
step_left([2, 116/0, f], [2, 115/0, f]).
step_left([2, 116/2, f], [2, 115/2, f]).
step_left([2, 116/4, f], [2, 115/4, f]).
step_left([2, 116/6, f], [2, 115/6, f]).
step_left([2, 116/10, f], [2, 115/10, f]).
step_left([2, 116/18, f], [2, 115/18, f]).
step_left([2, 116/20, f], [2, 115/20, f]).
step_left([2, 116/26, f], [2, 115/26, f]).
step_left([2, 116/28, f], [2, 115/28, f]).
step_left([2, 116/32, f], [2, 115/32, f]).
step_left([2, 116/34, f], [2, 115/34, f]).
step_left([2, 116/40, f], [2, 115/40, f]).
step_left([2, 116/42, f], [2, 115/42, f]).
step_left([2, 116/44, f], [2, 115/44, f]).
step_left([2, 116/46, f], [2, 115/46, f]).
step_left([2, 116/50, f], [2, 115/50, f]).
step_left([2, 116/56, f], [2, 115/56, f]).
step_left([2, 116/60, f], [2, 115/60, f]).
step_left([2, 116/68, f], [2, 115/68, f]).
step_left([2, 116/70, f], [2, 115/70, f]).
step_left([2, 116/72, f], [2, 115/72, f]).
step_left([2, 116/76, f], [2, 115/76, f]).
step_left([2, 116/80, f], [2, 115/80, f]).
step_left([2, 116/82, f], [2, 115/82, f]).
step_left([2, 116/90, f], [2, 115/90, f]).
step_left([2, 116/92, f], [2, 115/92, f]).
step_left([2, 116/94, f], [2, 115/94, f]).
step_left([2, 116/102, f], [2, 115/102, f]).
step_left([2, 116/110, f], [2, 115/110, f]).
step_left([2, 116/112, f], [2, 115/112, f]).
step_left([2, 116/114, f], [2, 115/114, f]).
step_left([2, 116/118, f], [2, 115/118, f]).
step_left([2, 117/2, f], [2, 116/2, f]).
step_left([2, 117/4, f], [2, 116/4, f]).
step_left([2, 117/12, f], [2, 116/12, f]).
step_left([2, 117/16, f], [2, 116/16, f]).
step_left([2, 117/18, f], [2, 116/18, f]).
step_left([2, 117/22, f], [2, 116/22, f]).
step_left([2, 117/24, f], [2, 116/24, f]).
step_left([2, 117/30, f], [2, 116/30, f]).
step_left([2, 117/32, f], [2, 116/32, f]).
step_left([2, 117/34, f], [2, 116/34, f]).
step_left([2, 117/38, f], [2, 116/38, f]).
step_left([2, 117/40, f], [2, 116/40, f]).
step_left([2, 117/42, f], [2, 116/42, f]).
step_left([2, 117/44, f], [2, 116/44, f]).
step_left([2, 117/48, f], [2, 116/48, f]).
step_left([2, 117/52, f], [2, 116/52, f]).
step_left([2, 117/54, f], [2, 116/54, f]).
step_left([2, 117/62, f], [2, 116/62, f]).
step_left([2, 117/70, f], [2, 116/70, f]).
step_left([2, 117/74, f], [2, 116/74, f]).
step_left([2, 117/78, f], [2, 116/78, f]).
step_left([2, 117/80, f], [2, 116/80, f]).
step_left([2, 117/86, f], [2, 116/86, f]).
step_left([2, 117/88, f], [2, 116/88, f]).
step_left([2, 117/96, f], [2, 116/96, f]).
step_left([2, 117/112, f], [2, 116/112, f]).
step_left([2, 117/116, f], [2, 116/116, f]).
step_left([2, 117/118, f], [2, 116/118, f]).
step_left([2, 118/2, f], [2, 117/2, f]).
step_left([2, 118/4, f], [2, 117/4, f]).
step_left([2, 118/12, f], [2, 117/12, f]).
step_left([2, 118/16, f], [2, 117/16, f]).
step_left([2, 118/18, f], [2, 117/18, f]).
step_left([2, 118/22, f], [2, 117/22, f]).
step_left([2, 118/24, f], [2, 117/24, f]).
step_left([2, 118/30, f], [2, 117/30, f]).
step_left([2, 118/32, f], [2, 117/32, f]).
step_left([2, 118/34, f], [2, 117/34, f]).
step_left([2, 118/38, f], [2, 117/38, f]).
step_left([2, 118/40, f], [2, 117/40, f]).
step_left([2, 118/42, f], [2, 117/42, f]).
step_left([2, 118/44, f], [2, 117/44, f]).
step_left([2, 118/48, f], [2, 117/48, f]).
step_left([2, 118/52, f], [2, 117/52, f]).
step_left([2, 118/54, f], [2, 117/54, f]).
step_left([2, 118/62, f], [2, 117/62, f]).
step_left([2, 118/70, f], [2, 117/70, f]).
step_left([2, 118/74, f], [2, 117/74, f]).
step_left([2, 118/78, f], [2, 117/78, f]).
step_left([2, 118/80, f], [2, 117/80, f]).
step_left([2, 118/86, f], [2, 117/86, f]).
step_left([2, 118/88, f], [2, 117/88, f]).
step_left([2, 118/96, f], [2, 117/96, f]).
step_left([2, 118/112, f], [2, 117/112, f]).
step_left([2, 118/116, f], [2, 117/116, f]).
step_left([2, 118/118, f], [2, 117/118, f]).
step_right([2, 0/0, f], [2, 1/0, f]).
step_right([2, 0/10, f], [2, 1/10, f]).
step_right([2, 0/12, f], [2, 1/12, f]).
step_right([2, 0/16, f], [2, 1/16, f]).
step_right([2, 0/20, f], [2, 1/20, f]).
step_right([2, 0/22, f], [2, 1/22, f]).
step_right([2, 0/24, f], [2, 1/24, f]).
step_right([2, 0/34, f], [2, 1/34, f]).
step_right([2, 0/36, f], [2, 1/36, f]).
step_right([2, 0/38, f], [2, 1/38, f]).
step_right([2, 0/40, f], [2, 1/40, f]).
step_right([2, 0/44, f], [2, 1/44, f]).
step_right([2, 0/48, f], [2, 1/48, f]).
step_right([2, 0/50, f], [2, 1/50, f]).
step_right([2, 0/56, f], [2, 1/56, f]).
step_right([2, 0/58, f], [2, 1/58, f]).
step_right([2, 0/64, f], [2, 1/64, f]).
step_right([2, 0/72, f], [2, 1/72, f]).
step_right([2, 0/76, f], [2, 1/76, f]).
step_right([2, 0/80, f], [2, 1/80, f]).
step_right([2, 0/82, f], [2, 1/82, f]).
step_right([2, 0/84, f], [2, 1/84, f]).
step_right([2, 0/94, f], [2, 1/94, f]).
step_right([2, 0/98, f], [2, 1/98, f]).
step_right([2, 0/100, f], [2, 1/100, f]).
step_right([2, 0/102, f], [2, 1/102, f]).
step_right([2, 0/106, f], [2, 1/106, f]).
step_right([2, 0/112, f], [2, 1/112, f]).
step_right([2, 1/0, f], [2, 2/0, f]).
step_right([2, 1/10, f], [2, 2/10, f]).
step_right([2, 1/12, f], [2, 2/12, f]).
step_right([2, 1/16, f], [2, 2/16, f]).
step_right([2, 1/20, f], [2, 2/20, f]).
step_right([2, 1/22, f], [2, 2/22, f]).
step_right([2, 1/24, f], [2, 2/24, f]).
step_right([2, 1/34, f], [2, 2/34, f]).
step_right([2, 1/36, f], [2, 2/36, f]).
step_right([2, 1/38, f], [2, 2/38, f]).
step_right([2, 1/40, f], [2, 2/40, f]).
step_right([2, 1/44, f], [2, 2/44, f]).
step_right([2, 1/48, f], [2, 2/48, f]).
step_right([2, 1/50, f], [2, 2/50, f]).
step_right([2, 1/56, f], [2, 2/56, f]).
step_right([2, 1/58, f], [2, 2/58, f]).
step_right([2, 1/64, f], [2, 2/64, f]).
step_right([2, 1/72, f], [2, 2/72, f]).
step_right([2, 1/76, f], [2, 2/76, f]).
step_right([2, 1/80, f], [2, 2/80, f]).
step_right([2, 1/82, f], [2, 2/82, f]).
step_right([2, 1/84, f], [2, 2/84, f]).
step_right([2, 1/94, f], [2, 2/94, f]).
step_right([2, 1/98, f], [2, 2/98, f]).
step_right([2, 1/100, f], [2, 2/100, f]).
step_right([2, 1/102, f], [2, 2/102, f]).
step_right([2, 1/106, f], [2, 2/106, f]).
step_right([2, 1/112, f], [2, 2/112, f]).
step_right([2, 2/4, f], [2, 3/4, f]).
step_right([2, 2/6, f], [2, 3/6, f]).
step_right([2, 2/12, f], [2, 3/12, f]).
step_right([2, 2/14, f], [2, 3/14, f]).
step_right([2, 2/22, f], [2, 3/22, f]).
step_right([2, 2/28, f], [2, 3/28, f]).
step_right([2, 2/30, f], [2, 3/30, f]).
step_right([2, 2/32, f], [2, 3/32, f]).
step_right([2, 2/46, f], [2, 3/46, f]).
step_right([2, 2/48, f], [2, 3/48, f]).
step_right([2, 2/50, f], [2, 3/50, f]).
step_right([2, 2/52, f], [2, 3/52, f]).
step_right([2, 2/58, f], [2, 3/58, f]).
step_right([2, 2/60, f], [2, 3/60, f]).
step_right([2, 2/62, f], [2, 3/62, f]).
step_right([2, 2/64, f], [2, 3/64, f]).
step_right([2, 2/66, f], [2, 3/66, f]).
step_right([2, 2/74, f], [2, 3/74, f]).
step_right([2, 2/80, f], [2, 3/80, f]).
step_right([2, 2/82, f], [2, 3/82, f]).
step_right([2, 2/88, f], [2, 3/88, f]).
step_right([2, 2/90, f], [2, 3/90, f]).
step_right([2, 2/96, f], [2, 3/96, f]).
step_right([2, 2/100, f], [2, 3/100, f]).
step_right([2, 2/102, f], [2, 3/102, f]).
step_right([2, 2/104, f], [2, 3/104, f]).
step_right([2, 2/108, f], [2, 3/108, f]).
step_right([2, 2/116, f], [2, 3/116, f]).
step_right([2, 2/118, f], [2, 3/118, f]).
step_right([2, 3/4, f], [2, 4/4, f]).
step_right([2, 3/6, f], [2, 4/6, f]).
step_right([2, 3/12, f], [2, 4/12, f]).
step_right([2, 3/14, f], [2, 4/14, f]).
step_right([2, 3/22, f], [2, 4/22, f]).
step_right([2, 3/28, f], [2, 4/28, f]).
step_right([2, 3/30, f], [2, 4/30, f]).
step_right([2, 3/32, f], [2, 4/32, f]).
step_right([2, 3/46, f], [2, 4/46, f]).
step_right([2, 3/48, f], [2, 4/48, f]).
step_right([2, 3/50, f], [2, 4/50, f]).
step_right([2, 3/52, f], [2, 4/52, f]).
step_right([2, 3/58, f], [2, 4/58, f]).
step_right([2, 3/60, f], [2, 4/60, f]).
step_right([2, 3/62, f], [2, 4/62, f]).
step_right([2, 3/64, f], [2, 4/64, f]).
step_right([2, 3/66, f], [2, 4/66, f]).
step_right([2, 3/74, f], [2, 4/74, f]).
step_right([2, 3/80, f], [2, 4/80, f]).
step_right([2, 3/82, f], [2, 4/82, f]).
step_right([2, 3/88, f], [2, 4/88, f]).
step_right([2, 3/90, f], [2, 4/90, f]).
step_right([2, 3/96, f], [2, 4/96, f]).
step_right([2, 3/100, f], [2, 4/100, f]).
step_right([2, 3/102, f], [2, 4/102, f]).
step_right([2, 3/104, f], [2, 4/104, f]).
step_right([2, 3/108, f], [2, 4/108, f]).
step_right([2, 3/116, f], [2, 4/116, f]).
step_right([2, 3/118, f], [2, 4/118, f]).
step_right([2, 4/0, f], [2, 5/0, f]).
step_right([2, 4/2, f], [2, 5/2, f]).
step_right([2, 4/6, f], [2, 5/6, f]).
step_right([2, 4/8, f], [2, 5/8, f]).
step_right([2, 4/10, f], [2, 5/10, f]).
step_right([2, 4/12, f], [2, 5/12, f]).
step_right([2, 4/14, f], [2, 5/14, f]).
step_right([2, 4/18, f], [2, 5/18, f]).
step_right([2, 4/20, f], [2, 5/20, f]).
step_right([2, 4/26, f], [2, 5/26, f]).
step_right([2, 4/28, f], [2, 5/28, f]).
step_right([2, 4/30, f], [2, 5/30, f]).
step_right([2, 4/38, f], [2, 5/38, f]).
step_right([2, 4/44, f], [2, 5/44, f]).
step_right([2, 4/46, f], [2, 5/46, f]).
step_right([2, 4/52, f], [2, 5/52, f]).
step_right([2, 4/56, f], [2, 5/56, f]).
step_right([2, 4/58, f], [2, 5/58, f]).
step_right([2, 4/60, f], [2, 5/60, f]).
step_right([2, 4/62, f], [2, 5/62, f]).
step_right([2, 4/66, f], [2, 5/66, f]).
step_right([2, 4/68, f], [2, 5/68, f]).
step_right([2, 4/76, f], [2, 5/76, f]).
step_right([2, 4/78, f], [2, 5/78, f]).
step_right([2, 4/80, f], [2, 5/80, f]).
step_right([2, 4/82, f], [2, 5/82, f]).
step_right([2, 4/84, f], [2, 5/84, f]).
step_right([2, 4/86, f], [2, 5/86, f]).
step_right([2, 4/88, f], [2, 5/88, f]).
step_right([2, 4/92, f], [2, 5/92, f]).
step_right([2, 4/94, f], [2, 5/94, f]).
step_right([2, 4/102, f], [2, 5/102, f]).
step_right([2, 4/106, f], [2, 5/106, f]).
step_right([2, 4/110, f], [2, 5/110, f]).
step_right([2, 4/112, f], [2, 5/112, f]).
step_right([2, 4/114, f], [2, 5/114, f]).
step_right([2, 4/118, f], [2, 5/118, f]).
step_right([2, 5/0, f], [2, 6/0, f]).
step_right([2, 5/2, f], [2, 6/2, f]).
step_right([2, 5/6, f], [2, 6/6, f]).
step_right([2, 5/8, f], [2, 6/8, f]).
step_right([2, 5/10, f], [2, 6/10, f]).
step_right([2, 5/12, f], [2, 6/12, f]).
step_right([2, 5/14, f], [2, 6/14, f]).
step_right([2, 5/18, f], [2, 6/18, f]).
step_right([2, 5/20, f], [2, 6/20, f]).
step_right([2, 5/26, f], [2, 6/26, f]).
step_right([2, 5/28, f], [2, 6/28, f]).
step_right([2, 5/30, f], [2, 6/30, f]).
step_right([2, 5/38, f], [2, 6/38, f]).
step_right([2, 5/44, f], [2, 6/44, f]).
step_right([2, 5/46, f], [2, 6/46, f]).
step_right([2, 5/52, f], [2, 6/52, f]).
step_right([2, 5/56, f], [2, 6/56, f]).
step_right([2, 5/58, f], [2, 6/58, f]).
step_right([2, 5/60, f], [2, 6/60, f]).
step_right([2, 5/62, f], [2, 6/62, f]).
step_right([2, 5/66, f], [2, 6/66, f]).
step_right([2, 5/68, f], [2, 6/68, f]).
step_right([2, 5/76, f], [2, 6/76, f]).
step_right([2, 5/78, f], [2, 6/78, f]).
step_right([2, 5/80, f], [2, 6/80, f]).
step_right([2, 5/82, f], [2, 6/82, f]).
step_right([2, 5/84, f], [2, 6/84, f]).
step_right([2, 5/86, f], [2, 6/86, f]).
step_right([2, 5/88, f], [2, 6/88, f]).
step_right([2, 5/92, f], [2, 6/92, f]).
step_right([2, 5/94, f], [2, 6/94, f]).
step_right([2, 5/102, f], [2, 6/102, f]).
step_right([2, 5/106, f], [2, 6/106, f]).
step_right([2, 5/110, f], [2, 6/110, f]).
step_right([2, 5/112, f], [2, 6/112, f]).
step_right([2, 5/114, f], [2, 6/114, f]).
step_right([2, 5/118, f], [2, 6/118, f]).
step_right([2, 6/0, f], [2, 7/0, f]).
step_right([2, 6/4, f], [2, 7/4, f]).
step_right([2, 6/14, f], [2, 7/14, f]).
step_right([2, 6/16, f], [2, 7/16, f]).
step_right([2, 6/20, f], [2, 7/20, f]).
step_right([2, 6/30, f], [2, 7/30, f]).
step_right([2, 6/32, f], [2, 7/32, f]).
step_right([2, 6/36, f], [2, 7/36, f]).
step_right([2, 6/40, f], [2, 7/40, f]).
step_right([2, 6/42, f], [2, 7/42, f]).
step_right([2, 6/46, f], [2, 7/46, f]).
step_right([2, 6/48, f], [2, 7/48, f]).
step_right([2, 6/54, f], [2, 7/54, f]).
step_right([2, 6/58, f], [2, 7/58, f]).
step_right([2, 6/78, f], [2, 7/78, f]).
step_right([2, 6/82, f], [2, 7/82, f]).
step_right([2, 6/84, f], [2, 7/84, f]).
step_right([2, 6/86, f], [2, 7/86, f]).
step_right([2, 6/88, f], [2, 7/88, f]).
step_right([2, 6/90, f], [2, 7/90, f]).
step_right([2, 6/104, f], [2, 7/104, f]).
step_right([2, 6/106, f], [2, 7/106, f]).
step_right([2, 6/108, f], [2, 7/108, f]).
step_right([2, 6/112, f], [2, 7/112, f]).
step_right([2, 6/118, f], [2, 7/118, f]).
step_right([2, 7/0, f], [2, 8/0, f]).
step_right([2, 7/4, f], [2, 8/4, f]).
step_right([2, 7/14, f], [2, 8/14, f]).
step_right([2, 7/16, f], [2, 8/16, f]).
step_right([2, 7/20, f], [2, 8/20, f]).
step_right([2, 7/30, f], [2, 8/30, f]).
step_right([2, 7/32, f], [2, 8/32, f]).
step_right([2, 7/36, f], [2, 8/36, f]).
step_right([2, 7/40, f], [2, 8/40, f]).
step_right([2, 7/42, f], [2, 8/42, f]).
step_right([2, 7/46, f], [2, 8/46, f]).
step_right([2, 7/48, f], [2, 8/48, f]).
step_right([2, 7/54, f], [2, 8/54, f]).
step_right([2, 7/58, f], [2, 8/58, f]).
step_right([2, 7/78, f], [2, 8/78, f]).
step_right([2, 7/82, f], [2, 8/82, f]).
step_right([2, 7/84, f], [2, 8/84, f]).
step_right([2, 7/86, f], [2, 8/86, f]).
step_right([2, 7/88, f], [2, 8/88, f]).
step_right([2, 7/90, f], [2, 8/90, f]).
step_right([2, 7/104, f], [2, 8/104, f]).
step_right([2, 7/106, f], [2, 8/106, f]).
step_right([2, 7/108, f], [2, 8/108, f]).
step_right([2, 7/112, f], [2, 8/112, f]).
step_right([2, 7/118, f], [2, 8/118, f]).
step_right([2, 8/0, f], [2, 9/0, f]).
step_right([2, 8/2, f], [2, 9/2, f]).
step_right([2, 8/6, f], [2, 9/6, f]).
step_right([2, 8/8, f], [2, 9/8, f]).
step_right([2, 8/16, f], [2, 9/16, f]).
step_right([2, 8/18, f], [2, 9/18, f]).
step_right([2, 8/22, f], [2, 9/22, f]).
step_right([2, 8/24, f], [2, 9/24, f]).
step_right([2, 8/30, f], [2, 9/30, f]).
step_right([2, 8/34, f], [2, 9/34, f]).
step_right([2, 8/38, f], [2, 9/38, f]).
step_right([2, 8/44, f], [2, 9/44, f]).
step_right([2, 8/46, f], [2, 9/46, f]).
step_right([2, 8/52, f], [2, 9/52, f]).
step_right([2, 8/56, f], [2, 9/56, f]).
step_right([2, 8/58, f], [2, 9/58, f]).
step_right([2, 8/60, f], [2, 9/60, f]).
step_right([2, 8/62, f], [2, 9/62, f]).
step_right([2, 8/64, f], [2, 9/64, f]).
step_right([2, 8/76, f], [2, 9/76, f]).
step_right([2, 8/78, f], [2, 9/78, f]).
step_right([2, 8/84, f], [2, 9/84, f]).
step_right([2, 8/86, f], [2, 9/86, f]).
step_right([2, 8/88, f], [2, 9/88, f]).
step_right([2, 8/94, f], [2, 9/94, f]).
step_right([2, 8/98, f], [2, 9/98, f]).
step_right([2, 8/100, f], [2, 9/100, f]).
step_right([2, 8/102, f], [2, 9/102, f]).
step_right([2, 8/108, f], [2, 9/108, f]).
step_right([2, 8/112, f], [2, 9/112, f]).
step_right([2, 8/114, f], [2, 9/114, f]).
step_right([2, 9/0, f], [2, 10/0, f]).
step_right([2, 9/2, f], [2, 10/2, f]).
step_right([2, 9/6, f], [2, 10/6, f]).
step_right([2, 9/8, f], [2, 10/8, f]).
step_right([2, 9/16, f], [2, 10/16, f]).
step_right([2, 9/18, f], [2, 10/18, f]).
step_right([2, 9/22, f], [2, 10/22, f]).
step_right([2, 9/24, f], [2, 10/24, f]).
step_right([2, 9/30, f], [2, 10/30, f]).
step_right([2, 9/34, f], [2, 10/34, f]).
step_right([2, 9/38, f], [2, 10/38, f]).
step_right([2, 9/44, f], [2, 10/44, f]).
step_right([2, 9/46, f], [2, 10/46, f]).
step_right([2, 9/52, f], [2, 10/52, f]).
step_right([2, 9/56, f], [2, 10/56, f]).
step_right([2, 9/58, f], [2, 10/58, f]).
step_right([2, 9/60, f], [2, 10/60, f]).
step_right([2, 9/62, f], [2, 10/62, f]).
step_right([2, 9/64, f], [2, 10/64, f]).
step_right([2, 9/76, f], [2, 10/76, f]).
step_right([2, 9/78, f], [2, 10/78, f]).
step_right([2, 9/84, f], [2, 10/84, f]).
step_right([2, 9/86, f], [2, 10/86, f]).
step_right([2, 9/88, f], [2, 10/88, f]).
step_right([2, 9/94, f], [2, 10/94, f]).
step_right([2, 9/98, f], [2, 10/98, f]).
step_right([2, 9/100, f], [2, 10/100, f]).
step_right([2, 9/102, f], [2, 10/102, f]).
step_right([2, 9/108, f], [2, 10/108, f]).
step_right([2, 9/112, f], [2, 10/112, f]).
step_right([2, 9/114, f], [2, 10/114, f]).
step_right([2, 10/0, f], [2, 11/0, f]).
step_right([2, 10/6, f], [2, 11/6, f]).
step_right([2, 10/8, f], [2, 11/8, f]).
step_right([2, 10/10, f], [2, 11/10, f]).
step_right([2, 10/12, f], [2, 11/12, f]).
step_right([2, 10/14, f], [2, 11/14, f]).
step_right([2, 10/18, f], [2, 11/18, f]).
step_right([2, 10/20, f], [2, 11/20, f]).
step_right([2, 10/26, f], [2, 11/26, f]).
step_right([2, 10/28, f], [2, 11/28, f]).
step_right([2, 10/34, f], [2, 11/34, f]).
step_right([2, 10/38, f], [2, 11/38, f]).
step_right([2, 10/40, f], [2, 11/40, f]).
step_right([2, 10/48, f], [2, 11/48, f]).
step_right([2, 10/54, f], [2, 11/54, f]).
step_right([2, 10/56, f], [2, 11/56, f]).
step_right([2, 10/62, f], [2, 11/62, f]).
step_right([2, 10/66, f], [2, 11/66, f]).
step_right([2, 10/74, f], [2, 11/74, f]).
step_right([2, 10/82, f], [2, 11/82, f]).
step_right([2, 10/84, f], [2, 11/84, f]).
step_right([2, 10/90, f], [2, 11/90, f]).
step_right([2, 10/92, f], [2, 11/92, f]).
step_right([2, 10/96, f], [2, 11/96, f]).
step_right([2, 10/100, f], [2, 11/100, f]).
step_right([2, 10/104, f], [2, 11/104, f]).
step_right([2, 10/106, f], [2, 11/106, f]).
step_right([2, 10/110, f], [2, 11/110, f]).
step_right([2, 10/118, f], [2, 11/118, f]).
step_right([2, 11/0, f], [2, 12/0, f]).
step_right([2, 11/6, f], [2, 12/6, f]).
step_right([2, 11/8, f], [2, 12/8, f]).
step_right([2, 11/10, f], [2, 12/10, f]).
step_right([2, 11/12, f], [2, 12/12, f]).
step_right([2, 11/14, f], [2, 12/14, f]).
step_right([2, 11/18, f], [2, 12/18, f]).
step_right([2, 11/20, f], [2, 12/20, f]).
step_right([2, 11/26, f], [2, 12/26, f]).
step_right([2, 11/28, f], [2, 12/28, f]).
step_right([2, 11/34, f], [2, 12/34, f]).
step_right([2, 11/38, f], [2, 12/38, f]).
step_right([2, 11/40, f], [2, 12/40, f]).
step_right([2, 11/48, f], [2, 12/48, f]).
step_right([2, 11/54, f], [2, 12/54, f]).
step_right([2, 11/56, f], [2, 12/56, f]).
step_right([2, 11/62, f], [2, 12/62, f]).
step_right([2, 11/66, f], [2, 12/66, f]).
step_right([2, 11/74, f], [2, 12/74, f]).
step_right([2, 11/82, f], [2, 12/82, f]).
step_right([2, 11/84, f], [2, 12/84, f]).
step_right([2, 11/90, f], [2, 12/90, f]).
step_right([2, 11/92, f], [2, 12/92, f]).
step_right([2, 11/96, f], [2, 12/96, f]).
step_right([2, 11/100, f], [2, 12/100, f]).
step_right([2, 11/104, f], [2, 12/104, f]).
step_right([2, 11/106, f], [2, 12/106, f]).
step_right([2, 11/110, f], [2, 12/110, f]).
step_right([2, 11/118, f], [2, 12/118, f]).
step_right([2, 12/0, f], [2, 13/0, f]).
step_right([2, 12/2, f], [2, 13/2, f]).
step_right([2, 12/12, f], [2, 13/12, f]).
step_right([2, 12/16, f], [2, 13/16, f]).
step_right([2, 12/18, f], [2, 13/18, f]).
step_right([2, 12/22, f], [2, 13/22, f]).
step_right([2, 12/24, f], [2, 13/24, f]).
step_right([2, 12/26, f], [2, 13/26, f]).
step_right([2, 12/28, f], [2, 13/28, f]).
step_right([2, 12/30, f], [2, 13/30, f]).
step_right([2, 12/32, f], [2, 13/32, f]).
step_right([2, 12/36, f], [2, 13/36, f]).
step_right([2, 12/42, f], [2, 13/42, f]).
step_right([2, 12/44, f], [2, 13/44, f]).
step_right([2, 12/50, f], [2, 13/50, f]).
step_right([2, 12/56, f], [2, 13/56, f]).
step_right([2, 12/58, f], [2, 13/58, f]).
step_right([2, 12/60, f], [2, 13/60, f]).
step_right([2, 12/64, f], [2, 13/64, f]).
step_right([2, 12/68, f], [2, 13/68, f]).
step_right([2, 12/72, f], [2, 13/72, f]).
step_right([2, 12/76, f], [2, 13/76, f]).
step_right([2, 12/78, f], [2, 13/78, f]).
step_right([2, 12/82, f], [2, 13/82, f]).
step_right([2, 12/84, f], [2, 13/84, f]).
step_right([2, 12/88, f], [2, 13/88, f]).
step_right([2, 12/90, f], [2, 13/90, f]).
step_right([2, 12/94, f], [2, 13/94, f]).
step_right([2, 12/96, f], [2, 13/96, f]).
step_right([2, 12/98, f], [2, 13/98, f]).
step_right([2, 12/102, f], [2, 13/102, f]).
step_right([2, 12/104, f], [2, 13/104, f]).
step_right([2, 12/106, f], [2, 13/106, f]).
step_right([2, 12/108, f], [2, 13/108, f]).
step_right([2, 12/112, f], [2, 13/112, f]).
step_right([2, 12/114, f], [2, 13/114, f]).
step_right([2, 12/116, f], [2, 13/116, f]).
step_right([2, 12/118, f], [2, 13/118, f]).
step_right([2, 13/0, f], [2, 14/0, f]).
step_right([2, 13/2, f], [2, 14/2, f]).
step_right([2, 13/12, f], [2, 14/12, f]).
step_right([2, 13/16, f], [2, 14/16, f]).
step_right([2, 13/18, f], [2, 14/18, f]).
step_right([2, 13/22, f], [2, 14/22, f]).
step_right([2, 13/24, f], [2, 14/24, f]).
step_right([2, 13/26, f], [2, 14/26, f]).
step_right([2, 13/28, f], [2, 14/28, f]).
step_right([2, 13/30, f], [2, 14/30, f]).
step_right([2, 13/32, f], [2, 14/32, f]).
step_right([2, 13/36, f], [2, 14/36, f]).
step_right([2, 13/42, f], [2, 14/42, f]).
step_right([2, 13/44, f], [2, 14/44, f]).
step_right([2, 13/50, f], [2, 14/50, f]).
step_right([2, 13/56, f], [2, 14/56, f]).
step_right([2, 13/58, f], [2, 14/58, f]).
step_right([2, 13/60, f], [2, 14/60, f]).
step_right([2, 13/64, f], [2, 14/64, f]).
step_right([2, 13/68, f], [2, 14/68, f]).
step_right([2, 13/72, f], [2, 14/72, f]).
step_right([2, 13/76, f], [2, 14/76, f]).
step_right([2, 13/78, f], [2, 14/78, f]).
step_right([2, 13/82, f], [2, 14/82, f]).
step_right([2, 13/84, f], [2, 14/84, f]).
step_right([2, 13/88, f], [2, 14/88, f]).
step_right([2, 13/90, f], [2, 14/90, f]).
step_right([2, 13/94, f], [2, 14/94, f]).
step_right([2, 13/96, f], [2, 14/96, f]).
step_right([2, 13/98, f], [2, 14/98, f]).
step_right([2, 13/102, f], [2, 14/102, f]).
step_right([2, 13/104, f], [2, 14/104, f]).
step_right([2, 13/106, f], [2, 14/106, f]).
step_right([2, 13/108, f], [2, 14/108, f]).
step_right([2, 13/112, f], [2, 14/112, f]).
step_right([2, 13/114, f], [2, 14/114, f]).
step_right([2, 13/116, f], [2, 14/116, f]).
step_right([2, 13/118, f], [2, 14/118, f]).
step_right([2, 14/0, f], [2, 15/0, f]).
step_right([2, 14/2, f], [2, 15/2, f]).
step_right([2, 14/6, f], [2, 15/6, f]).
step_right([2, 14/14, f], [2, 15/14, f]).
step_right([2, 14/16, f], [2, 15/16, f]).
step_right([2, 14/18, f], [2, 15/18, f]).
step_right([2, 14/20, f], [2, 15/20, f]).
step_right([2, 14/24, f], [2, 15/24, f]).
step_right([2, 14/26, f], [2, 15/26, f]).
step_right([2, 14/30, f], [2, 15/30, f]).
step_right([2, 14/32, f], [2, 15/32, f]).
step_right([2, 14/34, f], [2, 15/34, f]).
step_right([2, 14/36, f], [2, 15/36, f]).
step_right([2, 14/38, f], [2, 15/38, f]).
step_right([2, 14/40, f], [2, 15/40, f]).
step_right([2, 14/44, f], [2, 15/44, f]).
step_right([2, 14/46, f], [2, 15/46, f]).
step_right([2, 14/48, f], [2, 15/48, f]).
step_right([2, 14/50, f], [2, 15/50, f]).
step_right([2, 14/52, f], [2, 15/52, f]).
step_right([2, 14/58, f], [2, 15/58, f]).
step_right([2, 14/64, f], [2, 15/64, f]).
step_right([2, 14/66, f], [2, 15/66, f]).
step_right([2, 14/78, f], [2, 15/78, f]).
step_right([2, 14/80, f], [2, 15/80, f]).
step_right([2, 14/82, f], [2, 15/82, f]).
step_right([2, 14/86, f], [2, 15/86, f]).
step_right([2, 14/88, f], [2, 15/88, f]).
step_right([2, 14/92, f], [2, 15/92, f]).
step_right([2, 14/98, f], [2, 15/98, f]).
step_right([2, 14/102, f], [2, 15/102, f]).
step_right([2, 14/110, f], [2, 15/110, f]).
step_right([2, 14/112, f], [2, 15/112, f]).
step_right([2, 14/114, f], [2, 15/114, f]).
step_right([2, 14/118, f], [2, 15/118, f]).
step_right([2, 15/0, f], [2, 16/0, f]).
step_right([2, 15/2, f], [2, 16/2, f]).
step_right([2, 15/6, f], [2, 16/6, f]).
step_right([2, 15/14, f], [2, 16/14, f]).
step_right([2, 15/16, f], [2, 16/16, f]).
step_right([2, 15/18, f], [2, 16/18, f]).
step_right([2, 15/20, f], [2, 16/20, f]).
step_right([2, 15/24, f], [2, 16/24, f]).
step_right([2, 15/26, f], [2, 16/26, f]).
step_right([2, 15/30, f], [2, 16/30, f]).
step_right([2, 15/32, f], [2, 16/32, f]).
step_right([2, 15/34, f], [2, 16/34, f]).
step_right([2, 15/36, f], [2, 16/36, f]).
step_right([2, 15/38, f], [2, 16/38, f]).
step_right([2, 15/40, f], [2, 16/40, f]).
step_right([2, 15/44, f], [2, 16/44, f]).
step_right([2, 15/46, f], [2, 16/46, f]).
step_right([2, 15/48, f], [2, 16/48, f]).
step_right([2, 15/50, f], [2, 16/50, f]).
step_right([2, 15/52, f], [2, 16/52, f]).
step_right([2, 15/58, f], [2, 16/58, f]).
step_right([2, 15/64, f], [2, 16/64, f]).
step_right([2, 15/66, f], [2, 16/66, f]).
step_right([2, 15/78, f], [2, 16/78, f]).
step_right([2, 15/80, f], [2, 16/80, f]).
step_right([2, 15/82, f], [2, 16/82, f]).
step_right([2, 15/86, f], [2, 16/86, f]).
step_right([2, 15/88, f], [2, 16/88, f]).
step_right([2, 15/92, f], [2, 16/92, f]).
step_right([2, 15/98, f], [2, 16/98, f]).
step_right([2, 15/102, f], [2, 16/102, f]).
step_right([2, 15/110, f], [2, 16/110, f]).
step_right([2, 15/112, f], [2, 16/112, f]).
step_right([2, 15/114, f], [2, 16/114, f]).
step_right([2, 15/118, f], [2, 16/118, f]).
step_right([2, 16/0, f], [2, 17/0, f]).
step_right([2, 16/4, f], [2, 17/4, f]).
step_right([2, 16/10, f], [2, 17/10, f]).
step_right([2, 16/18, f], [2, 17/18, f]).
step_right([2, 16/20, f], [2, 17/20, f]).
step_right([2, 16/22, f], [2, 17/22, f]).
step_right([2, 16/24, f], [2, 17/24, f]).
step_right([2, 16/26, f], [2, 17/26, f]).
step_right([2, 16/28, f], [2, 17/28, f]).
step_right([2, 16/32, f], [2, 17/32, f]).
step_right([2, 16/38, f], [2, 17/38, f]).
step_right([2, 16/42, f], [2, 17/42, f]).
step_right([2, 16/48, f], [2, 17/48, f]).
step_right([2, 16/52, f], [2, 17/52, f]).
step_right([2, 16/54, f], [2, 17/54, f]).
step_right([2, 16/58, f], [2, 17/58, f]).
step_right([2, 16/60, f], [2, 17/60, f]).
step_right([2, 16/66, f], [2, 17/66, f]).
step_right([2, 16/68, f], [2, 17/68, f]).
step_right([2, 16/78, f], [2, 17/78, f]).
step_right([2, 16/90, f], [2, 17/90, f]).
step_right([2, 16/96, f], [2, 17/96, f]).
step_right([2, 16/98, f], [2, 17/98, f]).
step_right([2, 16/100, f], [2, 17/100, f]).
step_right([2, 16/104, f], [2, 17/104, f]).
step_right([2, 17/0, f], [2, 18/0, f]).
step_right([2, 17/4, f], [2, 18/4, f]).
step_right([2, 17/10, f], [2, 18/10, f]).
step_right([2, 17/18, f], [2, 18/18, f]).
step_right([2, 17/20, f], [2, 18/20, f]).
step_right([2, 17/22, f], [2, 18/22, f]).
step_right([2, 17/24, f], [2, 18/24, f]).
step_right([2, 17/26, f], [2, 18/26, f]).
step_right([2, 17/28, f], [2, 18/28, f]).
step_right([2, 17/32, f], [2, 18/32, f]).
step_right([2, 17/38, f], [2, 18/38, f]).
step_right([2, 17/42, f], [2, 18/42, f]).
step_right([2, 17/48, f], [2, 18/48, f]).
step_right([2, 17/52, f], [2, 18/52, f]).
step_right([2, 17/54, f], [2, 18/54, f]).
step_right([2, 17/58, f], [2, 18/58, f]).
step_right([2, 17/60, f], [2, 18/60, f]).
step_right([2, 17/66, f], [2, 18/66, f]).
step_right([2, 17/68, f], [2, 18/68, f]).
step_right([2, 17/78, f], [2, 18/78, f]).
step_right([2, 17/90, f], [2, 18/90, f]).
step_right([2, 17/96, f], [2, 18/96, f]).
step_right([2, 17/98, f], [2, 18/98, f]).
step_right([2, 17/100, f], [2, 18/100, f]).
step_right([2, 17/104, f], [2, 18/104, f]).
step_right([2, 18/2, f], [2, 19/2, f]).
step_right([2, 18/4, f], [2, 19/4, f]).
step_right([2, 18/8, f], [2, 19/8, f]).
step_right([2, 18/12, f], [2, 19/12, f]).
step_right([2, 18/14, f], [2, 19/14, f]).
step_right([2, 18/16, f], [2, 19/16, f]).
step_right([2, 18/18, f], [2, 19/18, f]).
step_right([2, 18/24, f], [2, 19/24, f]).
step_right([2, 18/30, f], [2, 19/30, f]).
step_right([2, 18/34, f], [2, 19/34, f]).
step_right([2, 18/36, f], [2, 19/36, f]).
step_right([2, 18/40, f], [2, 19/40, f]).
step_right([2, 18/44, f], [2, 19/44, f]).
step_right([2, 18/50, f], [2, 19/50, f]).
step_right([2, 18/54, f], [2, 19/54, f]).
step_right([2, 18/56, f], [2, 19/56, f]).
step_right([2, 18/60, f], [2, 19/60, f]).
step_right([2, 18/62, f], [2, 19/62, f]).
step_right([2, 18/66, f], [2, 19/66, f]).
step_right([2, 18/68, f], [2, 19/68, f]).
step_right([2, 18/70, f], [2, 19/70, f]).
step_right([2, 18/72, f], [2, 19/72, f]).
step_right([2, 18/80, f], [2, 19/80, f]).
step_right([2, 18/84, f], [2, 19/84, f]).
step_right([2, 18/86, f], [2, 19/86, f]).
step_right([2, 18/88, f], [2, 19/88, f]).
step_right([2, 18/92, f], [2, 19/92, f]).
step_right([2, 18/94, f], [2, 19/94, f]).
step_right([2, 18/98, f], [2, 19/98, f]).
step_right([2, 18/102, f], [2, 19/102, f]).
step_right([2, 18/110, f], [2, 19/110, f]).
step_right([2, 18/118, f], [2, 19/118, f]).
step_right([2, 19/2, f], [2, 20/2, f]).
step_right([2, 19/4, f], [2, 20/4, f]).
step_right([2, 19/8, f], [2, 20/8, f]).
step_right([2, 19/12, f], [2, 20/12, f]).
step_right([2, 19/14, f], [2, 20/14, f]).
step_right([2, 19/16, f], [2, 20/16, f]).
step_right([2, 19/18, f], [2, 20/18, f]).
step_right([2, 19/24, f], [2, 20/24, f]).
step_right([2, 19/30, f], [2, 20/30, f]).
step_right([2, 19/34, f], [2, 20/34, f]).
step_right([2, 19/36, f], [2, 20/36, f]).
step_right([2, 19/40, f], [2, 20/40, f]).
step_right([2, 19/44, f], [2, 20/44, f]).
step_right([2, 19/50, f], [2, 20/50, f]).
step_right([2, 19/54, f], [2, 20/54, f]).
step_right([2, 19/56, f], [2, 20/56, f]).
step_right([2, 19/60, f], [2, 20/60, f]).
step_right([2, 19/62, f], [2, 20/62, f]).
step_right([2, 19/66, f], [2, 20/66, f]).
step_right([2, 19/68, f], [2, 20/68, f]).
step_right([2, 19/70, f], [2, 20/70, f]).
step_right([2, 19/72, f], [2, 20/72, f]).
step_right([2, 19/80, f], [2, 20/80, f]).
step_right([2, 19/84, f], [2, 20/84, f]).
step_right([2, 19/86, f], [2, 20/86, f]).
step_right([2, 19/88, f], [2, 20/88, f]).
step_right([2, 19/92, f], [2, 20/92, f]).
step_right([2, 19/94, f], [2, 20/94, f]).
step_right([2, 19/98, f], [2, 20/98, f]).
step_right([2, 19/102, f], [2, 20/102, f]).
step_right([2, 19/110, f], [2, 20/110, f]).
step_right([2, 19/118, f], [2, 20/118, f]).
step_right([2, 20/0, f], [2, 21/0, f]).
step_right([2, 20/2, f], [2, 21/2, f]).
step_right([2, 20/6, f], [2, 21/6, f]).
step_right([2, 20/8, f], [2, 21/8, f]).
step_right([2, 20/12, f], [2, 21/12, f]).
step_right([2, 20/14, f], [2, 21/14, f]).
step_right([2, 20/18, f], [2, 21/18, f]).
step_right([2, 20/20, f], [2, 21/20, f]).
step_right([2, 20/22, f], [2, 21/22, f]).
step_right([2, 20/26, f], [2, 21/26, f]).
step_right([2, 20/32, f], [2, 21/32, f]).
step_right([2, 20/34, f], [2, 21/34, f]).
step_right([2, 20/42, f], [2, 21/42, f]).
step_right([2, 20/46, f], [2, 21/46, f]).
step_right([2, 20/52, f], [2, 21/52, f]).
step_right([2, 20/60, f], [2, 21/60, f]).
step_right([2, 20/64, f], [2, 21/64, f]).
step_right([2, 20/66, f], [2, 21/66, f]).
step_right([2, 20/68, f], [2, 21/68, f]).
step_right([2, 20/72, f], [2, 21/72, f]).
step_right([2, 20/74, f], [2, 21/74, f]).
step_right([2, 20/82, f], [2, 21/82, f]).
step_right([2, 20/84, f], [2, 21/84, f]).
step_right([2, 20/90, f], [2, 21/90, f]).
step_right([2, 20/92, f], [2, 21/92, f]).
step_right([2, 20/94, f], [2, 21/94, f]).
step_right([2, 20/98, f], [2, 21/98, f]).
step_right([2, 20/100, f], [2, 21/100, f]).
step_right([2, 20/106, f], [2, 21/106, f]).
step_right([2, 20/112, f], [2, 21/112, f]).
step_right([2, 21/0, f], [2, 22/0, f]).
step_right([2, 21/2, f], [2, 22/2, f]).
step_right([2, 21/6, f], [2, 22/6, f]).
step_right([2, 21/8, f], [2, 22/8, f]).
step_right([2, 21/12, f], [2, 22/12, f]).
step_right([2, 21/14, f], [2, 22/14, f]).
step_right([2, 21/18, f], [2, 22/18, f]).
step_right([2, 21/20, f], [2, 22/20, f]).
step_right([2, 21/22, f], [2, 22/22, f]).
step_right([2, 21/26, f], [2, 22/26, f]).
step_right([2, 21/32, f], [2, 22/32, f]).
step_right([2, 21/34, f], [2, 22/34, f]).
step_right([2, 21/42, f], [2, 22/42, f]).
step_right([2, 21/46, f], [2, 22/46, f]).
step_right([2, 21/52, f], [2, 22/52, f]).
step_right([2, 21/60, f], [2, 22/60, f]).
step_right([2, 21/64, f], [2, 22/64, f]).
step_right([2, 21/66, f], [2, 22/66, f]).
step_right([2, 21/68, f], [2, 22/68, f]).
step_right([2, 21/72, f], [2, 22/72, f]).
step_right([2, 21/74, f], [2, 22/74, f]).
step_right([2, 21/82, f], [2, 22/82, f]).
step_right([2, 21/84, f], [2, 22/84, f]).
step_right([2, 21/90, f], [2, 22/90, f]).
step_right([2, 21/92, f], [2, 22/92, f]).
step_right([2, 21/94, f], [2, 22/94, f]).
step_right([2, 21/98, f], [2, 22/98, f]).
step_right([2, 21/100, f], [2, 22/100, f]).
step_right([2, 21/106, f], [2, 22/106, f]).
step_right([2, 21/112, f], [2, 22/112, f]).
step_right([2, 22/0, f], [2, 23/0, f]).
step_right([2, 22/2, f], [2, 23/2, f]).
step_right([2, 22/4, f], [2, 23/4, f]).
step_right([2, 22/8, f], [2, 23/8, f]).
step_right([2, 22/10, f], [2, 23/10, f]).
step_right([2, 22/14, f], [2, 23/14, f]).
step_right([2, 22/16, f], [2, 23/16, f]).
step_right([2, 22/20, f], [2, 23/20, f]).
step_right([2, 22/22, f], [2, 23/22, f]).
step_right([2, 22/24, f], [2, 23/24, f]).
step_right([2, 22/28, f], [2, 23/28, f]).
step_right([2, 22/30, f], [2, 23/30, f]).
step_right([2, 22/32, f], [2, 23/32, f]).
step_right([2, 22/34, f], [2, 23/34, f]).
step_right([2, 22/38, f], [2, 23/38, f]).
step_right([2, 22/40, f], [2, 23/40, f]).
step_right([2, 22/48, f], [2, 23/48, f]).
step_right([2, 22/50, f], [2, 23/50, f]).
step_right([2, 22/54, f], [2, 23/54, f]).
step_right([2, 22/58, f], [2, 23/58, f]).
step_right([2, 22/62, f], [2, 23/62, f]).
step_right([2, 22/64, f], [2, 23/64, f]).
step_right([2, 22/66, f], [2, 23/66, f]).
step_right([2, 22/70, f], [2, 23/70, f]).
step_right([2, 22/72, f], [2, 23/72, f]).
step_right([2, 22/74, f], [2, 23/74, f]).
step_right([2, 22/76, f], [2, 23/76, f]).
step_right([2, 22/78, f], [2, 23/78, f]).
step_right([2, 22/92, f], [2, 23/92, f]).
step_right([2, 22/96, f], [2, 23/96, f]).
step_right([2, 22/100, f], [2, 23/100, f]).
step_right([2, 22/104, f], [2, 23/104, f]).
step_right([2, 22/108, f], [2, 23/108, f]).
step_right([2, 22/110, f], [2, 23/110, f]).
step_right([2, 22/112, f], [2, 23/112, f]).
step_right([2, 22/114, f], [2, 23/114, f]).
step_right([2, 22/116, f], [2, 23/116, f]).
step_right([2, 23/0, f], [2, 24/0, f]).
step_right([2, 23/2, f], [2, 24/2, f]).
step_right([2, 23/4, f], [2, 24/4, f]).
step_right([2, 23/8, f], [2, 24/8, f]).
step_right([2, 23/10, f], [2, 24/10, f]).
step_right([2, 23/14, f], [2, 24/14, f]).
step_right([2, 23/16, f], [2, 24/16, f]).
step_right([2, 23/20, f], [2, 24/20, f]).
step_right([2, 23/22, f], [2, 24/22, f]).
step_right([2, 23/24, f], [2, 24/24, f]).
step_right([2, 23/28, f], [2, 24/28, f]).
step_right([2, 23/30, f], [2, 24/30, f]).
step_right([2, 23/32, f], [2, 24/32, f]).
step_right([2, 23/34, f], [2, 24/34, f]).
step_right([2, 23/38, f], [2, 24/38, f]).
step_right([2, 23/40, f], [2, 24/40, f]).
step_right([2, 23/48, f], [2, 24/48, f]).
step_right([2, 23/50, f], [2, 24/50, f]).
step_right([2, 23/54, f], [2, 24/54, f]).
step_right([2, 23/58, f], [2, 24/58, f]).
step_right([2, 23/62, f], [2, 24/62, f]).
step_right([2, 23/64, f], [2, 24/64, f]).
step_right([2, 23/66, f], [2, 24/66, f]).
step_right([2, 23/70, f], [2, 24/70, f]).
step_right([2, 23/72, f], [2, 24/72, f]).
step_right([2, 23/74, f], [2, 24/74, f]).
step_right([2, 23/76, f], [2, 24/76, f]).
step_right([2, 23/78, f], [2, 24/78, f]).
step_right([2, 23/92, f], [2, 24/92, f]).
step_right([2, 23/96, f], [2, 24/96, f]).
step_right([2, 23/100, f], [2, 24/100, f]).
step_right([2, 23/104, f], [2, 24/104, f]).
step_right([2, 23/108, f], [2, 24/108, f]).
step_right([2, 23/110, f], [2, 24/110, f]).
step_right([2, 23/112, f], [2, 24/112, f]).
step_right([2, 23/114, f], [2, 24/114, f]).
step_right([2, 23/116, f], [2, 24/116, f]).
step_right([2, 24/0, f], [2, 25/0, f]).
step_right([2, 24/4, f], [2, 25/4, f]).
step_right([2, 24/12, f], [2, 25/12, f]).
step_right([2, 24/14, f], [2, 25/14, f]).
step_right([2, 24/18, f], [2, 25/18, f]).
step_right([2, 24/26, f], [2, 25/26, f]).
step_right([2, 24/28, f], [2, 25/28, f]).
step_right([2, 24/36, f], [2, 25/36, f]).
step_right([2, 24/38, f], [2, 25/38, f]).
step_right([2, 24/42, f], [2, 25/42, f]).
step_right([2, 24/48, f], [2, 25/48, f]).
step_right([2, 24/50, f], [2, 25/50, f]).
step_right([2, 24/52, f], [2, 25/52, f]).
step_right([2, 24/60, f], [2, 25/60, f]).
step_right([2, 24/62, f], [2, 25/62, f]).
step_right([2, 24/64, f], [2, 25/64, f]).
step_right([2, 24/68, f], [2, 25/68, f]).
step_right([2, 24/72, f], [2, 25/72, f]).
step_right([2, 24/74, f], [2, 25/74, f]).
step_right([2, 24/76, f], [2, 25/76, f]).
step_right([2, 24/84, f], [2, 25/84, f]).
step_right([2, 24/92, f], [2, 25/92, f]).
step_right([2, 24/98, f], [2, 25/98, f]).
step_right([2, 24/106, f], [2, 25/106, f]).
step_right([2, 24/110, f], [2, 25/110, f]).
step_right([2, 24/112, f], [2, 25/112, f]).
step_right([2, 24/118, f], [2, 25/118, f]).
step_right([2, 25/0, f], [2, 26/0, f]).
step_right([2, 25/4, f], [2, 26/4, f]).
step_right([2, 25/12, f], [2, 26/12, f]).
step_right([2, 25/14, f], [2, 26/14, f]).
step_right([2, 25/18, f], [2, 26/18, f]).
step_right([2, 25/26, f], [2, 26/26, f]).
step_right([2, 25/28, f], [2, 26/28, f]).
step_right([2, 25/36, f], [2, 26/36, f]).
step_right([2, 25/38, f], [2, 26/38, f]).
step_right([2, 25/42, f], [2, 26/42, f]).
step_right([2, 25/48, f], [2, 26/48, f]).
step_right([2, 25/50, f], [2, 26/50, f]).
step_right([2, 25/52, f], [2, 26/52, f]).
step_right([2, 25/60, f], [2, 26/60, f]).
step_right([2, 25/62, f], [2, 26/62, f]).
step_right([2, 25/64, f], [2, 26/64, f]).
step_right([2, 25/68, f], [2, 26/68, f]).
step_right([2, 25/72, f], [2, 26/72, f]).
step_right([2, 25/74, f], [2, 26/74, f]).
step_right([2, 25/76, f], [2, 26/76, f]).
step_right([2, 25/84, f], [2, 26/84, f]).
step_right([2, 25/92, f], [2, 26/92, f]).
step_right([2, 25/98, f], [2, 26/98, f]).
step_right([2, 25/106, f], [2, 26/106, f]).
step_right([2, 25/110, f], [2, 26/110, f]).
step_right([2, 25/112, f], [2, 26/112, f]).
step_right([2, 25/118, f], [2, 26/118, f]).
step_right([2, 26/2, f], [2, 27/2, f]).
step_right([2, 26/4, f], [2, 27/4, f]).
step_right([2, 26/6, f], [2, 27/6, f]).
step_right([2, 26/8, f], [2, 27/8, f]).
step_right([2, 26/10, f], [2, 27/10, f]).
step_right([2, 26/16, f], [2, 27/16, f]).
step_right([2, 26/20, f], [2, 27/20, f]).
step_right([2, 26/32, f], [2, 27/32, f]).
step_right([2, 26/34, f], [2, 27/34, f]).
step_right([2, 26/36, f], [2, 27/36, f]).
step_right([2, 26/40, f], [2, 27/40, f]).
step_right([2, 26/44, f], [2, 27/44, f]).
step_right([2, 26/46, f], [2, 27/46, f]).
step_right([2, 26/56, f], [2, 27/56, f]).
step_right([2, 26/58, f], [2, 27/58, f]).
step_right([2, 26/60, f], [2, 27/60, f]).
step_right([2, 26/66, f], [2, 27/66, f]).
step_right([2, 26/72, f], [2, 27/72, f]).
step_right([2, 26/74, f], [2, 27/74, f]).
step_right([2, 26/76, f], [2, 27/76, f]).
step_right([2, 26/78, f], [2, 27/78, f]).
step_right([2, 26/82, f], [2, 27/82, f]).
step_right([2, 26/88, f], [2, 27/88, f]).
step_right([2, 26/94, f], [2, 27/94, f]).
step_right([2, 26/96, f], [2, 27/96, f]).
step_right([2, 26/100, f], [2, 27/100, f]).
step_right([2, 26/102, f], [2, 27/102, f]).
step_right([2, 26/104, f], [2, 27/104, f]).
step_right([2, 26/108, f], [2, 27/108, f]).
step_right([2, 26/110, f], [2, 27/110, f]).
step_right([2, 26/118, f], [2, 27/118, f]).
step_right([2, 27/2, f], [2, 28/2, f]).
step_right([2, 27/4, f], [2, 28/4, f]).
step_right([2, 27/6, f], [2, 28/6, f]).
step_right([2, 27/8, f], [2, 28/8, f]).
step_right([2, 27/10, f], [2, 28/10, f]).
step_right([2, 27/16, f], [2, 28/16, f]).
step_right([2, 27/20, f], [2, 28/20, f]).
step_right([2, 27/32, f], [2, 28/32, f]).
step_right([2, 27/34, f], [2, 28/34, f]).
step_right([2, 27/36, f], [2, 28/36, f]).
step_right([2, 27/40, f], [2, 28/40, f]).
step_right([2, 27/44, f], [2, 28/44, f]).
step_right([2, 27/46, f], [2, 28/46, f]).
step_right([2, 27/56, f], [2, 28/56, f]).
step_right([2, 27/58, f], [2, 28/58, f]).
step_right([2, 27/60, f], [2, 28/60, f]).
step_right([2, 27/66, f], [2, 28/66, f]).
step_right([2, 27/72, f], [2, 28/72, f]).
step_right([2, 27/74, f], [2, 28/74, f]).
step_right([2, 27/76, f], [2, 28/76, f]).
step_right([2, 27/78, f], [2, 28/78, f]).
step_right([2, 27/82, f], [2, 28/82, f]).
step_right([2, 27/88, f], [2, 28/88, f]).
step_right([2, 27/94, f], [2, 28/94, f]).
step_right([2, 27/96, f], [2, 28/96, f]).
step_right([2, 27/100, f], [2, 28/100, f]).
step_right([2, 27/102, f], [2, 28/102, f]).
step_right([2, 27/104, f], [2, 28/104, f]).
step_right([2, 27/108, f], [2, 28/108, f]).
step_right([2, 27/110, f], [2, 28/110, f]).
step_right([2, 27/118, f], [2, 28/118, f]).
step_right([2, 28/0, f], [2, 29/0, f]).
step_right([2, 28/2, f], [2, 29/2, f]).
step_right([2, 28/6, f], [2, 29/6, f]).
step_right([2, 28/8, f], [2, 29/8, f]).
step_right([2, 28/14, f], [2, 29/14, f]).
step_right([2, 28/20, f], [2, 29/20, f]).
step_right([2, 28/22, f], [2, 29/22, f]).
step_right([2, 28/28, f], [2, 29/28, f]).
step_right([2, 28/30, f], [2, 29/30, f]).
step_right([2, 28/34, f], [2, 29/34, f]).
step_right([2, 28/36, f], [2, 29/36, f]).
step_right([2, 28/38, f], [2, 29/38, f]).
step_right([2, 28/42, f], [2, 29/42, f]).
step_right([2, 28/48, f], [2, 29/48, f]).
step_right([2, 28/52, f], [2, 29/52, f]).
step_right([2, 28/54, f], [2, 29/54, f]).
step_right([2, 28/58, f], [2, 29/58, f]).
step_right([2, 28/64, f], [2, 29/64, f]).
step_right([2, 28/66, f], [2, 29/66, f]).
step_right([2, 28/70, f], [2, 29/70, f]).
step_right([2, 28/72, f], [2, 29/72, f]).
step_right([2, 28/74, f], [2, 29/74, f]).
step_right([2, 28/80, f], [2, 29/80, f]).
step_right([2, 28/88, f], [2, 29/88, f]).
step_right([2, 28/98, f], [2, 29/98, f]).
step_right([2, 28/112, f], [2, 29/112, f]).
step_right([2, 28/116, f], [2, 29/116, f]).
step_right([2, 29/0, f], [2, 30/0, f]).
step_right([2, 29/2, f], [2, 30/2, f]).
step_right([2, 29/6, f], [2, 30/6, f]).
step_right([2, 29/8, f], [2, 30/8, f]).
step_right([2, 29/14, f], [2, 30/14, f]).
step_right([2, 29/20, f], [2, 30/20, f]).
step_right([2, 29/22, f], [2, 30/22, f]).
step_right([2, 29/28, f], [2, 30/28, f]).
step_right([2, 29/30, f], [2, 30/30, f]).
step_right([2, 29/34, f], [2, 30/34, f]).
step_right([2, 29/36, f], [2, 30/36, f]).
step_right([2, 29/38, f], [2, 30/38, f]).
step_right([2, 29/42, f], [2, 30/42, f]).
step_right([2, 29/48, f], [2, 30/48, f]).
step_right([2, 29/52, f], [2, 30/52, f]).
step_right([2, 29/54, f], [2, 30/54, f]).
step_right([2, 29/58, f], [2, 30/58, f]).
step_right([2, 29/64, f], [2, 30/64, f]).
step_right([2, 29/66, f], [2, 30/66, f]).
step_right([2, 29/70, f], [2, 30/70, f]).
step_right([2, 29/72, f], [2, 30/72, f]).
step_right([2, 29/74, f], [2, 30/74, f]).
step_right([2, 29/80, f], [2, 30/80, f]).
step_right([2, 29/88, f], [2, 30/88, f]).
step_right([2, 29/98, f], [2, 30/98, f]).
step_right([2, 29/112, f], [2, 30/112, f]).
step_right([2, 29/116, f], [2, 30/116, f]).
step_right([2, 30/0, f], [2, 31/0, f]).
step_right([2, 30/8, f], [2, 31/8, f]).
step_right([2, 30/10, f], [2, 31/10, f]).
step_right([2, 30/12, f], [2, 31/12, f]).
step_right([2, 30/16, f], [2, 31/16, f]).
step_right([2, 30/18, f], [2, 31/18, f]).
step_right([2, 30/26, f], [2, 31/26, f]).
step_right([2, 30/28, f], [2, 31/28, f]).
step_right([2, 30/32, f], [2, 31/32, f]).
step_right([2, 30/34, f], [2, 31/34, f]).
step_right([2, 30/36, f], [2, 31/36, f]).
step_right([2, 30/38, f], [2, 31/38, f]).
step_right([2, 30/48, f], [2, 31/48, f]).
step_right([2, 30/50, f], [2, 31/50, f]).
step_right([2, 30/56, f], [2, 31/56, f]).
step_right([2, 30/58, f], [2, 31/58, f]).
step_right([2, 30/60, f], [2, 31/60, f]).
step_right([2, 30/62, f], [2, 31/62, f]).
step_right([2, 30/64, f], [2, 31/64, f]).
step_right([2, 30/66, f], [2, 31/66, f]).
step_right([2, 30/68, f], [2, 31/68, f]).
step_right([2, 30/70, f], [2, 31/70, f]).
step_right([2, 30/72, f], [2, 31/72, f]).
step_right([2, 30/76, f], [2, 31/76, f]).
step_right([2, 30/80, f], [2, 31/80, f]).
step_right([2, 30/82, f], [2, 31/82, f]).
step_right([2, 30/84, f], [2, 31/84, f]).
step_right([2, 30/86, f], [2, 31/86, f]).
step_right([2, 30/92, f], [2, 31/92, f]).
step_right([2, 30/94, f], [2, 31/94, f]).
step_right([2, 30/96, f], [2, 31/96, f]).
step_right([2, 30/102, f], [2, 31/102, f]).
step_right([2, 30/108, f], [2, 31/108, f]).
step_right([2, 30/118, f], [2, 31/118, f]).
step_right([2, 31/0, f], [2, 32/0, f]).
step_right([2, 31/8, f], [2, 32/8, f]).
step_right([2, 31/10, f], [2, 32/10, f]).
step_right([2, 31/12, f], [2, 32/12, f]).
step_right([2, 31/16, f], [2, 32/16, f]).
step_right([2, 31/18, f], [2, 32/18, f]).
step_right([2, 31/26, f], [2, 32/26, f]).
step_right([2, 31/28, f], [2, 32/28, f]).
step_right([2, 31/32, f], [2, 32/32, f]).
step_right([2, 31/34, f], [2, 32/34, f]).
step_right([2, 31/36, f], [2, 32/36, f]).
step_right([2, 31/38, f], [2, 32/38, f]).
step_right([2, 31/48, f], [2, 32/48, f]).
step_right([2, 31/50, f], [2, 32/50, f]).
step_right([2, 31/56, f], [2, 32/56, f]).
step_right([2, 31/58, f], [2, 32/58, f]).
step_right([2, 31/60, f], [2, 32/60, f]).
step_right([2, 31/62, f], [2, 32/62, f]).
step_right([2, 31/64, f], [2, 32/64, f]).
step_right([2, 31/66, f], [2, 32/66, f]).
step_right([2, 31/68, f], [2, 32/68, f]).
step_right([2, 31/70, f], [2, 32/70, f]).
step_right([2, 31/72, f], [2, 32/72, f]).
step_right([2, 31/76, f], [2, 32/76, f]).
step_right([2, 31/80, f], [2, 32/80, f]).
step_right([2, 31/82, f], [2, 32/82, f]).
step_right([2, 31/84, f], [2, 32/84, f]).
step_right([2, 31/86, f], [2, 32/86, f]).
step_right([2, 31/92, f], [2, 32/92, f]).
step_right([2, 31/94, f], [2, 32/94, f]).
step_right([2, 31/96, f], [2, 32/96, f]).
step_right([2, 31/102, f], [2, 32/102, f]).
step_right([2, 31/108, f], [2, 32/108, f]).
step_right([2, 31/118, f], [2, 32/118, f]).
step_right([2, 32/0, f], [2, 33/0, f]).
step_right([2, 32/4, f], [2, 33/4, f]).
step_right([2, 32/8, f], [2, 33/8, f]).
step_right([2, 32/10, f], [2, 33/10, f]).
step_right([2, 32/18, f], [2, 33/18, f]).
step_right([2, 32/20, f], [2, 33/20, f]).
step_right([2, 32/22, f], [2, 33/22, f]).
step_right([2, 32/24, f], [2, 33/24, f]).
step_right([2, 32/28, f], [2, 33/28, f]).
step_right([2, 32/30, f], [2, 33/30, f]).
step_right([2, 32/32, f], [2, 33/32, f]).
step_right([2, 32/34, f], [2, 33/34, f]).
step_right([2, 32/36, f], [2, 33/36, f]).
step_right([2, 32/44, f], [2, 33/44, f]).
step_right([2, 32/46, f], [2, 33/46, f]).
step_right([2, 32/48, f], [2, 33/48, f]).
step_right([2, 32/54, f], [2, 33/54, f]).
step_right([2, 32/60, f], [2, 33/60, f]).
step_right([2, 32/62, f], [2, 33/62, f]).
step_right([2, 32/64, f], [2, 33/64, f]).
step_right([2, 32/68, f], [2, 33/68, f]).
step_right([2, 32/70, f], [2, 33/70, f]).
step_right([2, 32/74, f], [2, 33/74, f]).
step_right([2, 32/78, f], [2, 33/78, f]).
step_right([2, 32/80, f], [2, 33/80, f]).
step_right([2, 32/82, f], [2, 33/82, f]).
step_right([2, 32/84, f], [2, 33/84, f]).
step_right([2, 32/98, f], [2, 33/98, f]).
step_right([2, 32/100, f], [2, 33/100, f]).
step_right([2, 32/102, f], [2, 33/102, f]).
step_right([2, 32/104, f], [2, 33/104, f]).
step_right([2, 32/110, f], [2, 33/110, f]).
step_right([2, 32/112, f], [2, 33/112, f]).
step_right([2, 32/116, f], [2, 33/116, f]).
step_right([2, 32/118, f], [2, 33/118, f]).
step_right([2, 33/0, f], [2, 34/0, f]).
step_right([2, 33/4, f], [2, 34/4, f]).
step_right([2, 33/8, f], [2, 34/8, f]).
step_right([2, 33/10, f], [2, 34/10, f]).
step_right([2, 33/18, f], [2, 34/18, f]).
step_right([2, 33/20, f], [2, 34/20, f]).
step_right([2, 33/22, f], [2, 34/22, f]).
step_right([2, 33/24, f], [2, 34/24, f]).
step_right([2, 33/28, f], [2, 34/28, f]).
step_right([2, 33/30, f], [2, 34/30, f]).
step_right([2, 33/32, f], [2, 34/32, f]).
step_right([2, 33/34, f], [2, 34/34, f]).
step_right([2, 33/36, f], [2, 34/36, f]).
step_right([2, 33/44, f], [2, 34/44, f]).
step_right([2, 33/46, f], [2, 34/46, f]).
step_right([2, 33/48, f], [2, 34/48, f]).
step_right([2, 33/54, f], [2, 34/54, f]).
step_right([2, 33/60, f], [2, 34/60, f]).
step_right([2, 33/62, f], [2, 34/62, f]).
step_right([2, 33/64, f], [2, 34/64, f]).
step_right([2, 33/68, f], [2, 34/68, f]).
step_right([2, 33/70, f], [2, 34/70, f]).
step_right([2, 33/74, f], [2, 34/74, f]).
step_right([2, 33/78, f], [2, 34/78, f]).
step_right([2, 33/80, f], [2, 34/80, f]).
step_right([2, 33/82, f], [2, 34/82, f]).
step_right([2, 33/84, f], [2, 34/84, f]).
step_right([2, 33/98, f], [2, 34/98, f]).
step_right([2, 33/100, f], [2, 34/100, f]).
step_right([2, 33/102, f], [2, 34/102, f]).
step_right([2, 33/104, f], [2, 34/104, f]).
step_right([2, 33/110, f], [2, 34/110, f]).
step_right([2, 33/112, f], [2, 34/112, f]).
step_right([2, 33/116, f], [2, 34/116, f]).
step_right([2, 33/118, f], [2, 34/118, f]).
step_right([2, 34/2, f], [2, 35/2, f]).
step_right([2, 34/4, f], [2, 35/4, f]).
step_right([2, 34/6, f], [2, 35/6, f]).
step_right([2, 34/10, f], [2, 35/10, f]).
step_right([2, 34/12, f], [2, 35/12, f]).
step_right([2, 34/14, f], [2, 35/14, f]).
step_right([2, 34/20, f], [2, 35/20, f]).
step_right([2, 34/22, f], [2, 35/22, f]).
step_right([2, 34/24, f], [2, 35/24, f]).
step_right([2, 34/26, f], [2, 35/26, f]).
step_right([2, 34/28, f], [2, 35/28, f]).
step_right([2, 34/32, f], [2, 35/32, f]).
step_right([2, 34/34, f], [2, 35/34, f]).
step_right([2, 34/36, f], [2, 35/36, f]).
step_right([2, 34/40, f], [2, 35/40, f]).
step_right([2, 34/50, f], [2, 35/50, f]).
step_right([2, 34/52, f], [2, 35/52, f]).
step_right([2, 34/54, f], [2, 35/54, f]).
step_right([2, 34/56, f], [2, 35/56, f]).
step_right([2, 34/58, f], [2, 35/58, f]).
step_right([2, 34/60, f], [2, 35/60, f]).
step_right([2, 34/62, f], [2, 35/62, f]).
step_right([2, 34/64, f], [2, 35/64, f]).
step_right([2, 34/70, f], [2, 35/70, f]).
step_right([2, 34/72, f], [2, 35/72, f]).
step_right([2, 34/76, f], [2, 35/76, f]).
step_right([2, 34/78, f], [2, 35/78, f]).
step_right([2, 34/82, f], [2, 35/82, f]).
step_right([2, 34/84, f], [2, 35/84, f]).
step_right([2, 34/88, f], [2, 35/88, f]).
step_right([2, 34/90, f], [2, 35/90, f]).
step_right([2, 34/94, f], [2, 35/94, f]).
step_right([2, 34/96, f], [2, 35/96, f]).
step_right([2, 34/98, f], [2, 35/98, f]).
step_right([2, 34/102, f], [2, 35/102, f]).
step_right([2, 34/104, f], [2, 35/104, f]).
step_right([2, 34/114, f], [2, 35/114, f]).
step_right([2, 34/116, f], [2, 35/116, f]).
step_right([2, 34/118, f], [2, 35/118, f]).
step_right([2, 35/2, f], [2, 36/2, f]).
step_right([2, 35/4, f], [2, 36/4, f]).
step_right([2, 35/6, f], [2, 36/6, f]).
step_right([2, 35/10, f], [2, 36/10, f]).
step_right([2, 35/12, f], [2, 36/12, f]).
step_right([2, 35/14, f], [2, 36/14, f]).
step_right([2, 35/20, f], [2, 36/20, f]).
step_right([2, 35/22, f], [2, 36/22, f]).
step_right([2, 35/24, f], [2, 36/24, f]).
step_right([2, 35/26, f], [2, 36/26, f]).
step_right([2, 35/28, f], [2, 36/28, f]).
step_right([2, 35/32, f], [2, 36/32, f]).
step_right([2, 35/34, f], [2, 36/34, f]).
step_right([2, 35/36, f], [2, 36/36, f]).
step_right([2, 35/40, f], [2, 36/40, f]).
step_right([2, 35/50, f], [2, 36/50, f]).
step_right([2, 35/52, f], [2, 36/52, f]).
step_right([2, 35/54, f], [2, 36/54, f]).
step_right([2, 35/56, f], [2, 36/56, f]).
step_right([2, 35/58, f], [2, 36/58, f]).
step_right([2, 35/60, f], [2, 36/60, f]).
step_right([2, 35/62, f], [2, 36/62, f]).
step_right([2, 35/64, f], [2, 36/64, f]).
step_right([2, 35/70, f], [2, 36/70, f]).
step_right([2, 35/72, f], [2, 36/72, f]).
step_right([2, 35/76, f], [2, 36/76, f]).
step_right([2, 35/78, f], [2, 36/78, f]).
step_right([2, 35/82, f], [2, 36/82, f]).
step_right([2, 35/84, f], [2, 36/84, f]).
step_right([2, 35/88, f], [2, 36/88, f]).
step_right([2, 35/90, f], [2, 36/90, f]).
step_right([2, 35/94, f], [2, 36/94, f]).
step_right([2, 35/96, f], [2, 36/96, f]).
step_right([2, 35/98, f], [2, 36/98, f]).
step_right([2, 35/102, f], [2, 36/102, f]).
step_right([2, 35/104, f], [2, 36/104, f]).
step_right([2, 35/114, f], [2, 36/114, f]).
step_right([2, 35/116, f], [2, 36/116, f]).
step_right([2, 35/118, f], [2, 36/118, f]).
step_right([2, 36/0, f], [2, 37/0, f]).
step_right([2, 36/2, f], [2, 37/2, f]).
step_right([2, 36/8, f], [2, 37/8, f]).
step_right([2, 36/16, f], [2, 37/16, f]).
step_right([2, 36/18, f], [2, 37/18, f]).
step_right([2, 36/26, f], [2, 37/26, f]).
step_right([2, 36/28, f], [2, 37/28, f]).
step_right([2, 36/30, f], [2, 37/30, f]).
step_right([2, 36/34, f], [2, 37/34, f]).
step_right([2, 36/38, f], [2, 37/38, f]).
step_right([2, 36/52, f], [2, 37/52, f]).
step_right([2, 36/56, f], [2, 37/56, f]).
step_right([2, 36/72, f], [2, 37/72, f]).
step_right([2, 36/74, f], [2, 37/74, f]).
step_right([2, 36/80, f], [2, 37/80, f]).
step_right([2, 36/82, f], [2, 37/82, f]).
step_right([2, 36/86, f], [2, 37/86, f]).
step_right([2, 36/88, f], [2, 37/88, f]).
step_right([2, 36/90, f], [2, 37/90, f]).
step_right([2, 36/94, f], [2, 37/94, f]).
step_right([2, 36/98, f], [2, 37/98, f]).
step_right([2, 36/100, f], [2, 37/100, f]).
step_right([2, 36/104, f], [2, 37/104, f]).
step_right([2, 36/106, f], [2, 37/106, f]).
step_right([2, 36/108, f], [2, 37/108, f]).
step_right([2, 36/110, f], [2, 37/110, f]).
step_right([2, 36/112, f], [2, 37/112, f]).
step_right([2, 36/118, f], [2, 37/118, f]).
step_right([2, 37/0, f], [2, 38/0, f]).
step_right([2, 37/2, f], [2, 38/2, f]).
step_right([2, 37/8, f], [2, 38/8, f]).
step_right([2, 37/16, f], [2, 38/16, f]).
step_right([2, 37/18, f], [2, 38/18, f]).
step_right([2, 37/26, f], [2, 38/26, f]).
step_right([2, 37/28, f], [2, 38/28, f]).
step_right([2, 37/30, f], [2, 38/30, f]).
step_right([2, 37/34, f], [2, 38/34, f]).
step_right([2, 37/38, f], [2, 38/38, f]).
step_right([2, 37/52, f], [2, 38/52, f]).
step_right([2, 37/56, f], [2, 38/56, f]).
step_right([2, 37/72, f], [2, 38/72, f]).
step_right([2, 37/74, f], [2, 38/74, f]).
step_right([2, 37/80, f], [2, 38/80, f]).
step_right([2, 37/82, f], [2, 38/82, f]).
step_right([2, 37/86, f], [2, 38/86, f]).
step_right([2, 37/88, f], [2, 38/88, f]).
step_right([2, 37/90, f], [2, 38/90, f]).
step_right([2, 37/94, f], [2, 38/94, f]).
step_right([2, 37/98, f], [2, 38/98, f]).
step_right([2, 37/100, f], [2, 38/100, f]).
step_right([2, 37/104, f], [2, 38/104, f]).
step_right([2, 37/106, f], [2, 38/106, f]).
step_right([2, 37/108, f], [2, 38/108, f]).
step_right([2, 37/110, f], [2, 38/110, f]).
step_right([2, 37/112, f], [2, 38/112, f]).
step_right([2, 37/118, f], [2, 38/118, f]).
step_right([2, 38/0, f], [2, 39/0, f]).
step_right([2, 38/2, f], [2, 39/2, f]).
step_right([2, 38/4, f], [2, 39/4, f]).
step_right([2, 38/8, f], [2, 39/8, f]).
step_right([2, 38/10, f], [2, 39/10, f]).
step_right([2, 38/12, f], [2, 39/12, f]).
step_right([2, 38/14, f], [2, 39/14, f]).
step_right([2, 38/18, f], [2, 39/18, f]).
step_right([2, 38/20, f], [2, 39/20, f]).
step_right([2, 38/22, f], [2, 39/22, f]).
step_right([2, 38/24, f], [2, 39/24, f]).
step_right([2, 38/30, f], [2, 39/30, f]).
step_right([2, 38/32, f], [2, 39/32, f]).
step_right([2, 38/34, f], [2, 39/34, f]).
step_right([2, 38/36, f], [2, 39/36, f]).
step_right([2, 38/44, f], [2, 39/44, f]).
step_right([2, 38/50, f], [2, 39/50, f]).
step_right([2, 38/54, f], [2, 39/54, f]).
step_right([2, 38/56, f], [2, 39/56, f]).
step_right([2, 38/58, f], [2, 39/58, f]).
step_right([2, 38/70, f], [2, 39/70, f]).
step_right([2, 38/72, f], [2, 39/72, f]).
step_right([2, 38/74, f], [2, 39/74, f]).
step_right([2, 38/76, f], [2, 39/76, f]).
step_right([2, 38/82, f], [2, 39/82, f]).
step_right([2, 38/84, f], [2, 39/84, f]).
step_right([2, 38/88, f], [2, 39/88, f]).
step_right([2, 38/92, f], [2, 39/92, f]).
step_right([2, 38/102, f], [2, 39/102, f]).
step_right([2, 38/108, f], [2, 39/108, f]).
step_right([2, 38/110, f], [2, 39/110, f]).
step_right([2, 38/114, f], [2, 39/114, f]).
step_right([2, 38/116, f], [2, 39/116, f]).
step_right([2, 38/118, f], [2, 39/118, f]).
step_right([2, 39/0, f], [2, 40/0, f]).
step_right([2, 39/2, f], [2, 40/2, f]).
step_right([2, 39/4, f], [2, 40/4, f]).
step_right([2, 39/8, f], [2, 40/8, f]).
step_right([2, 39/10, f], [2, 40/10, f]).
step_right([2, 39/12, f], [2, 40/12, f]).
step_right([2, 39/14, f], [2, 40/14, f]).
step_right([2, 39/18, f], [2, 40/18, f]).
step_right([2, 39/20, f], [2, 40/20, f]).
step_right([2, 39/22, f], [2, 40/22, f]).
step_right([2, 39/24, f], [2, 40/24, f]).
step_right([2, 39/30, f], [2, 40/30, f]).
step_right([2, 39/32, f], [2, 40/32, f]).
step_right([2, 39/34, f], [2, 40/34, f]).
step_right([2, 39/36, f], [2, 40/36, f]).
step_right([2, 39/44, f], [2, 40/44, f]).
step_right([2, 39/50, f], [2, 40/50, f]).
step_right([2, 39/54, f], [2, 40/54, f]).
step_right([2, 39/56, f], [2, 40/56, f]).
step_right([2, 39/58, f], [2, 40/58, f]).
step_right([2, 39/70, f], [2, 40/70, f]).
step_right([2, 39/72, f], [2, 40/72, f]).
step_right([2, 39/74, f], [2, 40/74, f]).
step_right([2, 39/76, f], [2, 40/76, f]).
step_right([2, 39/82, f], [2, 40/82, f]).
step_right([2, 39/84, f], [2, 40/84, f]).
step_right([2, 39/88, f], [2, 40/88, f]).
step_right([2, 39/92, f], [2, 40/92, f]).
step_right([2, 39/102, f], [2, 40/102, f]).
step_right([2, 39/108, f], [2, 40/108, f]).
step_right([2, 39/110, f], [2, 40/110, f]).
step_right([2, 39/114, f], [2, 40/114, f]).
step_right([2, 39/116, f], [2, 40/116, f]).
step_right([2, 39/118, f], [2, 40/118, f]).
step_right([2, 40/0, f], [2, 41/0, f]).
step_right([2, 40/2, f], [2, 41/2, f]).
step_right([2, 40/4, f], [2, 41/4, f]).
step_right([2, 40/6, f], [2, 41/6, f]).
step_right([2, 40/8, f], [2, 41/8, f]).
step_right([2, 40/10, f], [2, 41/10, f]).
step_right([2, 40/12, f], [2, 41/12, f]).
step_right([2, 40/14, f], [2, 41/14, f]).
step_right([2, 40/16, f], [2, 41/16, f]).
step_right([2, 40/20, f], [2, 41/20, f]).
step_right([2, 40/28, f], [2, 41/28, f]).
step_right([2, 40/32, f], [2, 41/32, f]).
step_right([2, 40/36, f], [2, 41/36, f]).
step_right([2, 40/38, f], [2, 41/38, f]).
step_right([2, 40/40, f], [2, 41/40, f]).
step_right([2, 40/48, f], [2, 41/48, f]).
step_right([2, 40/52, f], [2, 41/52, f]).
step_right([2, 40/54, f], [2, 41/54, f]).
step_right([2, 40/56, f], [2, 41/56, f]).
step_right([2, 40/60, f], [2, 41/60, f]).
step_right([2, 40/72, f], [2, 41/72, f]).
step_right([2, 40/76, f], [2, 41/76, f]).
step_right([2, 40/78, f], [2, 41/78, f]).
step_right([2, 40/80, f], [2, 41/80, f]).
step_right([2, 40/82, f], [2, 41/82, f]).
step_right([2, 40/86, f], [2, 41/86, f]).
step_right([2, 40/90, f], [2, 41/90, f]).
step_right([2, 40/96, f], [2, 41/96, f]).
step_right([2, 40/98, f], [2, 41/98, f]).
step_right([2, 40/104, f], [2, 41/104, f]).
step_right([2, 40/106, f], [2, 41/106, f]).
step_right([2, 40/110, f], [2, 41/110, f]).
step_right([2, 40/112, f], [2, 41/112, f]).
step_right([2, 41/0, f], [2, 42/0, f]).
step_right([2, 41/2, f], [2, 42/2, f]).
step_right([2, 41/4, f], [2, 42/4, f]).
step_right([2, 41/6, f], [2, 42/6, f]).
step_right([2, 41/8, f], [2, 42/8, f]).
step_right([2, 41/10, f], [2, 42/10, f]).
step_right([2, 41/12, f], [2, 42/12, f]).
step_right([2, 41/14, f], [2, 42/14, f]).
step_right([2, 41/16, f], [2, 42/16, f]).
step_right([2, 41/20, f], [2, 42/20, f]).
step_right([2, 41/28, f], [2, 42/28, f]).
step_right([2, 41/32, f], [2, 42/32, f]).
step_right([2, 41/36, f], [2, 42/36, f]).
step_right([2, 41/38, f], [2, 42/38, f]).
step_right([2, 41/40, f], [2, 42/40, f]).
step_right([2, 41/48, f], [2, 42/48, f]).
step_right([2, 41/52, f], [2, 42/52, f]).
step_right([2, 41/54, f], [2, 42/54, f]).
step_right([2, 41/56, f], [2, 42/56, f]).
step_right([2, 41/60, f], [2, 42/60, f]).
step_right([2, 41/72, f], [2, 42/72, f]).
step_right([2, 41/76, f], [2, 42/76, f]).
step_right([2, 41/78, f], [2, 42/78, f]).
step_right([2, 41/80, f], [2, 42/80, f]).
step_right([2, 41/82, f], [2, 42/82, f]).
step_right([2, 41/86, f], [2, 42/86, f]).
step_right([2, 41/90, f], [2, 42/90, f]).
step_right([2, 41/96, f], [2, 42/96, f]).
step_right([2, 41/98, f], [2, 42/98, f]).
step_right([2, 41/104, f], [2, 42/104, f]).
step_right([2, 41/106, f], [2, 42/106, f]).
step_right([2, 41/110, f], [2, 42/110, f]).
step_right([2, 41/112, f], [2, 42/112, f]).
step_right([2, 42/0, f], [2, 43/0, f]).
step_right([2, 42/6, f], [2, 43/6, f]).
step_right([2, 42/8, f], [2, 43/8, f]).
step_right([2, 42/10, f], [2, 43/10, f]).
step_right([2, 42/12, f], [2, 43/12, f]).
step_right([2, 42/14, f], [2, 43/14, f]).
step_right([2, 42/16, f], [2, 43/16, f]).
step_right([2, 42/18, f], [2, 43/18, f]).
step_right([2, 42/26, f], [2, 43/26, f]).
step_right([2, 42/32, f], [2, 43/32, f]).
step_right([2, 42/34, f], [2, 43/34, f]).
step_right([2, 42/36, f], [2, 43/36, f]).
step_right([2, 42/38, f], [2, 43/38, f]).
step_right([2, 42/46, f], [2, 43/46, f]).
step_right([2, 42/50, f], [2, 43/50, f]).
step_right([2, 42/52, f], [2, 43/52, f]).
step_right([2, 42/54, f], [2, 43/54, f]).
step_right([2, 42/56, f], [2, 43/56, f]).
step_right([2, 42/60, f], [2, 43/60, f]).
step_right([2, 42/62, f], [2, 43/62, f]).
step_right([2, 42/68, f], [2, 43/68, f]).
step_right([2, 42/72, f], [2, 43/72, f]).
step_right([2, 42/74, f], [2, 43/74, f]).
step_right([2, 42/86, f], [2, 43/86, f]).
step_right([2, 42/88, f], [2, 43/88, f]).
step_right([2, 42/92, f], [2, 43/92, f]).
step_right([2, 42/94, f], [2, 43/94, f]).
step_right([2, 42/96, f], [2, 43/96, f]).
step_right([2, 42/98, f], [2, 43/98, f]).
step_right([2, 42/102, f], [2, 43/102, f]).
step_right([2, 42/108, f], [2, 43/108, f]).
step_right([2, 42/110, f], [2, 43/110, f]).
step_right([2, 42/112, f], [2, 43/112, f]).
step_right([2, 42/114, f], [2, 43/114, f]).
step_right([2, 42/118, f], [2, 43/118, f]).
step_right([2, 43/0, f], [2, 44/0, f]).
step_right([2, 43/6, f], [2, 44/6, f]).
step_right([2, 43/8, f], [2, 44/8, f]).
step_right([2, 43/10, f], [2, 44/10, f]).
step_right([2, 43/12, f], [2, 44/12, f]).
step_right([2, 43/14, f], [2, 44/14, f]).
step_right([2, 43/16, f], [2, 44/16, f]).
step_right([2, 43/18, f], [2, 44/18, f]).
step_right([2, 43/26, f], [2, 44/26, f]).
step_right([2, 43/32, f], [2, 44/32, f]).
step_right([2, 43/34, f], [2, 44/34, f]).
step_right([2, 43/36, f], [2, 44/36, f]).
step_right([2, 43/38, f], [2, 44/38, f]).
step_right([2, 43/46, f], [2, 44/46, f]).
step_right([2, 43/50, f], [2, 44/50, f]).
step_right([2, 43/52, f], [2, 44/52, f]).
step_right([2, 43/54, f], [2, 44/54, f]).
step_right([2, 43/56, f], [2, 44/56, f]).
step_right([2, 43/60, f], [2, 44/60, f]).
step_right([2, 43/62, f], [2, 44/62, f]).
step_right([2, 43/68, f], [2, 44/68, f]).
step_right([2, 43/72, f], [2, 44/72, f]).
step_right([2, 43/74, f], [2, 44/74, f]).
step_right([2, 43/86, f], [2, 44/86, f]).
step_right([2, 43/88, f], [2, 44/88, f]).
step_right([2, 43/92, f], [2, 44/92, f]).
step_right([2, 43/94, f], [2, 44/94, f]).
step_right([2, 43/96, f], [2, 44/96, f]).
step_right([2, 43/98, f], [2, 44/98, f]).
step_right([2, 43/102, f], [2, 44/102, f]).
step_right([2, 43/108, f], [2, 44/108, f]).
step_right([2, 43/110, f], [2, 44/110, f]).
step_right([2, 43/112, f], [2, 44/112, f]).
step_right([2, 43/114, f], [2, 44/114, f]).
step_right([2, 43/118, f], [2, 44/118, f]).
step_right([2, 44/4, f], [2, 45/4, f]).
step_right([2, 44/6, f], [2, 45/6, f]).
step_right([2, 44/10, f], [2, 45/10, f]).
step_right([2, 44/12, f], [2, 45/12, f]).
step_right([2, 44/16, f], [2, 45/16, f]).
step_right([2, 44/20, f], [2, 45/20, f]).
step_right([2, 44/28, f], [2, 45/28, f]).
step_right([2, 44/30, f], [2, 45/30, f]).
step_right([2, 44/32, f], [2, 45/32, f]).
step_right([2, 44/44, f], [2, 45/44, f]).
step_right([2, 44/46, f], [2, 45/46, f]).
step_right([2, 44/48, f], [2, 45/48, f]).
step_right([2, 44/50, f], [2, 45/50, f]).
step_right([2, 44/52, f], [2, 45/52, f]).
step_right([2, 44/56, f], [2, 45/56, f]).
step_right([2, 44/58, f], [2, 45/58, f]).
step_right([2, 44/62, f], [2, 45/62, f]).
step_right([2, 44/64, f], [2, 45/64, f]).
step_right([2, 44/68, f], [2, 45/68, f]).
step_right([2, 44/70, f], [2, 45/70, f]).
step_right([2, 44/74, f], [2, 45/74, f]).
step_right([2, 44/78, f], [2, 45/78, f]).
step_right([2, 44/80, f], [2, 45/80, f]).
step_right([2, 44/82, f], [2, 45/82, f]).
step_right([2, 44/88, f], [2, 45/88, f]).
step_right([2, 44/92, f], [2, 45/92, f]).
step_right([2, 44/94, f], [2, 45/94, f]).
step_right([2, 44/96, f], [2, 45/96, f]).
step_right([2, 44/100, f], [2, 45/100, f]).
step_right([2, 44/102, f], [2, 45/102, f]).
step_right([2, 44/104, f], [2, 45/104, f]).
step_right([2, 44/110, f], [2, 45/110, f]).
step_right([2, 44/118, f], [2, 45/118, f]).
step_right([2, 45/4, f], [2, 46/4, f]).
step_right([2, 45/6, f], [2, 46/6, f]).
step_right([2, 45/10, f], [2, 46/10, f]).
step_right([2, 45/12, f], [2, 46/12, f]).
step_right([2, 45/16, f], [2, 46/16, f]).
step_right([2, 45/20, f], [2, 46/20, f]).
step_right([2, 45/28, f], [2, 46/28, f]).
step_right([2, 45/30, f], [2, 46/30, f]).
step_right([2, 45/32, f], [2, 46/32, f]).
step_right([2, 45/44, f], [2, 46/44, f]).
step_right([2, 45/46, f], [2, 46/46, f]).
step_right([2, 45/48, f], [2, 46/48, f]).
step_right([2, 45/50, f], [2, 46/50, f]).
step_right([2, 45/52, f], [2, 46/52, f]).
step_right([2, 45/56, f], [2, 46/56, f]).
step_right([2, 45/58, f], [2, 46/58, f]).
step_right([2, 45/62, f], [2, 46/62, f]).
step_right([2, 45/64, f], [2, 46/64, f]).
step_right([2, 45/68, f], [2, 46/68, f]).
step_right([2, 45/70, f], [2, 46/70, f]).
step_right([2, 45/74, f], [2, 46/74, f]).
step_right([2, 45/78, f], [2, 46/78, f]).
step_right([2, 45/80, f], [2, 46/80, f]).
step_right([2, 45/82, f], [2, 46/82, f]).
step_right([2, 45/88, f], [2, 46/88, f]).
step_right([2, 45/92, f], [2, 46/92, f]).
step_right([2, 45/94, f], [2, 46/94, f]).
step_right([2, 45/96, f], [2, 46/96, f]).
step_right([2, 45/100, f], [2, 46/100, f]).
step_right([2, 45/102, f], [2, 46/102, f]).
step_right([2, 45/104, f], [2, 46/104, f]).
step_right([2, 45/110, f], [2, 46/110, f]).
step_right([2, 45/118, f], [2, 46/118, f]).
step_right([2, 46/0, f], [2, 47/0, f]).
step_right([2, 46/4, f], [2, 47/4, f]).
step_right([2, 46/14, f], [2, 47/14, f]).
step_right([2, 46/18, f], [2, 47/18, f]).
step_right([2, 46/30, f], [2, 47/30, f]).
step_right([2, 46/34, f], [2, 47/34, f]).
step_right([2, 46/46, f], [2, 47/46, f]).
step_right([2, 46/48, f], [2, 47/48, f]).
step_right([2, 46/50, f], [2, 47/50, f]).
step_right([2, 46/54, f], [2, 47/54, f]).
step_right([2, 46/58, f], [2, 47/58, f]).
step_right([2, 46/60, f], [2, 47/60, f]).
step_right([2, 46/64, f], [2, 47/64, f]).
step_right([2, 46/70, f], [2, 47/70, f]).
step_right([2, 46/72, f], [2, 47/72, f]).
step_right([2, 46/74, f], [2, 47/74, f]).
step_right([2, 46/76, f], [2, 47/76, f]).
step_right([2, 46/84, f], [2, 47/84, f]).
step_right([2, 46/86, f], [2, 47/86, f]).
step_right([2, 46/90, f], [2, 47/90, f]).
step_right([2, 46/92, f], [2, 47/92, f]).
step_right([2, 46/94, f], [2, 47/94, f]).
step_right([2, 46/98, f], [2, 47/98, f]).
step_right([2, 46/106, f], [2, 47/106, f]).
step_right([2, 46/108, f], [2, 47/108, f]).
step_right([2, 46/116, f], [2, 47/116, f]).
step_right([2, 46/118, f], [2, 47/118, f]).
step_right([2, 47/0, f], [2, 48/0, f]).
step_right([2, 47/4, f], [2, 48/4, f]).
step_right([2, 47/14, f], [2, 48/14, f]).
step_right([2, 47/18, f], [2, 48/18, f]).
step_right([2, 47/30, f], [2, 48/30, f]).
step_right([2, 47/34, f], [2, 48/34, f]).
step_right([2, 47/46, f], [2, 48/46, f]).
step_right([2, 47/48, f], [2, 48/48, f]).
step_right([2, 47/50, f], [2, 48/50, f]).
step_right([2, 47/54, f], [2, 48/54, f]).
step_right([2, 47/58, f], [2, 48/58, f]).
step_right([2, 47/60, f], [2, 48/60, f]).
step_right([2, 47/64, f], [2, 48/64, f]).
step_right([2, 47/70, f], [2, 48/70, f]).
step_right([2, 47/72, f], [2, 48/72, f]).
step_right([2, 47/74, f], [2, 48/74, f]).
step_right([2, 47/76, f], [2, 48/76, f]).
step_right([2, 47/84, f], [2, 48/84, f]).
step_right([2, 47/86, f], [2, 48/86, f]).
step_right([2, 47/90, f], [2, 48/90, f]).
step_right([2, 47/92, f], [2, 48/92, f]).
step_right([2, 47/94, f], [2, 48/94, f]).
step_right([2, 47/98, f], [2, 48/98, f]).
step_right([2, 47/106, f], [2, 48/106, f]).
step_right([2, 47/108, f], [2, 48/108, f]).
step_right([2, 47/116, f], [2, 48/116, f]).
step_right([2, 47/118, f], [2, 48/118, f]).
step_right([2, 48/2, f], [2, 49/2, f]).
step_right([2, 48/10, f], [2, 49/10, f]).
step_right([2, 48/12, f], [2, 49/12, f]).
step_right([2, 48/16, f], [2, 49/16, f]).
step_right([2, 48/22, f], [2, 49/22, f]).
step_right([2, 48/24, f], [2, 49/24, f]).
step_right([2, 48/26, f], [2, 49/26, f]).
step_right([2, 48/32, f], [2, 49/32, f]).
step_right([2, 48/40, f], [2, 49/40, f]).
step_right([2, 48/42, f], [2, 49/42, f]).
step_right([2, 48/44, f], [2, 49/44, f]).
step_right([2, 48/50, f], [2, 49/50, f]).
step_right([2, 48/52, f], [2, 49/52, f]).
step_right([2, 48/56, f], [2, 49/56, f]).
step_right([2, 48/62, f], [2, 49/62, f]).
step_right([2, 48/66, f], [2, 49/66, f]).
step_right([2, 48/68, f], [2, 49/68, f]).
step_right([2, 48/72, f], [2, 49/72, f]).
step_right([2, 48/78, f], [2, 49/78, f]).
step_right([2, 48/80, f], [2, 49/80, f]).
step_right([2, 48/84, f], [2, 49/84, f]).
step_right([2, 48/88, f], [2, 49/88, f]).
step_right([2, 48/92, f], [2, 49/92, f]).
step_right([2, 48/96, f], [2, 49/96, f]).
step_right([2, 48/98, f], [2, 49/98, f]).
step_right([2, 48/100, f], [2, 49/100, f]).
step_right([2, 48/112, f], [2, 49/112, f]).
step_right([2, 48/114, f], [2, 49/114, f]).
step_right([2, 48/118, f], [2, 49/118, f]).
step_right([2, 49/2, f], [2, 50/2, f]).
step_right([2, 49/10, f], [2, 50/10, f]).
step_right([2, 49/12, f], [2, 50/12, f]).
step_right([2, 49/16, f], [2, 50/16, f]).
step_right([2, 49/22, f], [2, 50/22, f]).
step_right([2, 49/24, f], [2, 50/24, f]).
step_right([2, 49/26, f], [2, 50/26, f]).
step_right([2, 49/32, f], [2, 50/32, f]).
step_right([2, 49/40, f], [2, 50/40, f]).
step_right([2, 49/42, f], [2, 50/42, f]).
step_right([2, 49/44, f], [2, 50/44, f]).
step_right([2, 49/50, f], [2, 50/50, f]).
step_right([2, 49/52, f], [2, 50/52, f]).
step_right([2, 49/56, f], [2, 50/56, f]).
step_right([2, 49/62, f], [2, 50/62, f]).
step_right([2, 49/66, f], [2, 50/66, f]).
step_right([2, 49/68, f], [2, 50/68, f]).
step_right([2, 49/72, f], [2, 50/72, f]).
step_right([2, 49/78, f], [2, 50/78, f]).
step_right([2, 49/80, f], [2, 50/80, f]).
step_right([2, 49/84, f], [2, 50/84, f]).
step_right([2, 49/88, f], [2, 50/88, f]).
step_right([2, 49/92, f], [2, 50/92, f]).
step_right([2, 49/96, f], [2, 50/96, f]).
step_right([2, 49/98, f], [2, 50/98, f]).
step_right([2, 49/100, f], [2, 50/100, f]).
step_right([2, 49/112, f], [2, 50/112, f]).
step_right([2, 49/114, f], [2, 50/114, f]).
step_right([2, 49/118, f], [2, 50/118, f]).
step_right([2, 50/0, f], [2, 51/0, f]).
step_right([2, 50/6, f], [2, 51/6, f]).
step_right([2, 50/8, f], [2, 51/8, f]).
step_right([2, 50/14, f], [2, 51/14, f]).
step_right([2, 50/24, f], [2, 51/24, f]).
step_right([2, 50/28, f], [2, 51/28, f]).
step_right([2, 50/30, f], [2, 51/30, f]).
step_right([2, 50/34, f], [2, 51/34, f]).
step_right([2, 50/44, f], [2, 51/44, f]).
step_right([2, 50/46, f], [2, 51/46, f]).
step_right([2, 50/58, f], [2, 51/58, f]).
step_right([2, 50/60, f], [2, 51/60, f]).
step_right([2, 50/62, f], [2, 51/62, f]).
step_right([2, 50/64, f], [2, 51/64, f]).
step_right([2, 50/68, f], [2, 51/68, f]).
step_right([2, 50/70, f], [2, 51/70, f]).
step_right([2, 50/74, f], [2, 51/74, f]).
step_right([2, 50/82, f], [2, 51/82, f]).
step_right([2, 50/86, f], [2, 51/86, f]).
step_right([2, 50/88, f], [2, 51/88, f]).
step_right([2, 50/92, f], [2, 51/92, f]).
step_right([2, 50/94, f], [2, 51/94, f]).
step_right([2, 50/96, f], [2, 51/96, f]).
step_right([2, 50/102, f], [2, 51/102, f]).
step_right([2, 50/108, f], [2, 51/108, f]).
step_right([2, 50/110, f], [2, 51/110, f]).
step_right([2, 50/114, f], [2, 51/114, f]).
step_right([2, 50/116, f], [2, 51/116, f]).
step_right([2, 50/118, f], [2, 51/118, f]).
step_right([2, 51/0, f], [2, 52/0, f]).
step_right([2, 51/6, f], [2, 52/6, f]).
step_right([2, 51/8, f], [2, 52/8, f]).
step_right([2, 51/14, f], [2, 52/14, f]).
step_right([2, 51/24, f], [2, 52/24, f]).
step_right([2, 51/28, f], [2, 52/28, f]).
step_right([2, 51/30, f], [2, 52/30, f]).
step_right([2, 51/34, f], [2, 52/34, f]).
step_right([2, 51/44, f], [2, 52/44, f]).
step_right([2, 51/46, f], [2, 52/46, f]).
step_right([2, 51/58, f], [2, 52/58, f]).
step_right([2, 51/60, f], [2, 52/60, f]).
step_right([2, 51/62, f], [2, 52/62, f]).
step_right([2, 51/64, f], [2, 52/64, f]).
step_right([2, 51/68, f], [2, 52/68, f]).
step_right([2, 51/70, f], [2, 52/70, f]).
step_right([2, 51/74, f], [2, 52/74, f]).
step_right([2, 51/82, f], [2, 52/82, f]).
step_right([2, 51/86, f], [2, 52/86, f]).
step_right([2, 51/88, f], [2, 52/88, f]).
step_right([2, 51/92, f], [2, 52/92, f]).
step_right([2, 51/94, f], [2, 52/94, f]).
step_right([2, 51/96, f], [2, 52/96, f]).
step_right([2, 51/102, f], [2, 52/102, f]).
step_right([2, 51/108, f], [2, 52/108, f]).
step_right([2, 51/110, f], [2, 52/110, f]).
step_right([2, 51/114, f], [2, 52/114, f]).
step_right([2, 51/116, f], [2, 52/116, f]).
step_right([2, 51/118, f], [2, 52/118, f]).
step_right([2, 52/0, f], [2, 53/0, f]).
step_right([2, 52/2, f], [2, 53/2, f]).
step_right([2, 52/4, f], [2, 53/4, f]).
step_right([2, 52/6, f], [2, 53/6, f]).
step_right([2, 52/12, f], [2, 53/12, f]).
step_right([2, 52/16, f], [2, 53/16, f]).
step_right([2, 52/20, f], [2, 53/20, f]).
step_right([2, 52/22, f], [2, 53/22, f]).
step_right([2, 52/24, f], [2, 53/24, f]).
step_right([2, 52/26, f], [2, 53/26, f]).
step_right([2, 52/32, f], [2, 53/32, f]).
step_right([2, 52/34, f], [2, 53/34, f]).
step_right([2, 52/36, f], [2, 53/36, f]).
step_right([2, 52/40, f], [2, 53/40, f]).
step_right([2, 52/42, f], [2, 53/42, f]).
step_right([2, 52/44, f], [2, 53/44, f]).
step_right([2, 52/48, f], [2, 53/48, f]).
step_right([2, 52/50, f], [2, 53/50, f]).
step_right([2, 52/52, f], [2, 53/52, f]).
step_right([2, 52/64, f], [2, 53/64, f]).
step_right([2, 52/66, f], [2, 53/66, f]).
step_right([2, 52/76, f], [2, 53/76, f]).
step_right([2, 52/82, f], [2, 53/82, f]).
step_right([2, 52/84, f], [2, 53/84, f]).
step_right([2, 52/90, f], [2, 53/90, f]).
step_right([2, 52/94, f], [2, 53/94, f]).
step_right([2, 52/106, f], [2, 53/106, f]).
step_right([2, 52/108, f], [2, 53/108, f]).
step_right([2, 52/112, f], [2, 53/112, f]).
step_right([2, 52/116, f], [2, 53/116, f]).
step_right([2, 53/0, f], [2, 54/0, f]).
step_right([2, 53/2, f], [2, 54/2, f]).
step_right([2, 53/4, f], [2, 54/4, f]).
step_right([2, 53/6, f], [2, 54/6, f]).
step_right([2, 53/12, f], [2, 54/12, f]).
step_right([2, 53/16, f], [2, 54/16, f]).
step_right([2, 53/20, f], [2, 54/20, f]).
step_right([2, 53/22, f], [2, 54/22, f]).
step_right([2, 53/24, f], [2, 54/24, f]).
step_right([2, 53/26, f], [2, 54/26, f]).
step_right([2, 53/32, f], [2, 54/32, f]).
step_right([2, 53/34, f], [2, 54/34, f]).
step_right([2, 53/36, f], [2, 54/36, f]).
step_right([2, 53/40, f], [2, 54/40, f]).
step_right([2, 53/42, f], [2, 54/42, f]).
step_right([2, 53/44, f], [2, 54/44, f]).
step_right([2, 53/48, f], [2, 54/48, f]).
step_right([2, 53/50, f], [2, 54/50, f]).
step_right([2, 53/52, f], [2, 54/52, f]).
step_right([2, 53/64, f], [2, 54/64, f]).
step_right([2, 53/66, f], [2, 54/66, f]).
step_right([2, 53/76, f], [2, 54/76, f]).
step_right([2, 53/82, f], [2, 54/82, f]).
step_right([2, 53/84, f], [2, 54/84, f]).
step_right([2, 53/90, f], [2, 54/90, f]).
step_right([2, 53/94, f], [2, 54/94, f]).
step_right([2, 53/106, f], [2, 54/106, f]).
step_right([2, 53/108, f], [2, 54/108, f]).
step_right([2, 53/112, f], [2, 54/112, f]).
step_right([2, 53/116, f], [2, 54/116, f]).
step_right([2, 54/4, f], [2, 55/4, f]).
step_right([2, 54/6, f], [2, 55/6, f]).
step_right([2, 54/8, f], [2, 55/8, f]).
step_right([2, 54/10, f], [2, 55/10, f]).
step_right([2, 54/14, f], [2, 55/14, f]).
step_right([2, 54/18, f], [2, 55/18, f]).
step_right([2, 54/28, f], [2, 55/28, f]).
step_right([2, 54/46, f], [2, 55/46, f]).
step_right([2, 54/50, f], [2, 55/50, f]).
step_right([2, 54/54, f], [2, 55/54, f]).
step_right([2, 54/56, f], [2, 55/56, f]).
step_right([2, 54/60, f], [2, 55/60, f]).
step_right([2, 54/62, f], [2, 55/62, f]).
step_right([2, 54/70, f], [2, 55/70, f]).
step_right([2, 54/76, f], [2, 55/76, f]).
step_right([2, 54/78, f], [2, 55/78, f]).
step_right([2, 54/80, f], [2, 55/80, f]).
step_right([2, 54/84, f], [2, 55/84, f]).
step_right([2, 54/86, f], [2, 55/86, f]).
step_right([2, 54/88, f], [2, 55/88, f]).
step_right([2, 54/92, f], [2, 55/92, f]).
step_right([2, 54/100, f], [2, 55/100, f]).
step_right([2, 54/102, f], [2, 55/102, f]).
step_right([2, 54/108, f], [2, 55/108, f]).
step_right([2, 54/110, f], [2, 55/110, f]).
step_right([2, 54/114, f], [2, 55/114, f]).
step_right([2, 54/116, f], [2, 55/116, f]).
step_right([2, 54/118, f], [2, 55/118, f]).
step_right([2, 55/4, f], [2, 56/4, f]).
step_right([2, 55/6, f], [2, 56/6, f]).
step_right([2, 55/8, f], [2, 56/8, f]).
step_right([2, 55/10, f], [2, 56/10, f]).
step_right([2, 55/14, f], [2, 56/14, f]).
step_right([2, 55/18, f], [2, 56/18, f]).
step_right([2, 55/28, f], [2, 56/28, f]).
step_right([2, 55/46, f], [2, 56/46, f]).
step_right([2, 55/50, f], [2, 56/50, f]).
step_right([2, 55/54, f], [2, 56/54, f]).
step_right([2, 55/56, f], [2, 56/56, f]).
step_right([2, 55/60, f], [2, 56/60, f]).
step_right([2, 55/62, f], [2, 56/62, f]).
step_right([2, 55/70, f], [2, 56/70, f]).
step_right([2, 55/76, f], [2, 56/76, f]).
step_right([2, 55/78, f], [2, 56/78, f]).
step_right([2, 55/80, f], [2, 56/80, f]).
step_right([2, 55/84, f], [2, 56/84, f]).
step_right([2, 55/86, f], [2, 56/86, f]).
step_right([2, 55/88, f], [2, 56/88, f]).
step_right([2, 55/92, f], [2, 56/92, f]).
step_right([2, 55/100, f], [2, 56/100, f]).
step_right([2, 55/102, f], [2, 56/102, f]).
step_right([2, 55/108, f], [2, 56/108, f]).
step_right([2, 55/110, f], [2, 56/110, f]).
step_right([2, 55/114, f], [2, 56/114, f]).
step_right([2, 55/116, f], [2, 56/116, f]).
step_right([2, 55/118, f], [2, 56/118, f]).
step_right([2, 56/0, f], [2, 57/0, f]).
step_right([2, 56/2, f], [2, 57/2, f]).
step_right([2, 56/8, f], [2, 57/8, f]).
step_right([2, 56/16, f], [2, 57/16, f]).
step_right([2, 56/22, f], [2, 57/22, f]).
step_right([2, 56/26, f], [2, 57/26, f]).
step_right([2, 56/28, f], [2, 57/28, f]).
step_right([2, 56/36, f], [2, 57/36, f]).
step_right([2, 56/38, f], [2, 57/38, f]).
step_right([2, 56/40, f], [2, 57/40, f]).
step_right([2, 56/42, f], [2, 57/42, f]).
step_right([2, 56/52, f], [2, 57/52, f]).
step_right([2, 56/64, f], [2, 57/64, f]).
step_right([2, 56/66, f], [2, 57/66, f]).
step_right([2, 56/68, f], [2, 57/68, f]).
step_right([2, 56/82, f], [2, 57/82, f]).
step_right([2, 56/84, f], [2, 57/84, f]).
step_right([2, 56/86, f], [2, 57/86, f]).
step_right([2, 56/88, f], [2, 57/88, f]).
step_right([2, 56/90, f], [2, 57/90, f]).
step_right([2, 56/92, f], [2, 57/92, f]).
step_right([2, 56/94, f], [2, 57/94, f]).
step_right([2, 56/106, f], [2, 57/106, f]).
step_right([2, 56/110, f], [2, 57/110, f]).
step_right([2, 56/112, f], [2, 57/112, f]).
step_right([2, 56/118, f], [2, 57/118, f]).
step_right([2, 57/0, f], [2, 58/0, f]).
step_right([2, 57/2, f], [2, 58/2, f]).
step_right([2, 57/8, f], [2, 58/8, f]).
step_right([2, 57/16, f], [2, 58/16, f]).
step_right([2, 57/22, f], [2, 58/22, f]).
step_right([2, 57/26, f], [2, 58/26, f]).
step_right([2, 57/28, f], [2, 58/28, f]).
step_right([2, 57/36, f], [2, 58/36, f]).
step_right([2, 57/38, f], [2, 58/38, f]).
step_right([2, 57/40, f], [2, 58/40, f]).
step_right([2, 57/42, f], [2, 58/42, f]).
step_right([2, 57/52, f], [2, 58/52, f]).
step_right([2, 57/64, f], [2, 58/64, f]).
step_right([2, 57/66, f], [2, 58/66, f]).
step_right([2, 57/68, f], [2, 58/68, f]).
step_right([2, 57/82, f], [2, 58/82, f]).
step_right([2, 57/84, f], [2, 58/84, f]).
step_right([2, 57/86, f], [2, 58/86, f]).
step_right([2, 57/88, f], [2, 58/88, f]).
step_right([2, 57/90, f], [2, 58/90, f]).
step_right([2, 57/92, f], [2, 58/92, f]).
step_right([2, 57/94, f], [2, 58/94, f]).
step_right([2, 57/106, f], [2, 58/106, f]).
step_right([2, 57/110, f], [2, 58/110, f]).
step_right([2, 57/112, f], [2, 58/112, f]).
step_right([2, 57/118, f], [2, 58/118, f]).
step_right([2, 58/0, f], [2, 59/0, f]).
step_right([2, 58/4, f], [2, 59/4, f]).
step_right([2, 58/6, f], [2, 59/6, f]).
step_right([2, 58/10, f], [2, 59/10, f]).
step_right([2, 58/12, f], [2, 59/12, f]).
step_right([2, 58/24, f], [2, 59/24, f]).
step_right([2, 58/32, f], [2, 59/32, f]).
step_right([2, 58/40, f], [2, 59/40, f]).
step_right([2, 58/50, f], [2, 59/50, f]).
step_right([2, 58/52, f], [2, 59/52, f]).
step_right([2, 58/54, f], [2, 59/54, f]).
step_right([2, 58/60, f], [2, 59/60, f]).
step_right([2, 58/62, f], [2, 59/62, f]).
step_right([2, 58/70, f], [2, 59/70, f]).
step_right([2, 58/72, f], [2, 59/72, f]).
step_right([2, 58/78, f], [2, 59/78, f]).
step_right([2, 58/80, f], [2, 59/80, f]).
step_right([2, 58/86, f], [2, 59/86, f]).
step_right([2, 58/88, f], [2, 59/88, f]).
step_right([2, 58/92, f], [2, 59/92, f]).
step_right([2, 58/96, f], [2, 59/96, f]).
step_right([2, 58/98, f], [2, 59/98, f]).
step_right([2, 58/100, f], [2, 59/100, f]).
step_right([2, 58/102, f], [2, 59/102, f]).
step_right([2, 58/104, f], [2, 59/104, f]).
step_right([2, 58/108, f], [2, 59/108, f]).
step_right([2, 58/114, f], [2, 59/114, f]).
step_right([2, 58/116, f], [2, 59/116, f]).
step_right([2, 58/118, f], [2, 59/118, f]).
step_right([2, 59/0, f], [2, 60/0, f]).
step_right([2, 59/4, f], [2, 60/4, f]).
step_right([2, 59/6, f], [2, 60/6, f]).
step_right([2, 59/10, f], [2, 60/10, f]).
step_right([2, 59/12, f], [2, 60/12, f]).
step_right([2, 59/24, f], [2, 60/24, f]).
step_right([2, 59/32, f], [2, 60/32, f]).
step_right([2, 59/40, f], [2, 60/40, f]).
step_right([2, 59/50, f], [2, 60/50, f]).
step_right([2, 59/52, f], [2, 60/52, f]).
step_right([2, 59/54, f], [2, 60/54, f]).
step_right([2, 59/60, f], [2, 60/60, f]).
step_right([2, 59/62, f], [2, 60/62, f]).
step_right([2, 59/70, f], [2, 60/70, f]).
step_right([2, 59/72, f], [2, 60/72, f]).
step_right([2, 59/78, f], [2, 60/78, f]).
step_right([2, 59/80, f], [2, 60/80, f]).
step_right([2, 59/86, f], [2, 60/86, f]).
step_right([2, 59/88, f], [2, 60/88, f]).
step_right([2, 59/92, f], [2, 60/92, f]).
step_right([2, 59/96, f], [2, 60/96, f]).
step_right([2, 59/98, f], [2, 60/98, f]).
step_right([2, 59/100, f], [2, 60/100, f]).
step_right([2, 59/102, f], [2, 60/102, f]).
step_right([2, 59/104, f], [2, 60/104, f]).
step_right([2, 59/108, f], [2, 60/108, f]).
step_right([2, 59/114, f], [2, 60/114, f]).
step_right([2, 59/116, f], [2, 60/116, f]).
step_right([2, 59/118, f], [2, 60/118, f]).
step_right([2, 60/0, f], [2, 61/0, f]).
step_right([2, 60/2, f], [2, 61/2, f]).
step_right([2, 60/4, f], [2, 61/4, f]).
step_right([2, 60/8, f], [2, 61/8, f]).
step_right([2, 60/10, f], [2, 61/10, f]).
step_right([2, 60/14, f], [2, 61/14, f]).
step_right([2, 60/22, f], [2, 61/22, f]).
step_right([2, 60/28, f], [2, 61/28, f]).
step_right([2, 60/34, f], [2, 61/34, f]).
step_right([2, 60/38, f], [2, 61/38, f]).
step_right([2, 60/42, f], [2, 61/42, f]).
step_right([2, 60/46, f], [2, 61/46, f]).
step_right([2, 60/54, f], [2, 61/54, f]).
step_right([2, 60/58, f], [2, 61/58, f]).
step_right([2, 60/60, f], [2, 61/60, f]).
step_right([2, 60/64, f], [2, 61/64, f]).
step_right([2, 60/66, f], [2, 61/66, f]).
step_right([2, 60/68, f], [2, 61/68, f]).
step_right([2, 60/74, f], [2, 61/74, f]).
step_right([2, 60/80, f], [2, 61/80, f]).
step_right([2, 60/82, f], [2, 61/82, f]).
step_right([2, 60/90, f], [2, 61/90, f]).
step_right([2, 60/100, f], [2, 61/100, f]).
step_right([2, 60/106, f], [2, 61/106, f]).
step_right([2, 60/112, f], [2, 61/112, f]).
step_right([2, 60/118, f], [2, 61/118, f]).
step_right([2, 61/0, f], [2, 62/0, f]).
step_right([2, 61/2, f], [2, 62/2, f]).
step_right([2, 61/4, f], [2, 62/4, f]).
step_right([2, 61/8, f], [2, 62/8, f]).
step_right([2, 61/10, f], [2, 62/10, f]).
step_right([2, 61/14, f], [2, 62/14, f]).
step_right([2, 61/22, f], [2, 62/22, f]).
step_right([2, 61/28, f], [2, 62/28, f]).
step_right([2, 61/34, f], [2, 62/34, f]).
step_right([2, 61/38, f], [2, 62/38, f]).
step_right([2, 61/42, f], [2, 62/42, f]).
step_right([2, 61/46, f], [2, 62/46, f]).
step_right([2, 61/54, f], [2, 62/54, f]).
step_right([2, 61/58, f], [2, 62/58, f]).
step_right([2, 61/60, f], [2, 62/60, f]).
step_right([2, 61/64, f], [2, 62/64, f]).
step_right([2, 61/66, f], [2, 62/66, f]).
step_right([2, 61/68, f], [2, 62/68, f]).
step_right([2, 61/74, f], [2, 62/74, f]).
step_right([2, 61/80, f], [2, 62/80, f]).
step_right([2, 61/82, f], [2, 62/82, f]).
step_right([2, 61/90, f], [2, 62/90, f]).
step_right([2, 61/100, f], [2, 62/100, f]).
step_right([2, 61/106, f], [2, 62/106, f]).
step_right([2, 61/112, f], [2, 62/112, f]).
step_right([2, 61/118, f], [2, 62/118, f]).
step_right([2, 62/0, f], [2, 63/0, f]).
step_right([2, 62/6, f], [2, 63/6, f]).
step_right([2, 62/8, f], [2, 63/8, f]).
step_right([2, 62/10, f], [2, 63/10, f]).
step_right([2, 62/12, f], [2, 63/12, f]).
step_right([2, 62/14, f], [2, 63/14, f]).
step_right([2, 62/16, f], [2, 63/16, f]).
step_right([2, 62/18, f], [2, 63/18, f]).
step_right([2, 62/24, f], [2, 63/24, f]).
step_right([2, 62/26, f], [2, 63/26, f]).
step_right([2, 62/30, f], [2, 63/30, f]).
step_right([2, 62/32, f], [2, 63/32, f]).
step_right([2, 62/36, f], [2, 63/36, f]).
step_right([2, 62/40, f], [2, 63/40, f]).
step_right([2, 62/44, f], [2, 63/44, f]).
step_right([2, 62/46, f], [2, 63/46, f]).
step_right([2, 62/48, f], [2, 63/48, f]).
step_right([2, 62/56, f], [2, 63/56, f]).
step_right([2, 62/58, f], [2, 63/58, f]).
step_right([2, 62/60, f], [2, 63/60, f]).
step_right([2, 62/70, f], [2, 63/70, f]).
step_right([2, 62/76, f], [2, 63/76, f]).
step_right([2, 62/82, f], [2, 63/82, f]).
step_right([2, 62/84, f], [2, 63/84, f]).
step_right([2, 62/92, f], [2, 63/92, f]).
step_right([2, 62/98, f], [2, 63/98, f]).
step_right([2, 62/100, f], [2, 63/100, f]).
step_right([2, 62/102, f], [2, 63/102, f]).
step_right([2, 62/106, f], [2, 63/106, f]).
step_right([2, 62/108, f], [2, 63/108, f]).
step_right([2, 62/110, f], [2, 63/110, f]).
step_right([2, 62/114, f], [2, 63/114, f]).
step_right([2, 62/116, f], [2, 63/116, f]).
step_right([2, 62/118, f], [2, 63/118, f]).
step_right([2, 63/0, f], [2, 64/0, f]).
step_right([2, 63/6, f], [2, 64/6, f]).
step_right([2, 63/8, f], [2, 64/8, f]).
step_right([2, 63/10, f], [2, 64/10, f]).
step_right([2, 63/12, f], [2, 64/12, f]).
step_right([2, 63/14, f], [2, 64/14, f]).
step_right([2, 63/16, f], [2, 64/16, f]).
step_right([2, 63/18, f], [2, 64/18, f]).
step_right([2, 63/24, f], [2, 64/24, f]).
step_right([2, 63/26, f], [2, 64/26, f]).
step_right([2, 63/30, f], [2, 64/30, f]).
step_right([2, 63/32, f], [2, 64/32, f]).
step_right([2, 63/36, f], [2, 64/36, f]).
step_right([2, 63/40, f], [2, 64/40, f]).
step_right([2, 63/44, f], [2, 64/44, f]).
step_right([2, 63/46, f], [2, 64/46, f]).
step_right([2, 63/48, f], [2, 64/48, f]).
step_right([2, 63/56, f], [2, 64/56, f]).
step_right([2, 63/58, f], [2, 64/58, f]).
step_right([2, 63/60, f], [2, 64/60, f]).
step_right([2, 63/70, f], [2, 64/70, f]).
step_right([2, 63/76, f], [2, 64/76, f]).
step_right([2, 63/82, f], [2, 64/82, f]).
step_right([2, 63/84, f], [2, 64/84, f]).
step_right([2, 63/92, f], [2, 64/92, f]).
step_right([2, 63/98, f], [2, 64/98, f]).
step_right([2, 63/100, f], [2, 64/100, f]).
step_right([2, 63/102, f], [2, 64/102, f]).
step_right([2, 63/106, f], [2, 64/106, f]).
step_right([2, 63/108, f], [2, 64/108, f]).
step_right([2, 63/110, f], [2, 64/110, f]).
step_right([2, 63/114, f], [2, 64/114, f]).
step_right([2, 63/116, f], [2, 64/116, f]).
step_right([2, 63/118, f], [2, 64/118, f]).
step_right([2, 64/6, f], [2, 65/6, f]).
step_right([2, 64/12, f], [2, 65/12, f]).
step_right([2, 64/22, f], [2, 65/22, f]).
step_right([2, 64/28, f], [2, 65/28, f]).
step_right([2, 64/30, f], [2, 65/30, f]).
step_right([2, 64/32, f], [2, 65/32, f]).
step_right([2, 64/36, f], [2, 65/36, f]).
step_right([2, 64/38, f], [2, 65/38, f]).
step_right([2, 64/44, f], [2, 65/44, f]).
step_right([2, 64/46, f], [2, 65/46, f]).
step_right([2, 64/48, f], [2, 65/48, f]).
step_right([2, 64/50, f], [2, 65/50, f]).
step_right([2, 64/52, f], [2, 65/52, f]).
step_right([2, 64/54, f], [2, 65/54, f]).
step_right([2, 64/58, f], [2, 65/58, f]).
step_right([2, 64/68, f], [2, 65/68, f]).
step_right([2, 64/70, f], [2, 65/70, f]).
step_right([2, 64/72, f], [2, 65/72, f]).
step_right([2, 64/74, f], [2, 65/74, f]).
step_right([2, 64/76, f], [2, 65/76, f]).
step_right([2, 64/78, f], [2, 65/78, f]).
step_right([2, 64/80, f], [2, 65/80, f]).
step_right([2, 64/82, f], [2, 65/82, f]).
step_right([2, 64/84, f], [2, 65/84, f]).
step_right([2, 64/88, f], [2, 65/88, f]).
step_right([2, 64/94, f], [2, 65/94, f]).
step_right([2, 64/98, f], [2, 65/98, f]).
step_right([2, 64/104, f], [2, 65/104, f]).
step_right([2, 64/108, f], [2, 65/108, f]).
step_right([2, 64/112, f], [2, 65/112, f]).
step_right([2, 64/114, f], [2, 65/114, f]).
step_right([2, 64/116, f], [2, 65/116, f]).
step_right([2, 64/118, f], [2, 65/118, f]).
step_right([2, 65/6, f], [2, 66/6, f]).
step_right([2, 65/12, f], [2, 66/12, f]).
step_right([2, 65/22, f], [2, 66/22, f]).
step_right([2, 65/28, f], [2, 66/28, f]).
step_right([2, 65/30, f], [2, 66/30, f]).
step_right([2, 65/32, f], [2, 66/32, f]).
step_right([2, 65/36, f], [2, 66/36, f]).
step_right([2, 65/38, f], [2, 66/38, f]).
step_right([2, 65/44, f], [2, 66/44, f]).
step_right([2, 65/46, f], [2, 66/46, f]).
step_right([2, 65/48, f], [2, 66/48, f]).
step_right([2, 65/50, f], [2, 66/50, f]).
step_right([2, 65/52, f], [2, 66/52, f]).
step_right([2, 65/54, f], [2, 66/54, f]).
step_right([2, 65/58, f], [2, 66/58, f]).
step_right([2, 65/68, f], [2, 66/68, f]).
step_right([2, 65/70, f], [2, 66/70, f]).
step_right([2, 65/72, f], [2, 66/72, f]).
step_right([2, 65/74, f], [2, 66/74, f]).
step_right([2, 65/76, f], [2, 66/76, f]).
step_right([2, 65/78, f], [2, 66/78, f]).
step_right([2, 65/80, f], [2, 66/80, f]).
step_right([2, 65/82, f], [2, 66/82, f]).
step_right([2, 65/84, f], [2, 66/84, f]).
step_right([2, 65/88, f], [2, 66/88, f]).
step_right([2, 65/94, f], [2, 66/94, f]).
step_right([2, 65/98, f], [2, 66/98, f]).
step_right([2, 65/104, f], [2, 66/104, f]).
step_right([2, 65/108, f], [2, 66/108, f]).
step_right([2, 65/112, f], [2, 66/112, f]).
step_right([2, 65/114, f], [2, 66/114, f]).
step_right([2, 65/116, f], [2, 66/116, f]).
step_right([2, 65/118, f], [2, 66/118, f]).
step_right([2, 66/0, f], [2, 67/0, f]).
step_right([2, 66/4, f], [2, 67/4, f]).
step_right([2, 66/6, f], [2, 67/6, f]).
step_right([2, 66/8, f], [2, 67/8, f]).
step_right([2, 66/10, f], [2, 67/10, f]).
step_right([2, 66/14, f], [2, 67/14, f]).
step_right([2, 66/20, f], [2, 67/20, f]).
step_right([2, 66/26, f], [2, 67/26, f]).
step_right([2, 66/30, f], [2, 67/30, f]).
step_right([2, 66/32, f], [2, 67/32, f]).
step_right([2, 66/34, f], [2, 67/34, f]).
step_right([2, 66/36, f], [2, 67/36, f]).
step_right([2, 66/38, f], [2, 67/38, f]).
step_right([2, 66/40, f], [2, 67/40, f]).
step_right([2, 66/42, f], [2, 67/42, f]).
step_right([2, 66/44, f], [2, 67/44, f]).
step_right([2, 66/50, f], [2, 67/50, f]).
step_right([2, 66/60, f], [2, 67/60, f]).
step_right([2, 66/66, f], [2, 67/66, f]).
step_right([2, 66/72, f], [2, 67/72, f]).
step_right([2, 66/74, f], [2, 67/74, f]).
step_right([2, 66/86, f], [2, 67/86, f]).
step_right([2, 66/88, f], [2, 67/88, f]).
step_right([2, 66/92, f], [2, 67/92, f]).
step_right([2, 66/94, f], [2, 67/94, f]).
step_right([2, 66/96, f], [2, 67/96, f]).
step_right([2, 66/100, f], [2, 67/100, f]).
step_right([2, 66/102, f], [2, 67/102, f]).
step_right([2, 66/106, f], [2, 67/106, f]).
step_right([2, 66/108, f], [2, 67/108, f]).
step_right([2, 66/110, f], [2, 67/110, f]).
step_right([2, 66/114, f], [2, 67/114, f]).
step_right([2, 66/116, f], [2, 67/116, f]).
step_right([2, 66/118, f], [2, 67/118, f]).
step_right([2, 67/0, f], [2, 68/0, f]).
step_right([2, 67/4, f], [2, 68/4, f]).
step_right([2, 67/6, f], [2, 68/6, f]).
step_right([2, 67/8, f], [2, 68/8, f]).
step_right([2, 67/10, f], [2, 68/10, f]).
step_right([2, 67/14, f], [2, 68/14, f]).
step_right([2, 67/20, f], [2, 68/20, f]).
step_right([2, 67/26, f], [2, 68/26, f]).
step_right([2, 67/30, f], [2, 68/30, f]).
step_right([2, 67/32, f], [2, 68/32, f]).
step_right([2, 67/34, f], [2, 68/34, f]).
step_right([2, 67/36, f], [2, 68/36, f]).
step_right([2, 67/38, f], [2, 68/38, f]).
step_right([2, 67/40, f], [2, 68/40, f]).
step_right([2, 67/42, f], [2, 68/42, f]).
step_right([2, 67/44, f], [2, 68/44, f]).
step_right([2, 67/50, f], [2, 68/50, f]).
step_right([2, 67/60, f], [2, 68/60, f]).
step_right([2, 67/66, f], [2, 68/66, f]).
step_right([2, 67/72, f], [2, 68/72, f]).
step_right([2, 67/74, f], [2, 68/74, f]).
step_right([2, 67/86, f], [2, 68/86, f]).
step_right([2, 67/88, f], [2, 68/88, f]).
step_right([2, 67/92, f], [2, 68/92, f]).
step_right([2, 67/94, f], [2, 68/94, f]).
step_right([2, 67/96, f], [2, 68/96, f]).
step_right([2, 67/100, f], [2, 68/100, f]).
step_right([2, 67/102, f], [2, 68/102, f]).
step_right([2, 67/106, f], [2, 68/106, f]).
step_right([2, 67/108, f], [2, 68/108, f]).
step_right([2, 67/110, f], [2, 68/110, f]).
step_right([2, 67/114, f], [2, 68/114, f]).
step_right([2, 67/116, f], [2, 68/116, f]).
step_right([2, 67/118, f], [2, 68/118, f]).
step_right([2, 68/2, f], [2, 69/2, f]).
step_right([2, 68/6, f], [2, 69/6, f]).
step_right([2, 68/12, f], [2, 69/12, f]).
step_right([2, 68/18, f], [2, 69/18, f]).
step_right([2, 68/20, f], [2, 69/20, f]).
step_right([2, 68/22, f], [2, 69/22, f]).
step_right([2, 68/28, f], [2, 69/28, f]).
step_right([2, 68/34, f], [2, 69/34, f]).
step_right([2, 68/36, f], [2, 69/36, f]).
step_right([2, 68/46, f], [2, 69/46, f]).
step_right([2, 68/52, f], [2, 69/52, f]).
step_right([2, 68/62, f], [2, 69/62, f]).
step_right([2, 68/68, f], [2, 69/68, f]).
step_right([2, 68/84, f], [2, 69/84, f]).
step_right([2, 68/86, f], [2, 69/86, f]).
step_right([2, 68/88, f], [2, 69/88, f]).
step_right([2, 68/90, f], [2, 69/90, f]).
step_right([2, 68/92, f], [2, 69/92, f]).
step_right([2, 68/94, f], [2, 69/94, f]).
step_right([2, 68/104, f], [2, 69/104, f]).
step_right([2, 68/106, f], [2, 69/106, f]).
step_right([2, 68/108, f], [2, 69/108, f]).
step_right([2, 68/112, f], [2, 69/112, f]).
step_right([2, 68/118, f], [2, 69/118, f]).
step_right([2, 69/2, f], [2, 70/2, f]).
step_right([2, 69/6, f], [2, 70/6, f]).
step_right([2, 69/12, f], [2, 70/12, f]).
step_right([2, 69/18, f], [2, 70/18, f]).
step_right([2, 69/20, f], [2, 70/20, f]).
step_right([2, 69/22, f], [2, 70/22, f]).
step_right([2, 69/28, f], [2, 70/28, f]).
step_right([2, 69/34, f], [2, 70/34, f]).
step_right([2, 69/36, f], [2, 70/36, f]).
step_right([2, 69/46, f], [2, 70/46, f]).
step_right([2, 69/52, f], [2, 70/52, f]).
step_right([2, 69/62, f], [2, 70/62, f]).
step_right([2, 69/68, f], [2, 70/68, f]).
step_right([2, 69/84, f], [2, 70/84, f]).
step_right([2, 69/86, f], [2, 70/86, f]).
step_right([2, 69/88, f], [2, 70/88, f]).
step_right([2, 69/90, f], [2, 70/90, f]).
step_right([2, 69/92, f], [2, 70/92, f]).
step_right([2, 69/94, f], [2, 70/94, f]).
step_right([2, 69/104, f], [2, 70/104, f]).
step_right([2, 69/106, f], [2, 70/106, f]).
step_right([2, 69/108, f], [2, 70/108, f]).
step_right([2, 69/112, f], [2, 70/112, f]).
step_right([2, 69/118, f], [2, 70/118, f]).
step_right([2, 70/0, f], [2, 71/0, f]).
step_right([2, 70/4, f], [2, 71/4, f]).
step_right([2, 70/8, f], [2, 71/8, f]).
step_right([2, 70/10, f], [2, 71/10, f]).
step_right([2, 70/14, f], [2, 71/14, f]).
step_right([2, 70/16, f], [2, 71/16, f]).
step_right([2, 70/18, f], [2, 71/18, f]).
step_right([2, 70/20, f], [2, 71/20, f]).
step_right([2, 70/22, f], [2, 71/22, f]).
step_right([2, 70/24, f], [2, 71/24, f]).
step_right([2, 70/32, f], [2, 71/32, f]).
step_right([2, 70/40, f], [2, 71/40, f]).
step_right([2, 70/50, f], [2, 71/50, f]).
step_right([2, 70/56, f], [2, 71/56, f]).
step_right([2, 70/58, f], [2, 71/58, f]).
step_right([2, 70/64, f], [2, 71/64, f]).
step_right([2, 70/66, f], [2, 71/66, f]).
step_right([2, 70/68, f], [2, 71/68, f]).
step_right([2, 70/70, f], [2, 71/70, f]).
step_right([2, 70/74, f], [2, 71/74, f]).
step_right([2, 70/76, f], [2, 71/76, f]).
step_right([2, 70/82, f], [2, 71/82, f]).
step_right([2, 70/90, f], [2, 71/90, f]).
step_right([2, 70/92, f], [2, 71/92, f]).
step_right([2, 70/94, f], [2, 71/94, f]).
step_right([2, 70/98, f], [2, 71/98, f]).
step_right([2, 70/100, f], [2, 71/100, f]).
step_right([2, 70/102, f], [2, 71/102, f]).
step_right([2, 70/106, f], [2, 71/106, f]).
step_right([2, 70/108, f], [2, 71/108, f]).
step_right([2, 70/110, f], [2, 71/110, f]).
step_right([2, 70/112, f], [2, 71/112, f]).
step_right([2, 70/114, f], [2, 71/114, f]).
step_right([2, 71/0, f], [2, 72/0, f]).
step_right([2, 71/4, f], [2, 72/4, f]).
step_right([2, 71/8, f], [2, 72/8, f]).
step_right([2, 71/10, f], [2, 72/10, f]).
step_right([2, 71/14, f], [2, 72/14, f]).
step_right([2, 71/16, f], [2, 72/16, f]).
step_right([2, 71/18, f], [2, 72/18, f]).
step_right([2, 71/20, f], [2, 72/20, f]).
step_right([2, 71/22, f], [2, 72/22, f]).
step_right([2, 71/24, f], [2, 72/24, f]).
step_right([2, 71/32, f], [2, 72/32, f]).
step_right([2, 71/40, f], [2, 72/40, f]).
step_right([2, 71/50, f], [2, 72/50, f]).
step_right([2, 71/56, f], [2, 72/56, f]).
step_right([2, 71/58, f], [2, 72/58, f]).
step_right([2, 71/64, f], [2, 72/64, f]).
step_right([2, 71/66, f], [2, 72/66, f]).
step_right([2, 71/68, f], [2, 72/68, f]).
step_right([2, 71/70, f], [2, 72/70, f]).
step_right([2, 71/74, f], [2, 72/74, f]).
step_right([2, 71/76, f], [2, 72/76, f]).
step_right([2, 71/82, f], [2, 72/82, f]).
step_right([2, 71/90, f], [2, 72/90, f]).
step_right([2, 71/92, f], [2, 72/92, f]).
step_right([2, 71/94, f], [2, 72/94, f]).
step_right([2, 71/98, f], [2, 72/98, f]).
step_right([2, 71/100, f], [2, 72/100, f]).
step_right([2, 71/102, f], [2, 72/102, f]).
step_right([2, 71/106, f], [2, 72/106, f]).
step_right([2, 71/108, f], [2, 72/108, f]).
step_right([2, 71/110, f], [2, 72/110, f]).
step_right([2, 71/112, f], [2, 72/112, f]).
step_right([2, 71/114, f], [2, 72/114, f]).
step_right([2, 72/0, f], [2, 73/0, f]).
step_right([2, 72/2, f], [2, 73/2, f]).
step_right([2, 72/16, f], [2, 73/16, f]).
step_right([2, 72/18, f], [2, 73/18, f]).
step_right([2, 72/22, f], [2, 73/22, f]).
step_right([2, 72/24, f], [2, 73/24, f]).
step_right([2, 72/28, f], [2, 73/28, f]).
step_right([2, 72/36, f], [2, 73/36, f]).
step_right([2, 72/38, f], [2, 73/38, f]).
step_right([2, 72/48, f], [2, 73/48, f]).
step_right([2, 72/50, f], [2, 73/50, f]).
step_right([2, 72/52, f], [2, 73/52, f]).
step_right([2, 72/54, f], [2, 73/54, f]).
step_right([2, 72/58, f], [2, 73/58, f]).
step_right([2, 72/60, f], [2, 73/60, f]).
step_right([2, 72/66, f], [2, 73/66, f]).
step_right([2, 72/72, f], [2, 73/72, f]).
step_right([2, 72/74, f], [2, 73/74, f]).
step_right([2, 72/78, f], [2, 73/78, f]).
step_right([2, 72/80, f], [2, 73/80, f]).
step_right([2, 72/88, f], [2, 73/88, f]).
step_right([2, 72/92, f], [2, 73/92, f]).
step_right([2, 72/96, f], [2, 73/96, f]).
step_right([2, 72/102, f], [2, 73/102, f]).
step_right([2, 72/104, f], [2, 73/104, f]).
step_right([2, 72/106, f], [2, 73/106, f]).
step_right([2, 72/110, f], [2, 73/110, f]).
step_right([2, 72/118, f], [2, 73/118, f]).
step_right([2, 73/0, f], [2, 74/0, f]).
step_right([2, 73/2, f], [2, 74/2, f]).
step_right([2, 73/16, f], [2, 74/16, f]).
step_right([2, 73/18, f], [2, 74/18, f]).
step_right([2, 73/22, f], [2, 74/22, f]).
step_right([2, 73/24, f], [2, 74/24, f]).
step_right([2, 73/28, f], [2, 74/28, f]).
step_right([2, 73/36, f], [2, 74/36, f]).
step_right([2, 73/38, f], [2, 74/38, f]).
step_right([2, 73/48, f], [2, 74/48, f]).
step_right([2, 73/50, f], [2, 74/50, f]).
step_right([2, 73/52, f], [2, 74/52, f]).
step_right([2, 73/54, f], [2, 74/54, f]).
step_right([2, 73/58, f], [2, 74/58, f]).
step_right([2, 73/60, f], [2, 74/60, f]).
step_right([2, 73/66, f], [2, 74/66, f]).
step_right([2, 73/72, f], [2, 74/72, f]).
step_right([2, 73/74, f], [2, 74/74, f]).
step_right([2, 73/78, f], [2, 74/78, f]).
step_right([2, 73/80, f], [2, 74/80, f]).
step_right([2, 73/88, f], [2, 74/88, f]).
step_right([2, 73/92, f], [2, 74/92, f]).
step_right([2, 73/96, f], [2, 74/96, f]).
step_right([2, 73/102, f], [2, 74/102, f]).
step_right([2, 73/104, f], [2, 74/104, f]).
step_right([2, 73/106, f], [2, 74/106, f]).
step_right([2, 73/110, f], [2, 74/110, f]).
step_right([2, 73/118, f], [2, 74/118, f]).
step_right([2, 74/0, f], [2, 75/0, f]).
step_right([2, 74/6, f], [2, 75/6, f]).
step_right([2, 74/10, f], [2, 75/10, f]).
step_right([2, 74/12, f], [2, 75/12, f]).
step_right([2, 74/14, f], [2, 75/14, f]).
step_right([2, 74/16, f], [2, 75/16, f]).
step_right([2, 74/18, f], [2, 75/18, f]).
step_right([2, 74/20, f], [2, 75/20, f]).
step_right([2, 74/22, f], [2, 75/22, f]).
step_right([2, 74/34, f], [2, 75/34, f]).
step_right([2, 74/38, f], [2, 75/38, f]).
step_right([2, 74/40, f], [2, 75/40, f]).
step_right([2, 74/42, f], [2, 75/42, f]).
step_right([2, 74/44, f], [2, 75/44, f]).
step_right([2, 74/46, f], [2, 75/46, f]).
step_right([2, 74/52, f], [2, 75/52, f]).
step_right([2, 74/54, f], [2, 75/54, f]).
step_right([2, 74/56, f], [2, 75/56, f]).
step_right([2, 74/58, f], [2, 75/58, f]).
step_right([2, 74/62, f], [2, 75/62, f]).
step_right([2, 74/64, f], [2, 75/64, f]).
step_right([2, 74/66, f], [2, 75/66, f]).
step_right([2, 74/68, f], [2, 75/68, f]).
step_right([2, 74/76, f], [2, 75/76, f]).
step_right([2, 74/80, f], [2, 75/80, f]).
step_right([2, 74/84, f], [2, 75/84, f]).
step_right([2, 74/86, f], [2, 75/86, f]).
step_right([2, 74/90, f], [2, 75/90, f]).
step_right([2, 74/92, f], [2, 75/92, f]).
step_right([2, 74/94, f], [2, 75/94, f]).
step_right([2, 74/96, f], [2, 75/96, f]).
step_right([2, 74/108, f], [2, 75/108, f]).
step_right([2, 74/116, f], [2, 75/116, f]).
step_right([2, 74/118, f], [2, 75/118, f]).
step_right([2, 75/0, f], [2, 76/0, f]).
step_right([2, 75/6, f], [2, 76/6, f]).
step_right([2, 75/10, f], [2, 76/10, f]).
step_right([2, 75/12, f], [2, 76/12, f]).
step_right([2, 75/14, f], [2, 76/14, f]).
step_right([2, 75/16, f], [2, 76/16, f]).
step_right([2, 75/18, f], [2, 76/18, f]).
step_right([2, 75/20, f], [2, 76/20, f]).
step_right([2, 75/22, f], [2, 76/22, f]).
step_right([2, 75/34, f], [2, 76/34, f]).
step_right([2, 75/38, f], [2, 76/38, f]).
step_right([2, 75/40, f], [2, 76/40, f]).
step_right([2, 75/42, f], [2, 76/42, f]).
step_right([2, 75/44, f], [2, 76/44, f]).
step_right([2, 75/46, f], [2, 76/46, f]).
step_right([2, 75/52, f], [2, 76/52, f]).
step_right([2, 75/54, f], [2, 76/54, f]).
step_right([2, 75/56, f], [2, 76/56, f]).
step_right([2, 75/58, f], [2, 76/58, f]).
step_right([2, 75/62, f], [2, 76/62, f]).
step_right([2, 75/64, f], [2, 76/64, f]).
step_right([2, 75/66, f], [2, 76/66, f]).
step_right([2, 75/68, f], [2, 76/68, f]).
step_right([2, 75/76, f], [2, 76/76, f]).
step_right([2, 75/80, f], [2, 76/80, f]).
step_right([2, 75/84, f], [2, 76/84, f]).
step_right([2, 75/86, f], [2, 76/86, f]).
step_right([2, 75/90, f], [2, 76/90, f]).
step_right([2, 75/92, f], [2, 76/92, f]).
step_right([2, 75/94, f], [2, 76/94, f]).
step_right([2, 75/96, f], [2, 76/96, f]).
step_right([2, 75/108, f], [2, 76/108, f]).
step_right([2, 75/116, f], [2, 76/116, f]).
step_right([2, 75/118, f], [2, 76/118, f]).
step_right([2, 76/6, f], [2, 77/6, f]).
step_right([2, 76/8, f], [2, 77/8, f]).
step_right([2, 76/12, f], [2, 77/12, f]).
step_right([2, 76/16, f], [2, 77/16, f]).
step_right([2, 76/18, f], [2, 77/18, f]).
step_right([2, 76/24, f], [2, 77/24, f]).
step_right([2, 76/26, f], [2, 77/26, f]).
step_right([2, 76/28, f], [2, 77/28, f]).
step_right([2, 76/30, f], [2, 77/30, f]).
step_right([2, 76/32, f], [2, 77/32, f]).
step_right([2, 76/34, f], [2, 77/34, f]).
step_right([2, 76/36, f], [2, 77/36, f]).
step_right([2, 76/38, f], [2, 77/38, f]).
step_right([2, 76/44, f], [2, 77/44, f]).
step_right([2, 76/46, f], [2, 77/46, f]).
step_right([2, 76/52, f], [2, 77/52, f]).
step_right([2, 76/54, f], [2, 77/54, f]).
step_right([2, 76/56, f], [2, 77/56, f]).
step_right([2, 76/60, f], [2, 77/60, f]).
step_right([2, 76/62, f], [2, 77/62, f]).
step_right([2, 76/68, f], [2, 77/68, f]).
step_right([2, 76/70, f], [2, 77/70, f]).
step_right([2, 76/74, f], [2, 77/74, f]).
step_right([2, 76/76, f], [2, 77/76, f]).
step_right([2, 76/80, f], [2, 77/80, f]).
step_right([2, 76/84, f], [2, 77/84, f]).
step_right([2, 76/86, f], [2, 77/86, f]).
step_right([2, 76/88, f], [2, 77/88, f]).
step_right([2, 76/94, f], [2, 77/94, f]).
step_right([2, 76/100, f], [2, 77/100, f]).
step_right([2, 76/104, f], [2, 77/104, f]).
step_right([2, 76/110, f], [2, 77/110, f]).
step_right([2, 76/114, f], [2, 77/114, f]).
step_right([2, 77/6, f], [2, 78/6, f]).
step_right([2, 77/8, f], [2, 78/8, f]).
step_right([2, 77/12, f], [2, 78/12, f]).
step_right([2, 77/16, f], [2, 78/16, f]).
step_right([2, 77/18, f], [2, 78/18, f]).
step_right([2, 77/24, f], [2, 78/24, f]).
step_right([2, 77/26, f], [2, 78/26, f]).
step_right([2, 77/28, f], [2, 78/28, f]).
step_right([2, 77/30, f], [2, 78/30, f]).
step_right([2, 77/32, f], [2, 78/32, f]).
step_right([2, 77/34, f], [2, 78/34, f]).
step_right([2, 77/36, f], [2, 78/36, f]).
step_right([2, 77/38, f], [2, 78/38, f]).
step_right([2, 77/44, f], [2, 78/44, f]).
step_right([2, 77/46, f], [2, 78/46, f]).
step_right([2, 77/52, f], [2, 78/52, f]).
step_right([2, 77/54, f], [2, 78/54, f]).
step_right([2, 77/56, f], [2, 78/56, f]).
step_right([2, 77/60, f], [2, 78/60, f]).
step_right([2, 77/62, f], [2, 78/62, f]).
step_right([2, 77/68, f], [2, 78/68, f]).
step_right([2, 77/70, f], [2, 78/70, f]).
step_right([2, 77/74, f], [2, 78/74, f]).
step_right([2, 77/76, f], [2, 78/76, f]).
step_right([2, 77/80, f], [2, 78/80, f]).
step_right([2, 77/84, f], [2, 78/84, f]).
step_right([2, 77/86, f], [2, 78/86, f]).
step_right([2, 77/88, f], [2, 78/88, f]).
step_right([2, 77/94, f], [2, 78/94, f]).
step_right([2, 77/100, f], [2, 78/100, f]).
step_right([2, 77/104, f], [2, 78/104, f]).
step_right([2, 77/110, f], [2, 78/110, f]).
step_right([2, 77/114, f], [2, 78/114, f]).
step_right([2, 78/0, f], [2, 79/0, f]).
step_right([2, 78/6, f], [2, 79/6, f]).
step_right([2, 78/14, f], [2, 79/14, f]).
step_right([2, 78/22, f], [2, 79/22, f]).
step_right([2, 78/28, f], [2, 79/28, f]).
step_right([2, 78/36, f], [2, 79/36, f]).
step_right([2, 78/38, f], [2, 79/38, f]).
step_right([2, 78/40, f], [2, 79/40, f]).
step_right([2, 78/54, f], [2, 79/54, f]).
step_right([2, 78/60, f], [2, 79/60, f]).
step_right([2, 78/66, f], [2, 79/66, f]).
step_right([2, 78/72, f], [2, 79/72, f]).
step_right([2, 78/74, f], [2, 79/74, f]).
step_right([2, 78/76, f], [2, 79/76, f]).
step_right([2, 78/78, f], [2, 79/78, f]).
step_right([2, 78/82, f], [2, 79/82, f]).
step_right([2, 78/90, f], [2, 79/90, f]).
step_right([2, 78/92, f], [2, 79/92, f]).
step_right([2, 78/98, f], [2, 79/98, f]).
step_right([2, 78/102, f], [2, 79/102, f]).
step_right([2, 78/104, f], [2, 79/104, f]).
step_right([2, 78/106, f], [2, 79/106, f]).
step_right([2, 78/108, f], [2, 79/108, f]).
step_right([2, 78/112, f], [2, 79/112, f]).
step_right([2, 78/114, f], [2, 79/114, f]).
step_right([2, 78/118, f], [2, 79/118, f]).
step_right([2, 79/0, f], [2, 80/0, f]).
step_right([2, 79/6, f], [2, 80/6, f]).
step_right([2, 79/14, f], [2, 80/14, f]).
step_right([2, 79/22, f], [2, 80/22, f]).
step_right([2, 79/28, f], [2, 80/28, f]).
step_right([2, 79/36, f], [2, 80/36, f]).
step_right([2, 79/38, f], [2, 80/38, f]).
step_right([2, 79/40, f], [2, 80/40, f]).
step_right([2, 79/54, f], [2, 80/54, f]).
step_right([2, 79/60, f], [2, 80/60, f]).
step_right([2, 79/66, f], [2, 80/66, f]).
step_right([2, 79/72, f], [2, 80/72, f]).
step_right([2, 79/74, f], [2, 80/74, f]).
step_right([2, 79/76, f], [2, 80/76, f]).
step_right([2, 79/78, f], [2, 80/78, f]).
step_right([2, 79/82, f], [2, 80/82, f]).
step_right([2, 79/90, f], [2, 80/90, f]).
step_right([2, 79/92, f], [2, 80/92, f]).
step_right([2, 79/98, f], [2, 80/98, f]).
step_right([2, 79/102, f], [2, 80/102, f]).
step_right([2, 79/104, f], [2, 80/104, f]).
step_right([2, 79/106, f], [2, 80/106, f]).
step_right([2, 79/108, f], [2, 80/108, f]).
step_right([2, 79/112, f], [2, 80/112, f]).
step_right([2, 79/114, f], [2, 80/114, f]).
step_right([2, 79/118, f], [2, 80/118, f]).
step_right([2, 80/0, f], [2, 81/0, f]).
step_right([2, 80/8, f], [2, 81/8, f]).
step_right([2, 80/12, f], [2, 81/12, f]).
step_right([2, 80/16, f], [2, 81/16, f]).
step_right([2, 80/18, f], [2, 81/18, f]).
step_right([2, 80/26, f], [2, 81/26, f]).
step_right([2, 80/30, f], [2, 81/30, f]).
step_right([2, 80/32, f], [2, 81/32, f]).
step_right([2, 80/48, f], [2, 81/48, f]).
step_right([2, 80/50, f], [2, 81/50, f]).
step_right([2, 80/58, f], [2, 81/58, f]).
step_right([2, 80/64, f], [2, 81/64, f]).
step_right([2, 80/66, f], [2, 81/66, f]).
step_right([2, 80/72, f], [2, 81/72, f]).
step_right([2, 80/74, f], [2, 81/74, f]).
step_right([2, 80/78, f], [2, 81/78, f]).
step_right([2, 80/80, f], [2, 81/80, f]).
step_right([2, 80/82, f], [2, 81/82, f]).
step_right([2, 80/92, f], [2, 81/92, f]).
step_right([2, 80/96, f], [2, 81/96, f]).
step_right([2, 80/98, f], [2, 81/98, f]).
step_right([2, 80/100, f], [2, 81/100, f]).
step_right([2, 80/104, f], [2, 81/104, f]).
step_right([2, 80/108, f], [2, 81/108, f]).
step_right([2, 80/110, f], [2, 81/110, f]).
step_right([2, 80/118, f], [2, 81/118, f]).
step_right([2, 81/0, f], [2, 82/0, f]).
step_right([2, 81/8, f], [2, 82/8, f]).
step_right([2, 81/12, f], [2, 82/12, f]).
step_right([2, 81/16, f], [2, 82/16, f]).
step_right([2, 81/18, f], [2, 82/18, f]).
step_right([2, 81/26, f], [2, 82/26, f]).
step_right([2, 81/30, f], [2, 82/30, f]).
step_right([2, 81/32, f], [2, 82/32, f]).
step_right([2, 81/48, f], [2, 82/48, f]).
step_right([2, 81/50, f], [2, 82/50, f]).
step_right([2, 81/58, f], [2, 82/58, f]).
step_right([2, 81/64, f], [2, 82/64, f]).
step_right([2, 81/66, f], [2, 82/66, f]).
step_right([2, 81/72, f], [2, 82/72, f]).
step_right([2, 81/74, f], [2, 82/74, f]).
step_right([2, 81/78, f], [2, 82/78, f]).
step_right([2, 81/80, f], [2, 82/80, f]).
step_right([2, 81/82, f], [2, 82/82, f]).
step_right([2, 81/92, f], [2, 82/92, f]).
step_right([2, 81/96, f], [2, 82/96, f]).
step_right([2, 81/98, f], [2, 82/98, f]).
step_right([2, 81/100, f], [2, 82/100, f]).
step_right([2, 81/104, f], [2, 82/104, f]).
step_right([2, 81/108, f], [2, 82/108, f]).
step_right([2, 81/110, f], [2, 82/110, f]).
step_right([2, 81/118, f], [2, 82/118, f]).
step_right([2, 82/0, f], [2, 83/0, f]).
step_right([2, 82/10, f], [2, 83/10, f]).
step_right([2, 82/16, f], [2, 83/16, f]).
step_right([2, 82/20, f], [2, 83/20, f]).
step_right([2, 82/22, f], [2, 83/22, f]).
step_right([2, 82/24, f], [2, 83/24, f]).
step_right([2, 82/28, f], [2, 83/28, f]).
step_right([2, 82/30, f], [2, 83/30, f]).
step_right([2, 82/34, f], [2, 83/34, f]).
step_right([2, 82/36, f], [2, 83/36, f]).
step_right([2, 82/40, f], [2, 83/40, f]).
step_right([2, 82/42, f], [2, 83/42, f]).
step_right([2, 82/56, f], [2, 83/56, f]).
step_right([2, 82/60, f], [2, 83/60, f]).
step_right([2, 82/68, f], [2, 83/68, f]).
step_right([2, 82/70, f], [2, 83/70, f]).
step_right([2, 82/76, f], [2, 83/76, f]).
step_right([2, 82/78, f], [2, 83/78, f]).
step_right([2, 82/80, f], [2, 83/80, f]).
step_right([2, 82/82, f], [2, 83/82, f]).
step_right([2, 82/84, f], [2, 83/84, f]).
step_right([2, 82/88, f], [2, 83/88, f]).
step_right([2, 82/94, f], [2, 83/94, f]).
step_right([2, 82/96, f], [2, 83/96, f]).
step_right([2, 82/98, f], [2, 83/98, f]).
step_right([2, 82/100, f], [2, 83/100, f]).
step_right([2, 82/102, f], [2, 83/102, f]).
step_right([2, 82/106, f], [2, 83/106, f]).
step_right([2, 82/110, f], [2, 83/110, f]).
step_right([2, 82/112, f], [2, 83/112, f]).
step_right([2, 82/118, f], [2, 83/118, f]).
step_right([2, 83/0, f], [2, 84/0, f]).
step_right([2, 83/10, f], [2, 84/10, f]).
step_right([2, 83/16, f], [2, 84/16, f]).
step_right([2, 83/20, f], [2, 84/20, f]).
step_right([2, 83/22, f], [2, 84/22, f]).
step_right([2, 83/24, f], [2, 84/24, f]).
step_right([2, 83/28, f], [2, 84/28, f]).
step_right([2, 83/30, f], [2, 84/30, f]).
step_right([2, 83/34, f], [2, 84/34, f]).
step_right([2, 83/36, f], [2, 84/36, f]).
step_right([2, 83/40, f], [2, 84/40, f]).
step_right([2, 83/42, f], [2, 84/42, f]).
step_right([2, 83/56, f], [2, 84/56, f]).
step_right([2, 83/60, f], [2, 84/60, f]).
step_right([2, 83/68, f], [2, 84/68, f]).
step_right([2, 83/70, f], [2, 84/70, f]).
step_right([2, 83/76, f], [2, 84/76, f]).
step_right([2, 83/78, f], [2, 84/78, f]).
step_right([2, 83/80, f], [2, 84/80, f]).
step_right([2, 83/82, f], [2, 84/82, f]).
step_right([2, 83/84, f], [2, 84/84, f]).
step_right([2, 83/88, f], [2, 84/88, f]).
step_right([2, 83/94, f], [2, 84/94, f]).
step_right([2, 83/96, f], [2, 84/96, f]).
step_right([2, 83/98, f], [2, 84/98, f]).
step_right([2, 83/100, f], [2, 84/100, f]).
step_right([2, 83/102, f], [2, 84/102, f]).
step_right([2, 83/106, f], [2, 84/106, f]).
step_right([2, 83/110, f], [2, 84/110, f]).
step_right([2, 83/112, f], [2, 84/112, f]).
step_right([2, 83/118, f], [2, 84/118, f]).
step_right([2, 84/0, f], [2, 85/0, f]).
step_right([2, 84/6, f], [2, 85/6, f]).
step_right([2, 84/14, f], [2, 85/14, f]).
step_right([2, 84/18, f], [2, 85/18, f]).
step_right([2, 84/20, f], [2, 85/20, f]).
step_right([2, 84/22, f], [2, 85/22, f]).
step_right([2, 84/24, f], [2, 85/24, f]).
step_right([2, 84/26, f], [2, 85/26, f]).
step_right([2, 84/32, f], [2, 85/32, f]).
step_right([2, 84/34, f], [2, 85/34, f]).
step_right([2, 84/36, f], [2, 85/36, f]).
step_right([2, 84/38, f], [2, 85/38, f]).
step_right([2, 84/40, f], [2, 85/40, f]).
step_right([2, 84/46, f], [2, 85/46, f]).
step_right([2, 84/48, f], [2, 85/48, f]).
step_right([2, 84/52, f], [2, 85/52, f]).
step_right([2, 84/54, f], [2, 85/54, f]).
step_right([2, 84/58, f], [2, 85/58, f]).
step_right([2, 84/72, f], [2, 85/72, f]).
step_right([2, 84/76, f], [2, 85/76, f]).
step_right([2, 84/78, f], [2, 85/78, f]).
step_right([2, 84/80, f], [2, 85/80, f]).
step_right([2, 84/82, f], [2, 85/82, f]).
step_right([2, 84/92, f], [2, 85/92, f]).
step_right([2, 84/94, f], [2, 85/94, f]).
step_right([2, 84/96, f], [2, 85/96, f]).
step_right([2, 84/104, f], [2, 85/104, f]).
step_right([2, 84/116, f], [2, 85/116, f]).
step_right([2, 85/0, f], [2, 86/0, f]).
step_right([2, 85/6, f], [2, 86/6, f]).
step_right([2, 85/14, f], [2, 86/14, f]).
step_right([2, 85/18, f], [2, 86/18, f]).
step_right([2, 85/20, f], [2, 86/20, f]).
step_right([2, 85/22, f], [2, 86/22, f]).
step_right([2, 85/24, f], [2, 86/24, f]).
step_right([2, 85/26, f], [2, 86/26, f]).
step_right([2, 85/32, f], [2, 86/32, f]).
step_right([2, 85/34, f], [2, 86/34, f]).
step_right([2, 85/36, f], [2, 86/36, f]).
step_right([2, 85/38, f], [2, 86/38, f]).
step_right([2, 85/40, f], [2, 86/40, f]).
step_right([2, 85/46, f], [2, 86/46, f]).
step_right([2, 85/48, f], [2, 86/48, f]).
step_right([2, 85/52, f], [2, 86/52, f]).
step_right([2, 85/54, f], [2, 86/54, f]).
step_right([2, 85/58, f], [2, 86/58, f]).
step_right([2, 85/72, f], [2, 86/72, f]).
step_right([2, 85/76, f], [2, 86/76, f]).
step_right([2, 85/78, f], [2, 86/78, f]).
step_right([2, 85/80, f], [2, 86/80, f]).
step_right([2, 85/82, f], [2, 86/82, f]).
step_right([2, 85/92, f], [2, 86/92, f]).
step_right([2, 85/94, f], [2, 86/94, f]).
step_right([2, 85/96, f], [2, 86/96, f]).
step_right([2, 85/104, f], [2, 86/104, f]).
step_right([2, 85/116, f], [2, 86/116, f]).
step_right([2, 86/4, f], [2, 87/4, f]).
step_right([2, 86/8, f], [2, 87/8, f]).
step_right([2, 86/14, f], [2, 87/14, f]).
step_right([2, 86/20, f], [2, 87/20, f]).
step_right([2, 86/22, f], [2, 87/22, f]).
step_right([2, 86/28, f], [2, 87/28, f]).
step_right([2, 86/38, f], [2, 87/38, f]).
step_right([2, 86/40, f], [2, 87/40, f]).
step_right([2, 86/44, f], [2, 87/44, f]).
step_right([2, 86/48, f], [2, 87/48, f]).
step_right([2, 86/50, f], [2, 87/50, f]).
step_right([2, 86/52, f], [2, 87/52, f]).
step_right([2, 86/54, f], [2, 87/54, f]).
step_right([2, 86/56, f], [2, 87/56, f]).
step_right([2, 86/60, f], [2, 87/60, f]).
step_right([2, 86/62, f], [2, 87/62, f]).
step_right([2, 86/68, f], [2, 87/68, f]).
step_right([2, 86/74, f], [2, 87/74, f]).
step_right([2, 86/76, f], [2, 87/76, f]).
step_right([2, 86/80, f], [2, 87/80, f]).
step_right([2, 86/84, f], [2, 87/84, f]).
step_right([2, 86/88, f], [2, 87/88, f]).
step_right([2, 86/96, f], [2, 87/96, f]).
step_right([2, 86/110, f], [2, 87/110, f]).
step_right([2, 86/114, f], [2, 87/114, f]).
step_right([2, 86/116, f], [2, 87/116, f]).
step_right([2, 86/118, f], [2, 87/118, f]).
step_right([2, 87/4, f], [2, 88/4, f]).
step_right([2, 87/8, f], [2, 88/8, f]).
step_right([2, 87/14, f], [2, 88/14, f]).
step_right([2, 87/20, f], [2, 88/20, f]).
step_right([2, 87/22, f], [2, 88/22, f]).
step_right([2, 87/28, f], [2, 88/28, f]).
step_right([2, 87/38, f], [2, 88/38, f]).
step_right([2, 87/40, f], [2, 88/40, f]).
step_right([2, 87/44, f], [2, 88/44, f]).
step_right([2, 87/48, f], [2, 88/48, f]).
step_right([2, 87/50, f], [2, 88/50, f]).
step_right([2, 87/52, f], [2, 88/52, f]).
step_right([2, 87/54, f], [2, 88/54, f]).
step_right([2, 87/56, f], [2, 88/56, f]).
step_right([2, 87/60, f], [2, 88/60, f]).
step_right([2, 87/62, f], [2, 88/62, f]).
step_right([2, 87/68, f], [2, 88/68, f]).
step_right([2, 87/74, f], [2, 88/74, f]).
step_right([2, 87/76, f], [2, 88/76, f]).
step_right([2, 87/80, f], [2, 88/80, f]).
step_right([2, 87/84, f], [2, 88/84, f]).
step_right([2, 87/88, f], [2, 88/88, f]).
step_right([2, 87/96, f], [2, 88/96, f]).
step_right([2, 87/110, f], [2, 88/110, f]).
step_right([2, 87/114, f], [2, 88/114, f]).
step_right([2, 87/116, f], [2, 88/116, f]).
step_right([2, 87/118, f], [2, 88/118, f]).
step_right([2, 88/0, f], [2, 89/0, f]).
step_right([2, 88/6, f], [2, 89/6, f]).
step_right([2, 88/8, f], [2, 89/8, f]).
step_right([2, 88/12, f], [2, 89/12, f]).
step_right([2, 88/16, f], [2, 89/16, f]).
step_right([2, 88/20, f], [2, 89/20, f]).
step_right([2, 88/32, f], [2, 89/32, f]).
step_right([2, 88/36, f], [2, 89/36, f]).
step_right([2, 88/38, f], [2, 89/38, f]).
step_right([2, 88/42, f], [2, 89/42, f]).
step_right([2, 88/48, f], [2, 89/48, f]).
step_right([2, 88/58, f], [2, 89/58, f]).
step_right([2, 88/72, f], [2, 89/72, f]).
step_right([2, 88/74, f], [2, 89/74, f]).
step_right([2, 88/76, f], [2, 89/76, f]).
step_right([2, 88/78, f], [2, 89/78, f]).
step_right([2, 88/82, f], [2, 89/82, f]).
step_right([2, 88/86, f], [2, 89/86, f]).
step_right([2, 88/90, f], [2, 89/90, f]).
step_right([2, 88/92, f], [2, 89/92, f]).
step_right([2, 88/96, f], [2, 89/96, f]).
step_right([2, 88/98, f], [2, 89/98, f]).
step_right([2, 88/104, f], [2, 89/104, f]).
step_right([2, 88/106, f], [2, 89/106, f]).
step_right([2, 88/112, f], [2, 89/112, f]).
step_right([2, 88/114, f], [2, 89/114, f]).
step_right([2, 88/116, f], [2, 89/116, f]).
step_right([2, 88/118, f], [2, 89/118, f]).
step_right([2, 89/0, f], [2, 90/0, f]).
step_right([2, 89/6, f], [2, 90/6, f]).
step_right([2, 89/8, f], [2, 90/8, f]).
step_right([2, 89/12, f], [2, 90/12, f]).
step_right([2, 89/16, f], [2, 90/16, f]).
step_right([2, 89/20, f], [2, 90/20, f]).
step_right([2, 89/32, f], [2, 90/32, f]).
step_right([2, 89/36, f], [2, 90/36, f]).
step_right([2, 89/38, f], [2, 90/38, f]).
step_right([2, 89/42, f], [2, 90/42, f]).
step_right([2, 89/48, f], [2, 90/48, f]).
step_right([2, 89/58, f], [2, 90/58, f]).
step_right([2, 89/72, f], [2, 90/72, f]).
step_right([2, 89/74, f], [2, 90/74, f]).
step_right([2, 89/76, f], [2, 90/76, f]).
step_right([2, 89/78, f], [2, 90/78, f]).
step_right([2, 89/82, f], [2, 90/82, f]).
step_right([2, 89/86, f], [2, 90/86, f]).
step_right([2, 89/90, f], [2, 90/90, f]).
step_right([2, 89/92, f], [2, 90/92, f]).
step_right([2, 89/96, f], [2, 90/96, f]).
step_right([2, 89/98, f], [2, 90/98, f]).
step_right([2, 89/104, f], [2, 90/104, f]).
step_right([2, 89/106, f], [2, 90/106, f]).
step_right([2, 89/112, f], [2, 90/112, f]).
step_right([2, 89/114, f], [2, 90/114, f]).
step_right([2, 89/116, f], [2, 90/116, f]).
step_right([2, 89/118, f], [2, 90/118, f]).
step_right([2, 90/0, f], [2, 91/0, f]).
step_right([2, 90/2, f], [2, 91/2, f]).
step_right([2, 90/12, f], [2, 91/12, f]).
step_right([2, 90/18, f], [2, 91/18, f]).
step_right([2, 90/24, f], [2, 91/24, f]).
step_right([2, 90/26, f], [2, 91/26, f]).
step_right([2, 90/28, f], [2, 91/28, f]).
step_right([2, 90/30, f], [2, 91/30, f]).
step_right([2, 90/34, f], [2, 91/34, f]).
step_right([2, 90/40, f], [2, 91/40, f]).
step_right([2, 90/44, f], [2, 91/44, f]).
step_right([2, 90/46, f], [2, 91/46, f]).
step_right([2, 90/50, f], [2, 91/50, f]).
step_right([2, 90/52, f], [2, 91/52, f]).
step_right([2, 90/56, f], [2, 91/56, f]).
step_right([2, 90/58, f], [2, 91/58, f]).
step_right([2, 90/60, f], [2, 91/60, f]).
step_right([2, 90/66, f], [2, 91/66, f]).
step_right([2, 90/68, f], [2, 91/68, f]).
step_right([2, 90/70, f], [2, 91/70, f]).
step_right([2, 90/72, f], [2, 91/72, f]).
step_right([2, 90/78, f], [2, 91/78, f]).
step_right([2, 90/80, f], [2, 91/80, f]).
step_right([2, 90/84, f], [2, 91/84, f]).
step_right([2, 90/88, f], [2, 91/88, f]).
step_right([2, 90/90, f], [2, 91/90, f]).
step_right([2, 90/104, f], [2, 91/104, f]).
step_right([2, 90/112, f], [2, 91/112, f]).
step_right([2, 90/118, f], [2, 91/118, f]).
step_right([2, 91/0, f], [2, 92/0, f]).
step_right([2, 91/2, f], [2, 92/2, f]).
step_right([2, 91/12, f], [2, 92/12, f]).
step_right([2, 91/18, f], [2, 92/18, f]).
step_right([2, 91/24, f], [2, 92/24, f]).
step_right([2, 91/26, f], [2, 92/26, f]).
step_right([2, 91/28, f], [2, 92/28, f]).
step_right([2, 91/30, f], [2, 92/30, f]).
step_right([2, 91/34, f], [2, 92/34, f]).
step_right([2, 91/40, f], [2, 92/40, f]).
step_right([2, 91/44, f], [2, 92/44, f]).
step_right([2, 91/46, f], [2, 92/46, f]).
step_right([2, 91/50, f], [2, 92/50, f]).
step_right([2, 91/52, f], [2, 92/52, f]).
step_right([2, 91/56, f], [2, 92/56, f]).
step_right([2, 91/58, f], [2, 92/58, f]).
step_right([2, 91/60, f], [2, 92/60, f]).
step_right([2, 91/66, f], [2, 92/66, f]).
step_right([2, 91/68, f], [2, 92/68, f]).
step_right([2, 91/70, f], [2, 92/70, f]).
step_right([2, 91/72, f], [2, 92/72, f]).
step_right([2, 91/78, f], [2, 92/78, f]).
step_right([2, 91/80, f], [2, 92/80, f]).
step_right([2, 91/84, f], [2, 92/84, f]).
step_right([2, 91/88, f], [2, 92/88, f]).
step_right([2, 91/90, f], [2, 92/90, f]).
step_right([2, 91/104, f], [2, 92/104, f]).
step_right([2, 91/112, f], [2, 92/112, f]).
step_right([2, 91/118, f], [2, 92/118, f]).
step_right([2, 92/0, f], [2, 93/0, f]).
step_right([2, 92/4, f], [2, 93/4, f]).
step_right([2, 92/10, f], [2, 93/10, f]).
step_right([2, 92/14, f], [2, 93/14, f]).
step_right([2, 92/18, f], [2, 93/18, f]).
step_right([2, 92/20, f], [2, 93/20, f]).
step_right([2, 92/30, f], [2, 93/30, f]).
step_right([2, 92/32, f], [2, 93/32, f]).
step_right([2, 92/36, f], [2, 93/36, f]).
step_right([2, 92/38, f], [2, 93/38, f]).
step_right([2, 92/42, f], [2, 93/42, f]).
step_right([2, 92/44, f], [2, 93/44, f]).
step_right([2, 92/52, f], [2, 93/52, f]).
step_right([2, 92/54, f], [2, 93/54, f]).
step_right([2, 92/62, f], [2, 93/62, f]).
step_right([2, 92/72, f], [2, 93/72, f]).
step_right([2, 92/78, f], [2, 93/78, f]).
step_right([2, 92/80, f], [2, 93/80, f]).
step_right([2, 92/86, f], [2, 93/86, f]).
step_right([2, 92/94, f], [2, 93/94, f]).
step_right([2, 92/96, f], [2, 93/96, f]).
step_right([2, 92/98, f], [2, 93/98, f]).
step_right([2, 92/100, f], [2, 93/100, f]).
step_right([2, 92/102, f], [2, 93/102, f]).
step_right([2, 92/106, f], [2, 93/106, f]).
step_right([2, 92/110, f], [2, 93/110, f]).
step_right([2, 92/118, f], [2, 93/118, f]).
step_right([2, 93/0, f], [2, 94/0, f]).
step_right([2, 93/4, f], [2, 94/4, f]).
step_right([2, 93/10, f], [2, 94/10, f]).
step_right([2, 93/14, f], [2, 94/14, f]).
step_right([2, 93/18, f], [2, 94/18, f]).
step_right([2, 93/20, f], [2, 94/20, f]).
step_right([2, 93/30, f], [2, 94/30, f]).
step_right([2, 93/32, f], [2, 94/32, f]).
step_right([2, 93/36, f], [2, 94/36, f]).
step_right([2, 93/38, f], [2, 94/38, f]).
step_right([2, 93/42, f], [2, 94/42, f]).
step_right([2, 93/44, f], [2, 94/44, f]).
step_right([2, 93/52, f], [2, 94/52, f]).
step_right([2, 93/54, f], [2, 94/54, f]).
step_right([2, 93/62, f], [2, 94/62, f]).
step_right([2, 93/72, f], [2, 94/72, f]).
step_right([2, 93/78, f], [2, 94/78, f]).
step_right([2, 93/80, f], [2, 94/80, f]).
step_right([2, 93/86, f], [2, 94/86, f]).
step_right([2, 93/94, f], [2, 94/94, f]).
step_right([2, 93/96, f], [2, 94/96, f]).
step_right([2, 93/98, f], [2, 94/98, f]).
step_right([2, 93/100, f], [2, 94/100, f]).
step_right([2, 93/102, f], [2, 94/102, f]).
step_right([2, 93/106, f], [2, 94/106, f]).
step_right([2, 93/110, f], [2, 94/110, f]).
step_right([2, 93/118, f], [2, 94/118, f]).
step_right([2, 94/6, f], [2, 95/6, f]).
step_right([2, 94/8, f], [2, 95/8, f]).
step_right([2, 94/10, f], [2, 95/10, f]).
step_right([2, 94/12, f], [2, 95/12, f]).
step_right([2, 94/16, f], [2, 95/16, f]).
step_right([2, 94/20, f], [2, 95/20, f]).
step_right([2, 94/22, f], [2, 95/22, f]).
step_right([2, 94/24, f], [2, 95/24, f]).
step_right([2, 94/34, f], [2, 95/34, f]).
step_right([2, 94/36, f], [2, 95/36, f]).
step_right([2, 94/46, f], [2, 95/46, f]).
step_right([2, 94/54, f], [2, 95/54, f]).
step_right([2, 94/56, f], [2, 95/56, f]).
step_right([2, 94/60, f], [2, 95/60, f]).
step_right([2, 94/62, f], [2, 95/62, f]).
step_right([2, 94/68, f], [2, 95/68, f]).
step_right([2, 94/70, f], [2, 95/70, f]).
step_right([2, 94/74, f], [2, 95/74, f]).
step_right([2, 94/76, f], [2, 95/76, f]).
step_right([2, 94/78, f], [2, 95/78, f]).
step_right([2, 94/82, f], [2, 95/82, f]).
step_right([2, 94/84, f], [2, 95/84, f]).
step_right([2, 94/88, f], [2, 95/88, f]).
step_right([2, 94/94, f], [2, 95/94, f]).
step_right([2, 94/96, f], [2, 95/96, f]).
step_right([2, 94/104, f], [2, 95/104, f]).
step_right([2, 94/106, f], [2, 95/106, f]).
step_right([2, 94/108, f], [2, 95/108, f]).
step_right([2, 94/112, f], [2, 95/112, f]).
step_right([2, 94/116, f], [2, 95/116, f]).
step_right([2, 94/118, f], [2, 95/118, f]).
step_right([2, 95/6, f], [2, 96/6, f]).
step_right([2, 95/8, f], [2, 96/8, f]).
step_right([2, 95/10, f], [2, 96/10, f]).
step_right([2, 95/12, f], [2, 96/12, f]).
step_right([2, 95/16, f], [2, 96/16, f]).
step_right([2, 95/20, f], [2, 96/20, f]).
step_right([2, 95/22, f], [2, 96/22, f]).
step_right([2, 95/24, f], [2, 96/24, f]).
step_right([2, 95/34, f], [2, 96/34, f]).
step_right([2, 95/36, f], [2, 96/36, f]).
step_right([2, 95/46, f], [2, 96/46, f]).
step_right([2, 95/54, f], [2, 96/54, f]).
step_right([2, 95/56, f], [2, 96/56, f]).
step_right([2, 95/60, f], [2, 96/60, f]).
step_right([2, 95/62, f], [2, 96/62, f]).
step_right([2, 95/68, f], [2, 96/68, f]).
step_right([2, 95/70, f], [2, 96/70, f]).
step_right([2, 95/74, f], [2, 96/74, f]).
step_right([2, 95/76, f], [2, 96/76, f]).
step_right([2, 95/78, f], [2, 96/78, f]).
step_right([2, 95/82, f], [2, 96/82, f]).
step_right([2, 95/84, f], [2, 96/84, f]).
step_right([2, 95/88, f], [2, 96/88, f]).
step_right([2, 95/94, f], [2, 96/94, f]).
step_right([2, 95/96, f], [2, 96/96, f]).
step_right([2, 95/104, f], [2, 96/104, f]).
step_right([2, 95/106, f], [2, 96/106, f]).
step_right([2, 95/108, f], [2, 96/108, f]).
step_right([2, 95/112, f], [2, 96/112, f]).
step_right([2, 95/116, f], [2, 96/116, f]).
step_right([2, 95/118, f], [2, 96/118, f]).
step_right([2, 96/0, f], [2, 97/0, f]).
step_right([2, 96/2, f], [2, 97/2, f]).
step_right([2, 96/6, f], [2, 97/6, f]).
step_right([2, 96/14, f], [2, 97/14, f]).
step_right([2, 96/16, f], [2, 97/16, f]).
step_right([2, 96/18, f], [2, 97/18, f]).
step_right([2, 96/22, f], [2, 97/22, f]).
step_right([2, 96/24, f], [2, 97/24, f]).
step_right([2, 96/26, f], [2, 97/26, f]).
step_right([2, 96/32, f], [2, 97/32, f]).
step_right([2, 96/34, f], [2, 97/34, f]).
step_right([2, 96/40, f], [2, 97/40, f]).
step_right([2, 96/42, f], [2, 97/42, f]).
step_right([2, 96/48, f], [2, 97/48, f]).
step_right([2, 96/50, f], [2, 97/50, f]).
step_right([2, 96/52, f], [2, 97/52, f]).
step_right([2, 96/56, f], [2, 97/56, f]).
step_right([2, 96/58, f], [2, 97/58, f]).
step_right([2, 96/62, f], [2, 97/62, f]).
step_right([2, 96/64, f], [2, 97/64, f]).
step_right([2, 96/66, f], [2, 97/66, f]).
step_right([2, 96/70, f], [2, 97/70, f]).
step_right([2, 96/72, f], [2, 97/72, f]).
step_right([2, 96/76, f], [2, 97/76, f]).
step_right([2, 96/78, f], [2, 97/78, f]).
step_right([2, 96/80, f], [2, 97/80, f]).
step_right([2, 96/84, f], [2, 97/84, f]).
step_right([2, 96/86, f], [2, 97/86, f]).
step_right([2, 96/98, f], [2, 97/98, f]).
step_right([2, 96/100, f], [2, 97/100, f]).
step_right([2, 96/108, f], [2, 97/108, f]).
step_right([2, 96/114, f], [2, 97/114, f]).
step_right([2, 96/116, f], [2, 97/116, f]).
step_right([2, 96/118, f], [2, 97/118, f]).
step_right([2, 97/0, f], [2, 98/0, f]).
step_right([2, 97/2, f], [2, 98/2, f]).
step_right([2, 97/6, f], [2, 98/6, f]).
step_right([2, 97/14, f], [2, 98/14, f]).
step_right([2, 97/16, f], [2, 98/16, f]).
step_right([2, 97/18, f], [2, 98/18, f]).
step_right([2, 97/22, f], [2, 98/22, f]).
step_right([2, 97/24, f], [2, 98/24, f]).
step_right([2, 97/26, f], [2, 98/26, f]).
step_right([2, 97/32, f], [2, 98/32, f]).
step_right([2, 97/34, f], [2, 98/34, f]).
step_right([2, 97/40, f], [2, 98/40, f]).
step_right([2, 97/42, f], [2, 98/42, f]).
step_right([2, 97/48, f], [2, 98/48, f]).
step_right([2, 97/50, f], [2, 98/50, f]).
step_right([2, 97/52, f], [2, 98/52, f]).
step_right([2, 97/56, f], [2, 98/56, f]).
step_right([2, 97/58, f], [2, 98/58, f]).
step_right([2, 97/62, f], [2, 98/62, f]).
step_right([2, 97/64, f], [2, 98/64, f]).
step_right([2, 97/66, f], [2, 98/66, f]).
step_right([2, 97/70, f], [2, 98/70, f]).
step_right([2, 97/72, f], [2, 98/72, f]).
step_right([2, 97/76, f], [2, 98/76, f]).
step_right([2, 97/78, f], [2, 98/78, f]).
step_right([2, 97/80, f], [2, 98/80, f]).
step_right([2, 97/84, f], [2, 98/84, f]).
step_right([2, 97/86, f], [2, 98/86, f]).
step_right([2, 97/98, f], [2, 98/98, f]).
step_right([2, 97/100, f], [2, 98/100, f]).
step_right([2, 97/108, f], [2, 98/108, f]).
step_right([2, 97/114, f], [2, 98/114, f]).
step_right([2, 97/116, f], [2, 98/116, f]).
step_right([2, 97/118, f], [2, 98/118, f]).
step_right([2, 98/0, f], [2, 99/0, f]).
step_right([2, 98/2, f], [2, 99/2, f]).
step_right([2, 98/4, f], [2, 99/4, f]).
step_right([2, 98/10, f], [2, 99/10, f]).
step_right([2, 98/12, f], [2, 99/12, f]).
step_right([2, 98/14, f], [2, 99/14, f]).
step_right([2, 98/18, f], [2, 99/18, f]).
step_right([2, 98/20, f], [2, 99/20, f]).
step_right([2, 98/22, f], [2, 99/22, f]).
step_right([2, 98/24, f], [2, 99/24, f]).
step_right([2, 98/26, f], [2, 99/26, f]).
step_right([2, 98/28, f], [2, 99/28, f]).
step_right([2, 98/30, f], [2, 99/30, f]).
step_right([2, 98/38, f], [2, 99/38, f]).
step_right([2, 98/40, f], [2, 99/40, f]).
step_right([2, 98/42, f], [2, 99/42, f]).
step_right([2, 98/44, f], [2, 99/44, f]).
step_right([2, 98/48, f], [2, 99/48, f]).
step_right([2, 98/50, f], [2, 99/50, f]).
step_right([2, 98/54, f], [2, 99/54, f]).
step_right([2, 98/56, f], [2, 99/56, f]).
step_right([2, 98/60, f], [2, 99/60, f]).
step_right([2, 98/64, f], [2, 99/64, f]).
step_right([2, 98/66, f], [2, 99/66, f]).
step_right([2, 98/68, f], [2, 99/68, f]).
step_right([2, 98/72, f], [2, 99/72, f]).
step_right([2, 98/82, f], [2, 99/82, f]).
step_right([2, 98/84, f], [2, 99/84, f]).
step_right([2, 98/86, f], [2, 99/86, f]).
step_right([2, 98/88, f], [2, 99/88, f]).
step_right([2, 98/92, f], [2, 99/92, f]).
step_right([2, 98/94, f], [2, 99/94, f]).
step_right([2, 98/100, f], [2, 99/100, f]).
step_right([2, 98/104, f], [2, 99/104, f]).
step_right([2, 98/106, f], [2, 99/106, f]).
step_right([2, 98/108, f], [2, 99/108, f]).
step_right([2, 98/110, f], [2, 99/110, f]).
step_right([2, 98/112, f], [2, 99/112, f]).
step_right([2, 98/114, f], [2, 99/114, f]).
step_right([2, 98/118, f], [2, 99/118, f]).
step_right([2, 99/0, f], [2, 100/0, f]).
step_right([2, 99/2, f], [2, 100/2, f]).
step_right([2, 99/4, f], [2, 100/4, f]).
step_right([2, 99/10, f], [2, 100/10, f]).
step_right([2, 99/12, f], [2, 100/12, f]).
step_right([2, 99/14, f], [2, 100/14, f]).
step_right([2, 99/18, f], [2, 100/18, f]).
step_right([2, 99/20, f], [2, 100/20, f]).
step_right([2, 99/22, f], [2, 100/22, f]).
step_right([2, 99/24, f], [2, 100/24, f]).
step_right([2, 99/26, f], [2, 100/26, f]).
step_right([2, 99/28, f], [2, 100/28, f]).
step_right([2, 99/30, f], [2, 100/30, f]).
step_right([2, 99/38, f], [2, 100/38, f]).
step_right([2, 99/40, f], [2, 100/40, f]).
step_right([2, 99/42, f], [2, 100/42, f]).
step_right([2, 99/44, f], [2, 100/44, f]).
step_right([2, 99/48, f], [2, 100/48, f]).
step_right([2, 99/50, f], [2, 100/50, f]).
step_right([2, 99/54, f], [2, 100/54, f]).
step_right([2, 99/56, f], [2, 100/56, f]).
step_right([2, 99/60, f], [2, 100/60, f]).
step_right([2, 99/64, f], [2, 100/64, f]).
step_right([2, 99/66, f], [2, 100/66, f]).
step_right([2, 99/68, f], [2, 100/68, f]).
step_right([2, 99/72, f], [2, 100/72, f]).
step_right([2, 99/82, f], [2, 100/82, f]).
step_right([2, 99/84, f], [2, 100/84, f]).
step_right([2, 99/86, f], [2, 100/86, f]).
step_right([2, 99/88, f], [2, 100/88, f]).
step_right([2, 99/92, f], [2, 100/92, f]).
step_right([2, 99/94, f], [2, 100/94, f]).
step_right([2, 99/100, f], [2, 100/100, f]).
step_right([2, 99/104, f], [2, 100/104, f]).
step_right([2, 99/106, f], [2, 100/106, f]).
step_right([2, 99/108, f], [2, 100/108, f]).
step_right([2, 99/110, f], [2, 100/110, f]).
step_right([2, 99/112, f], [2, 100/112, f]).
step_right([2, 99/114, f], [2, 100/114, f]).
step_right([2, 99/118, f], [2, 100/118, f]).
step_right([2, 100/0, f], [2, 101/0, f]).
step_right([2, 100/14, f], [2, 101/14, f]).
step_right([2, 100/16, f], [2, 101/16, f]).
step_right([2, 100/22, f], [2, 101/22, f]).
step_right([2, 100/24, f], [2, 101/24, f]).
step_right([2, 100/26, f], [2, 101/26, f]).
step_right([2, 100/28, f], [2, 101/28, f]).
step_right([2, 100/36, f], [2, 101/36, f]).
step_right([2, 100/44, f], [2, 101/44, f]).
step_right([2, 100/46, f], [2, 101/46, f]).
step_right([2, 100/52, f], [2, 101/52, f]).
step_right([2, 100/54, f], [2, 101/54, f]).
step_right([2, 100/58, f], [2, 101/58, f]).
step_right([2, 100/60, f], [2, 101/60, f]).
step_right([2, 100/62, f], [2, 101/62, f]).
step_right([2, 100/70, f], [2, 101/70, f]).
step_right([2, 100/84, f], [2, 101/84, f]).
step_right([2, 100/86, f], [2, 101/86, f]).
step_right([2, 100/88, f], [2, 101/88, f]).
step_right([2, 100/90, f], [2, 101/90, f]).
step_right([2, 100/92, f], [2, 101/92, f]).
step_right([2, 100/98, f], [2, 101/98, f]).
step_right([2, 100/104, f], [2, 101/104, f]).
step_right([2, 100/106, f], [2, 101/106, f]).
step_right([2, 100/112, f], [2, 101/112, f]).
step_right([2, 100/114, f], [2, 101/114, f]).
step_right([2, 101/0, f], [2, 102/0, f]).
step_right([2, 101/14, f], [2, 102/14, f]).
step_right([2, 101/16, f], [2, 102/16, f]).
step_right([2, 101/22, f], [2, 102/22, f]).
step_right([2, 101/24, f], [2, 102/24, f]).
step_right([2, 101/26, f], [2, 102/26, f]).
step_right([2, 101/28, f], [2, 102/28, f]).
step_right([2, 101/36, f], [2, 102/36, f]).
step_right([2, 101/44, f], [2, 102/44, f]).
step_right([2, 101/46, f], [2, 102/46, f]).
step_right([2, 101/52, f], [2, 102/52, f]).
step_right([2, 101/54, f], [2, 102/54, f]).
step_right([2, 101/58, f], [2, 102/58, f]).
step_right([2, 101/60, f], [2, 102/60, f]).
step_right([2, 101/62, f], [2, 102/62, f]).
step_right([2, 101/70, f], [2, 102/70, f]).
step_right([2, 101/84, f], [2, 102/84, f]).
step_right([2, 101/86, f], [2, 102/86, f]).
step_right([2, 101/88, f], [2, 102/88, f]).
step_right([2, 101/90, f], [2, 102/90, f]).
step_right([2, 101/92, f], [2, 102/92, f]).
step_right([2, 101/98, f], [2, 102/98, f]).
step_right([2, 101/104, f], [2, 102/104, f]).
step_right([2, 101/106, f], [2, 102/106, f]).
step_right([2, 101/112, f], [2, 102/112, f]).
step_right([2, 101/114, f], [2, 102/114, f]).
step_right([2, 102/0, f], [2, 103/0, f]).
step_right([2, 102/2, f], [2, 103/2, f]).
step_right([2, 102/6, f], [2, 103/6, f]).
step_right([2, 102/14, f], [2, 103/14, f]).
step_right([2, 102/22, f], [2, 103/22, f]).
step_right([2, 102/28, f], [2, 103/28, f]).
step_right([2, 102/32, f], [2, 103/32, f]).
step_right([2, 102/40, f], [2, 103/40, f]).
step_right([2, 102/42, f], [2, 103/42, f]).
step_right([2, 102/44, f], [2, 103/44, f]).
step_right([2, 102/48, f], [2, 103/48, f]).
step_right([2, 102/50, f], [2, 103/50, f]).
step_right([2, 102/54, f], [2, 103/54, f]).
step_right([2, 102/56, f], [2, 103/56, f]).
step_right([2, 102/62, f], [2, 103/62, f]).
step_right([2, 102/64, f], [2, 103/64, f]).
step_right([2, 102/66, f], [2, 103/66, f]).
step_right([2, 102/70, f], [2, 103/70, f]).
step_right([2, 102/80, f], [2, 103/80, f]).
step_right([2, 102/82, f], [2, 103/82, f]).
step_right([2, 102/84, f], [2, 103/84, f]).
step_right([2, 102/90, f], [2, 103/90, f]).
step_right([2, 102/92, f], [2, 103/92, f]).
step_right([2, 102/94, f], [2, 103/94, f]).
step_right([2, 102/110, f], [2, 103/110, f]).
step_right([2, 102/112, f], [2, 103/112, f]).
step_right([2, 102/118, f], [2, 103/118, f]).
step_right([2, 103/0, f], [2, 104/0, f]).
step_right([2, 103/2, f], [2, 104/2, f]).
step_right([2, 103/6, f], [2, 104/6, f]).
step_right([2, 103/14, f], [2, 104/14, f]).
step_right([2, 103/22, f], [2, 104/22, f]).
step_right([2, 103/28, f], [2, 104/28, f]).
step_right([2, 103/32, f], [2, 104/32, f]).
step_right([2, 103/40, f], [2, 104/40, f]).
step_right([2, 103/42, f], [2, 104/42, f]).
step_right([2, 103/44, f], [2, 104/44, f]).
step_right([2, 103/48, f], [2, 104/48, f]).
step_right([2, 103/50, f], [2, 104/50, f]).
step_right([2, 103/54, f], [2, 104/54, f]).
step_right([2, 103/56, f], [2, 104/56, f]).
step_right([2, 103/62, f], [2, 104/62, f]).
step_right([2, 103/64, f], [2, 104/64, f]).
step_right([2, 103/66, f], [2, 104/66, f]).
step_right([2, 103/70, f], [2, 104/70, f]).
step_right([2, 103/80, f], [2, 104/80, f]).
step_right([2, 103/82, f], [2, 104/82, f]).
step_right([2, 103/84, f], [2, 104/84, f]).
step_right([2, 103/90, f], [2, 104/90, f]).
step_right([2, 103/92, f], [2, 104/92, f]).
step_right([2, 103/94, f], [2, 104/94, f]).
step_right([2, 103/110, f], [2, 104/110, f]).
step_right([2, 103/112, f], [2, 104/112, f]).
step_right([2, 103/118, f], [2, 104/118, f]).
step_right([2, 104/0, f], [2, 105/0, f]).
step_right([2, 104/4, f], [2, 105/4, f]).
step_right([2, 104/8, f], [2, 105/8, f]).
step_right([2, 104/14, f], [2, 105/14, f]).
step_right([2, 104/16, f], [2, 105/16, f]).
step_right([2, 104/18, f], [2, 105/18, f]).
step_right([2, 104/24, f], [2, 105/24, f]).
step_right([2, 104/26, f], [2, 105/26, f]).
step_right([2, 104/28, f], [2, 105/28, f]).
step_right([2, 104/30, f], [2, 105/30, f]).
step_right([2, 104/36, f], [2, 105/36, f]).
step_right([2, 104/38, f], [2, 105/38, f]).
step_right([2, 104/42, f], [2, 105/42, f]).
step_right([2, 104/46, f], [2, 105/46, f]).
step_right([2, 104/50, f], [2, 105/50, f]).
step_right([2, 104/52, f], [2, 105/52, f]).
step_right([2, 104/56, f], [2, 105/56, f]).
step_right([2, 104/58, f], [2, 105/58, f]).
step_right([2, 104/66, f], [2, 105/66, f]).
step_right([2, 104/72, f], [2, 105/72, f]).
step_right([2, 104/82, f], [2, 105/82, f]).
step_right([2, 104/84, f], [2, 105/84, f]).
step_right([2, 104/86, f], [2, 105/86, f]).
step_right([2, 104/90, f], [2, 105/90, f]).
step_right([2, 104/98, f], [2, 105/98, f]).
step_right([2, 104/100, f], [2, 105/100, f]).
step_right([2, 104/102, f], [2, 105/102, f]).
step_right([2, 104/106, f], [2, 105/106, f]).
step_right([2, 104/108, f], [2, 105/108, f]).
step_right([2, 104/116, f], [2, 105/116, f]).
step_right([2, 104/118, f], [2, 105/118, f]).
step_right([2, 105/0, f], [2, 106/0, f]).
step_right([2, 105/4, f], [2, 106/4, f]).
step_right([2, 105/8, f], [2, 106/8, f]).
step_right([2, 105/14, f], [2, 106/14, f]).
step_right([2, 105/16, f], [2, 106/16, f]).
step_right([2, 105/18, f], [2, 106/18, f]).
step_right([2, 105/24, f], [2, 106/24, f]).
step_right([2, 105/26, f], [2, 106/26, f]).
step_right([2, 105/28, f], [2, 106/28, f]).
step_right([2, 105/30, f], [2, 106/30, f]).
step_right([2, 105/36, f], [2, 106/36, f]).
step_right([2, 105/38, f], [2, 106/38, f]).
step_right([2, 105/42, f], [2, 106/42, f]).
step_right([2, 105/46, f], [2, 106/46, f]).
step_right([2, 105/50, f], [2, 106/50, f]).
step_right([2, 105/52, f], [2, 106/52, f]).
step_right([2, 105/56, f], [2, 106/56, f]).
step_right([2, 105/58, f], [2, 106/58, f]).
step_right([2, 105/66, f], [2, 106/66, f]).
step_right([2, 105/72, f], [2, 106/72, f]).
step_right([2, 105/82, f], [2, 106/82, f]).
step_right([2, 105/84, f], [2, 106/84, f]).
step_right([2, 105/86, f], [2, 106/86, f]).
step_right([2, 105/90, f], [2, 106/90, f]).
step_right([2, 105/98, f], [2, 106/98, f]).
step_right([2, 105/100, f], [2, 106/100, f]).
step_right([2, 105/102, f], [2, 106/102, f]).
step_right([2, 105/106, f], [2, 106/106, f]).
step_right([2, 105/108, f], [2, 106/108, f]).
step_right([2, 105/116, f], [2, 106/116, f]).
step_right([2, 105/118, f], [2, 106/118, f]).
step_right([2, 106/0, f], [2, 107/0, f]).
step_right([2, 106/2, f], [2, 107/2, f]).
step_right([2, 106/6, f], [2, 107/6, f]).
step_right([2, 106/12, f], [2, 107/12, f]).
step_right([2, 106/14, f], [2, 107/14, f]).
step_right([2, 106/16, f], [2, 107/16, f]).
step_right([2, 106/18, f], [2, 107/18, f]).
step_right([2, 106/26, f], [2, 107/26, f]).
step_right([2, 106/30, f], [2, 107/30, f]).
step_right([2, 106/32, f], [2, 107/32, f]).
step_right([2, 106/34, f], [2, 107/34, f]).
step_right([2, 106/36, f], [2, 107/36, f]).
step_right([2, 106/38, f], [2, 107/38, f]).
step_right([2, 106/40, f], [2, 107/40, f]).
step_right([2, 106/44, f], [2, 107/44, f]).
step_right([2, 106/46, f], [2, 107/46, f]).
step_right([2, 106/48, f], [2, 107/48, f]).
step_right([2, 106/54, f], [2, 107/54, f]).
step_right([2, 106/56, f], [2, 107/56, f]).
step_right([2, 106/58, f], [2, 107/58, f]).
step_right([2, 106/62, f], [2, 107/62, f]).
step_right([2, 106/64, f], [2, 107/64, f]).
step_right([2, 106/66, f], [2, 107/66, f]).
step_right([2, 106/68, f], [2, 107/68, f]).
step_right([2, 106/74, f], [2, 107/74, f]).
step_right([2, 106/78, f], [2, 107/78, f]).
step_right([2, 106/82, f], [2, 107/82, f]).
step_right([2, 106/86, f], [2, 107/86, f]).
step_right([2, 106/88, f], [2, 107/88, f]).
step_right([2, 106/92, f], [2, 107/92, f]).
step_right([2, 106/94, f], [2, 107/94, f]).
step_right([2, 106/96, f], [2, 107/96, f]).
step_right([2, 106/100, f], [2, 107/100, f]).
step_right([2, 106/104, f], [2, 107/104, f]).
step_right([2, 106/106, f], [2, 107/106, f]).
step_right([2, 106/116, f], [2, 107/116, f]).
step_right([2, 106/118, f], [2, 107/118, f]).
step_right([2, 107/0, f], [2, 108/0, f]).
step_right([2, 107/2, f], [2, 108/2, f]).
step_right([2, 107/6, f], [2, 108/6, f]).
step_right([2, 107/12, f], [2, 108/12, f]).
step_right([2, 107/14, f], [2, 108/14, f]).
step_right([2, 107/16, f], [2, 108/16, f]).
step_right([2, 107/18, f], [2, 108/18, f]).
step_right([2, 107/26, f], [2, 108/26, f]).
step_right([2, 107/30, f], [2, 108/30, f]).
step_right([2, 107/32, f], [2, 108/32, f]).
step_right([2, 107/34, f], [2, 108/34, f]).
step_right([2, 107/36, f], [2, 108/36, f]).
step_right([2, 107/38, f], [2, 108/38, f]).
step_right([2, 107/40, f], [2, 108/40, f]).
step_right([2, 107/44, f], [2, 108/44, f]).
step_right([2, 107/46, f], [2, 108/46, f]).
step_right([2, 107/48, f], [2, 108/48, f]).
step_right([2, 107/54, f], [2, 108/54, f]).
step_right([2, 107/56, f], [2, 108/56, f]).
step_right([2, 107/58, f], [2, 108/58, f]).
step_right([2, 107/62, f], [2, 108/62, f]).
step_right([2, 107/64, f], [2, 108/64, f]).
step_right([2, 107/66, f], [2, 108/66, f]).
step_right([2, 107/68, f], [2, 108/68, f]).
step_right([2, 107/74, f], [2, 108/74, f]).
step_right([2, 107/78, f], [2, 108/78, f]).
step_right([2, 107/82, f], [2, 108/82, f]).
step_right([2, 107/86, f], [2, 108/86, f]).
step_right([2, 107/88, f], [2, 108/88, f]).
step_right([2, 107/92, f], [2, 108/92, f]).
step_right([2, 107/94, f], [2, 108/94, f]).
step_right([2, 107/96, f], [2, 108/96, f]).
step_right([2, 107/100, f], [2, 108/100, f]).
step_right([2, 107/104, f], [2, 108/104, f]).
step_right([2, 107/106, f], [2, 108/106, f]).
step_right([2, 107/116, f], [2, 108/116, f]).
step_right([2, 107/118, f], [2, 108/118, f]).
step_right([2, 108/0, f], [2, 109/0, f]).
step_right([2, 108/2, f], [2, 109/2, f]).
step_right([2, 108/4, f], [2, 109/4, f]).
step_right([2, 108/6, f], [2, 109/6, f]).
step_right([2, 108/10, f], [2, 109/10, f]).
step_right([2, 108/12, f], [2, 109/12, f]).
step_right([2, 108/14, f], [2, 109/14, f]).
step_right([2, 108/16, f], [2, 109/16, f]).
step_right([2, 108/24, f], [2, 109/24, f]).
step_right([2, 108/26, f], [2, 109/26, f]).
step_right([2, 108/28, f], [2, 109/28, f]).
step_right([2, 108/30, f], [2, 109/30, f]).
step_right([2, 108/32, f], [2, 109/32, f]).
step_right([2, 108/38, f], [2, 109/38, f]).
step_right([2, 108/46, f], [2, 109/46, f]).
step_right([2, 108/50, f], [2, 109/50, f]).
step_right([2, 108/52, f], [2, 109/52, f]).
step_right([2, 108/64, f], [2, 109/64, f]).
step_right([2, 108/66, f], [2, 109/66, f]).
step_right([2, 108/72, f], [2, 109/72, f]).
step_right([2, 108/74, f], [2, 109/74, f]).
step_right([2, 108/76, f], [2, 109/76, f]).
step_right([2, 108/80, f], [2, 109/80, f]).
step_right([2, 108/82, f], [2, 109/82, f]).
step_right([2, 108/84, f], [2, 109/84, f]).
step_right([2, 108/88, f], [2, 109/88, f]).
step_right([2, 108/92, f], [2, 109/92, f]).
step_right([2, 108/94, f], [2, 109/94, f]).
step_right([2, 108/96, f], [2, 109/96, f]).
step_right([2, 108/98, f], [2, 109/98, f]).
step_right([2, 108/102, f], [2, 109/102, f]).
step_right([2, 108/106, f], [2, 109/106, f]).
step_right([2, 108/108, f], [2, 109/108, f]).
step_right([2, 108/118, f], [2, 109/118, f]).
step_right([2, 109/0, f], [2, 110/0, f]).
step_right([2, 109/2, f], [2, 110/2, f]).
step_right([2, 109/4, f], [2, 110/4, f]).
step_right([2, 109/6, f], [2, 110/6, f]).
step_right([2, 109/10, f], [2, 110/10, f]).
step_right([2, 109/12, f], [2, 110/12, f]).
step_right([2, 109/14, f], [2, 110/14, f]).
step_right([2, 109/16, f], [2, 110/16, f]).
step_right([2, 109/24, f], [2, 110/24, f]).
step_right([2, 109/26, f], [2, 110/26, f]).
step_right([2, 109/28, f], [2, 110/28, f]).
step_right([2, 109/30, f], [2, 110/30, f]).
step_right([2, 109/32, f], [2, 110/32, f]).
step_right([2, 109/38, f], [2, 110/38, f]).
step_right([2, 109/46, f], [2, 110/46, f]).
step_right([2, 109/50, f], [2, 110/50, f]).
step_right([2, 109/52, f], [2, 110/52, f]).
step_right([2, 109/64, f], [2, 110/64, f]).
step_right([2, 109/66, f], [2, 110/66, f]).
step_right([2, 109/72, f], [2, 110/72, f]).
step_right([2, 109/74, f], [2, 110/74, f]).
step_right([2, 109/76, f], [2, 110/76, f]).
step_right([2, 109/80, f], [2, 110/80, f]).
step_right([2, 109/82, f], [2, 110/82, f]).
step_right([2, 109/84, f], [2, 110/84, f]).
step_right([2, 109/88, f], [2, 110/88, f]).
step_right([2, 109/92, f], [2, 110/92, f]).
step_right([2, 109/94, f], [2, 110/94, f]).
step_right([2, 109/96, f], [2, 110/96, f]).
step_right([2, 109/98, f], [2, 110/98, f]).
step_right([2, 109/102, f], [2, 110/102, f]).
step_right([2, 109/106, f], [2, 110/106, f]).
step_right([2, 109/108, f], [2, 110/108, f]).
step_right([2, 109/118, f], [2, 110/118, f]).
step_right([2, 110/0, f], [2, 111/0, f]).
step_right([2, 110/2, f], [2, 111/2, f]).
step_right([2, 110/14, f], [2, 111/14, f]).
step_right([2, 110/16, f], [2, 111/16, f]).
step_right([2, 110/22, f], [2, 111/22, f]).
step_right([2, 110/24, f], [2, 111/24, f]).
step_right([2, 110/28, f], [2, 111/28, f]).
step_right([2, 110/34, f], [2, 111/34, f]).
step_right([2, 110/36, f], [2, 111/36, f]).
step_right([2, 110/40, f], [2, 111/40, f]).
step_right([2, 110/48, f], [2, 111/48, f]).
step_right([2, 110/50, f], [2, 111/50, f]).
step_right([2, 110/54, f], [2, 111/54, f]).
step_right([2, 110/66, f], [2, 111/66, f]).
step_right([2, 110/72, f], [2, 111/72, f]).
step_right([2, 110/76, f], [2, 111/76, f]).
step_right([2, 110/78, f], [2, 111/78, f]).
step_right([2, 110/86, f], [2, 111/86, f]).
step_right([2, 110/90, f], [2, 111/90, f]).
step_right([2, 110/94, f], [2, 111/94, f]).
step_right([2, 110/96, f], [2, 111/96, f]).
step_right([2, 110/98, f], [2, 111/98, f]).
step_right([2, 110/100, f], [2, 111/100, f]).
step_right([2, 110/102, f], [2, 111/102, f]).
step_right([2, 110/104, f], [2, 111/104, f]).
step_right([2, 110/108, f], [2, 111/108, f]).
step_right([2, 110/116, f], [2, 111/116, f]).
step_right([2, 110/118, f], [2, 111/118, f]).
step_right([2, 111/0, f], [2, 112/0, f]).
step_right([2, 111/2, f], [2, 112/2, f]).
step_right([2, 111/14, f], [2, 112/14, f]).
step_right([2, 111/16, f], [2, 112/16, f]).
step_right([2, 111/22, f], [2, 112/22, f]).
step_right([2, 111/24, f], [2, 112/24, f]).
step_right([2, 111/28, f], [2, 112/28, f]).
step_right([2, 111/34, f], [2, 112/34, f]).
step_right([2, 111/36, f], [2, 112/36, f]).
step_right([2, 111/40, f], [2, 112/40, f]).
step_right([2, 111/48, f], [2, 112/48, f]).
step_right([2, 111/50, f], [2, 112/50, f]).
step_right([2, 111/54, f], [2, 112/54, f]).
step_right([2, 111/66, f], [2, 112/66, f]).
step_right([2, 111/72, f], [2, 112/72, f]).
step_right([2, 111/76, f], [2, 112/76, f]).
step_right([2, 111/78, f], [2, 112/78, f]).
step_right([2, 111/86, f], [2, 112/86, f]).
step_right([2, 111/90, f], [2, 112/90, f]).
step_right([2, 111/94, f], [2, 112/94, f]).
step_right([2, 111/96, f], [2, 112/96, f]).
step_right([2, 111/98, f], [2, 112/98, f]).
step_right([2, 111/100, f], [2, 112/100, f]).
step_right([2, 111/102, f], [2, 112/102, f]).
step_right([2, 111/104, f], [2, 112/104, f]).
step_right([2, 111/108, f], [2, 112/108, f]).
step_right([2, 111/116, f], [2, 112/116, f]).
step_right([2, 111/118, f], [2, 112/118, f]).
step_right([2, 112/0, f], [2, 113/0, f]).
step_right([2, 112/8, f], [2, 113/8, f]).
step_right([2, 112/12, f], [2, 113/12, f]).
step_right([2, 112/16, f], [2, 113/16, f]).
step_right([2, 112/18, f], [2, 113/18, f]).
step_right([2, 112/20, f], [2, 113/20, f]).
step_right([2, 112/22, f], [2, 113/22, f]).
step_right([2, 112/26, f], [2, 113/26, f]).
step_right([2, 112/30, f], [2, 113/30, f]).
step_right([2, 112/32, f], [2, 113/32, f]).
step_right([2, 112/34, f], [2, 113/34, f]).
step_right([2, 112/36, f], [2, 113/36, f]).
step_right([2, 112/38, f], [2, 113/38, f]).
step_right([2, 112/44, f], [2, 113/44, f]).
step_right([2, 112/48, f], [2, 113/48, f]).
step_right([2, 112/52, f], [2, 113/52, f]).
step_right([2, 112/54, f], [2, 113/54, f]).
step_right([2, 112/56, f], [2, 113/56, f]).
step_right([2, 112/58, f], [2, 113/58, f]).
step_right([2, 112/66, f], [2, 113/66, f]).
step_right([2, 112/68, f], [2, 113/68, f]).
step_right([2, 112/70, f], [2, 113/70, f]).
step_right([2, 112/74, f], [2, 113/74, f]).
step_right([2, 112/84, f], [2, 113/84, f]).
step_right([2, 112/86, f], [2, 113/86, f]).
step_right([2, 112/88, f], [2, 113/88, f]).
step_right([2, 112/98, f], [2, 113/98, f]).
step_right([2, 112/100, f], [2, 113/100, f]).
step_right([2, 112/104, f], [2, 113/104, f]).
step_right([2, 112/106, f], [2, 113/106, f]).
step_right([2, 112/114, f], [2, 113/114, f]).
step_right([2, 112/116, f], [2, 113/116, f]).
step_right([2, 112/118, f], [2, 113/118, f]).
step_right([2, 113/0, f], [2, 114/0, f]).
step_right([2, 113/8, f], [2, 114/8, f]).
step_right([2, 113/12, f], [2, 114/12, f]).
step_right([2, 113/16, f], [2, 114/16, f]).
step_right([2, 113/18, f], [2, 114/18, f]).
step_right([2, 113/20, f], [2, 114/20, f]).
step_right([2, 113/22, f], [2, 114/22, f]).
step_right([2, 113/26, f], [2, 114/26, f]).
step_right([2, 113/30, f], [2, 114/30, f]).
step_right([2, 113/32, f], [2, 114/32, f]).
step_right([2, 113/34, f], [2, 114/34, f]).
step_right([2, 113/36, f], [2, 114/36, f]).
step_right([2, 113/38, f], [2, 114/38, f]).
step_right([2, 113/44, f], [2, 114/44, f]).
step_right([2, 113/48, f], [2, 114/48, f]).
step_right([2, 113/52, f], [2, 114/52, f]).
step_right([2, 113/54, f], [2, 114/54, f]).
step_right([2, 113/56, f], [2, 114/56, f]).
step_right([2, 113/58, f], [2, 114/58, f]).
step_right([2, 113/66, f], [2, 114/66, f]).
step_right([2, 113/68, f], [2, 114/68, f]).
step_right([2, 113/70, f], [2, 114/70, f]).
step_right([2, 113/74, f], [2, 114/74, f]).
step_right([2, 113/84, f], [2, 114/84, f]).
step_right([2, 113/86, f], [2, 114/86, f]).
step_right([2, 113/88, f], [2, 114/88, f]).
step_right([2, 113/98, f], [2, 114/98, f]).
step_right([2, 113/100, f], [2, 114/100, f]).
step_right([2, 113/104, f], [2, 114/104, f]).
step_right([2, 113/106, f], [2, 114/106, f]).
step_right([2, 113/114, f], [2, 114/114, f]).
step_right([2, 113/116, f], [2, 114/116, f]).
step_right([2, 113/118, f], [2, 114/118, f]).
step_right([2, 114/0, f], [2, 115/0, f]).
step_right([2, 114/2, f], [2, 115/2, f]).
step_right([2, 114/4, f], [2, 115/4, f]).
step_right([2, 114/6, f], [2, 115/6, f]).
step_right([2, 114/10, f], [2, 115/10, f]).
step_right([2, 114/18, f], [2, 115/18, f]).
step_right([2, 114/20, f], [2, 115/20, f]).
step_right([2, 114/26, f], [2, 115/26, f]).
step_right([2, 114/28, f], [2, 115/28, f]).
step_right([2, 114/32, f], [2, 115/32, f]).
step_right([2, 114/34, f], [2, 115/34, f]).
step_right([2, 114/40, f], [2, 115/40, f]).
step_right([2, 114/42, f], [2, 115/42, f]).
step_right([2, 114/44, f], [2, 115/44, f]).
step_right([2, 114/46, f], [2, 115/46, f]).
step_right([2, 114/50, f], [2, 115/50, f]).
step_right([2, 114/56, f], [2, 115/56, f]).
step_right([2, 114/60, f], [2, 115/60, f]).
step_right([2, 114/68, f], [2, 115/68, f]).
step_right([2, 114/70, f], [2, 115/70, f]).
step_right([2, 114/72, f], [2, 115/72, f]).
step_right([2, 114/76, f], [2, 115/76, f]).
step_right([2, 114/80, f], [2, 115/80, f]).
step_right([2, 114/82, f], [2, 115/82, f]).
step_right([2, 114/90, f], [2, 115/90, f]).
step_right([2, 114/92, f], [2, 115/92, f]).
step_right([2, 114/94, f], [2, 115/94, f]).
step_right([2, 114/102, f], [2, 115/102, f]).
step_right([2, 114/110, f], [2, 115/110, f]).
step_right([2, 114/112, f], [2, 115/112, f]).
step_right([2, 114/114, f], [2, 115/114, f]).
step_right([2, 114/118, f], [2, 115/118, f]).
step_right([2, 115/0, f], [2, 116/0, f]).
step_right([2, 115/2, f], [2, 116/2, f]).
step_right([2, 115/4, f], [2, 116/4, f]).
step_right([2, 115/6, f], [2, 116/6, f]).
step_right([2, 115/10, f], [2, 116/10, f]).
step_right([2, 115/18, f], [2, 116/18, f]).
step_right([2, 115/20, f], [2, 116/20, f]).
step_right([2, 115/26, f], [2, 116/26, f]).
step_right([2, 115/28, f], [2, 116/28, f]).
step_right([2, 115/32, f], [2, 116/32, f]).
step_right([2, 115/34, f], [2, 116/34, f]).
step_right([2, 115/40, f], [2, 116/40, f]).
step_right([2, 115/42, f], [2, 116/42, f]).
step_right([2, 115/44, f], [2, 116/44, f]).
step_right([2, 115/46, f], [2, 116/46, f]).
step_right([2, 115/50, f], [2, 116/50, f]).
step_right([2, 115/56, f], [2, 116/56, f]).
step_right([2, 115/60, f], [2, 116/60, f]).
step_right([2, 115/68, f], [2, 116/68, f]).
step_right([2, 115/70, f], [2, 116/70, f]).
step_right([2, 115/72, f], [2, 116/72, f]).
step_right([2, 115/76, f], [2, 116/76, f]).
step_right([2, 115/80, f], [2, 116/80, f]).
step_right([2, 115/82, f], [2, 116/82, f]).
step_right([2, 115/90, f], [2, 116/90, f]).
step_right([2, 115/92, f], [2, 116/92, f]).
step_right([2, 115/94, f], [2, 116/94, f]).
step_right([2, 115/102, f], [2, 116/102, f]).
step_right([2, 115/110, f], [2, 116/110, f]).
step_right([2, 115/112, f], [2, 116/112, f]).
step_right([2, 115/114, f], [2, 116/114, f]).
step_right([2, 115/118, f], [2, 116/118, f]).
step_right([2, 116/2, f], [2, 117/2, f]).
step_right([2, 116/4, f], [2, 117/4, f]).
step_right([2, 116/12, f], [2, 117/12, f]).
step_right([2, 116/16, f], [2, 117/16, f]).
step_right([2, 116/18, f], [2, 117/18, f]).
step_right([2, 116/22, f], [2, 117/22, f]).
step_right([2, 116/24, f], [2, 117/24, f]).
step_right([2, 116/30, f], [2, 117/30, f]).
step_right([2, 116/32, f], [2, 117/32, f]).
step_right([2, 116/34, f], [2, 117/34, f]).
step_right([2, 116/38, f], [2, 117/38, f]).
step_right([2, 116/40, f], [2, 117/40, f]).
step_right([2, 116/42, f], [2, 117/42, f]).
step_right([2, 116/44, f], [2, 117/44, f]).
step_right([2, 116/48, f], [2, 117/48, f]).
step_right([2, 116/52, f], [2, 117/52, f]).
step_right([2, 116/54, f], [2, 117/54, f]).
step_right([2, 116/62, f], [2, 117/62, f]).
step_right([2, 116/70, f], [2, 117/70, f]).
step_right([2, 116/74, f], [2, 117/74, f]).
step_right([2, 116/78, f], [2, 117/78, f]).
step_right([2, 116/80, f], [2, 117/80, f]).
step_right([2, 116/86, f], [2, 117/86, f]).
step_right([2, 116/88, f], [2, 117/88, f]).
step_right([2, 116/96, f], [2, 117/96, f]).
step_right([2, 116/112, f], [2, 117/112, f]).
step_right([2, 116/116, f], [2, 117/116, f]).
step_right([2, 116/118, f], [2, 117/118, f]).
step_right([2, 117/2, f], [2, 118/2, f]).
step_right([2, 117/4, f], [2, 118/4, f]).
step_right([2, 117/12, f], [2, 118/12, f]).
step_right([2, 117/16, f], [2, 118/16, f]).
step_right([2, 117/18, f], [2, 118/18, f]).
step_right([2, 117/22, f], [2, 118/22, f]).
step_right([2, 117/24, f], [2, 118/24, f]).
step_right([2, 117/30, f], [2, 118/30, f]).
step_right([2, 117/32, f], [2, 118/32, f]).
step_right([2, 117/34, f], [2, 118/34, f]).
step_right([2, 117/38, f], [2, 118/38, f]).
step_right([2, 117/40, f], [2, 118/40, f]).
step_right([2, 117/42, f], [2, 118/42, f]).
step_right([2, 117/44, f], [2, 118/44, f]).
step_right([2, 117/48, f], [2, 118/48, f]).
step_right([2, 117/52, f], [2, 118/52, f]).
step_right([2, 117/54, f], [2, 118/54, f]).
step_right([2, 117/62, f], [2, 118/62, f]).
step_right([2, 117/70, f], [2, 118/70, f]).
step_right([2, 117/74, f], [2, 118/74, f]).
step_right([2, 117/78, f], [2, 118/78, f]).
step_right([2, 117/80, f], [2, 118/80, f]).
step_right([2, 117/86, f], [2, 118/86, f]).
step_right([2, 117/88, f], [2, 118/88, f]).
step_right([2, 117/96, f], [2, 118/96, f]).
step_right([2, 117/112, f], [2, 118/112, f]).
step_right([2, 117/116, f], [2, 118/116, f]).
step_right([2, 117/118, f], [2, 118/118, f]).
step_up([2, 0/0, f], [2, 0/1, f]).
step_up([2, 0/1, f], [2, 0/2, f]).
step_up([2, 0/2, f], [2, 0/3, f]).
step_up([2, 0/3, f], [2, 0/4, f]).
step_up([2, 0/4, f], [2, 0/5, f]).
step_up([2, 0/5, f], [2, 0/6, f]).
step_up([2, 0/6, f], [2, 0/7, f]).
step_up([2, 0/7, f], [2, 0/8, f]).
step_up([2, 0/8, f], [2, 0/9, f]).
step_up([2, 0/9, f], [2, 0/10, f]).
step_up([2, 0/12, f], [2, 0/13, f]).
step_up([2, 0/13, f], [2, 0/14, f]).
step_up([2, 0/14, f], [2, 0/15, f]).
step_up([2, 0/15, f], [2, 0/16, f]).
step_up([2, 0/16, f], [2, 0/17, f]).
step_up([2, 0/17, f], [2, 0/18, f]).
step_up([2, 0/20, f], [2, 0/21, f]).
step_up([2, 0/21, f], [2, 0/22, f]).
step_up([2, 0/24, f], [2, 0/25, f]).
step_up([2, 0/25, f], [2, 0/26, f]).
step_up([2, 0/26, f], [2, 0/27, f]).
step_up([2, 0/27, f], [2, 0/28, f]).
step_up([2, 0/28, f], [2, 0/29, f]).
step_up([2, 0/29, f], [2, 0/30, f]).
step_up([2, 0/30, f], [2, 0/31, f]).
step_up([2, 0/31, f], [2, 0/32, f]).
step_up([2, 0/32, f], [2, 0/33, f]).
step_up([2, 0/33, f], [2, 0/34, f]).
step_up([2, 0/34, f], [2, 0/35, f]).
step_up([2, 0/35, f], [2, 0/36, f]).
step_up([2, 0/38, f], [2, 0/39, f]).
step_up([2, 0/39, f], [2, 0/40, f]).
step_up([2, 0/42, f], [2, 0/43, f]).
step_up([2, 0/43, f], [2, 0/44, f]).
step_up([2, 0/44, f], [2, 0/45, f]).
step_up([2, 0/45, f], [2, 0/46, f]).
step_up([2, 0/46, f], [2, 0/47, f]).
step_up([2, 0/47, f], [2, 0/48, f]).
step_up([2, 0/50, f], [2, 0/51, f]).
step_up([2, 0/51, f], [2, 0/52, f]).
step_up([2, 0/52, f], [2, 0/53, f]).
step_up([2, 0/53, f], [2, 0/54, f]).
step_up([2, 0/54, f], [2, 0/55, f]).
step_up([2, 0/55, f], [2, 0/56, f]).
step_up([2, 0/58, f], [2, 0/59, f]).
step_up([2, 0/59, f], [2, 0/60, f]).
step_up([2, 0/60, f], [2, 0/61, f]).
step_up([2, 0/61, f], [2, 0/62, f]).
step_up([2, 0/62, f], [2, 0/63, f]).
step_up([2, 0/63, f], [2, 0/64, f]).
step_up([2, 0/64, f], [2, 0/65, f]).
step_up([2, 0/65, f], [2, 0/66, f]).
step_up([2, 0/66, f], [2, 0/67, f]).
step_up([2, 0/67, f], [2, 0/68, f]).
step_up([2, 0/68, f], [2, 0/69, f]).
step_up([2, 0/69, f], [2, 0/70, f]).
step_up([2, 0/72, f], [2, 0/73, f]).
step_up([2, 0/73, f], [2, 0/74, f]).
step_up([2, 0/74, f], [2, 0/75, f]).
step_up([2, 0/75, f], [2, 0/76, f]).
step_up([2, 0/76, f], [2, 0/77, f]).
step_up([2, 0/77, f], [2, 0/78, f]).
step_up([2, 0/78, f], [2, 0/79, f]).
step_up([2, 0/79, f], [2, 0/80, f]).
step_up([2, 0/84, f], [2, 0/85, f]).
step_up([2, 0/85, f], [2, 0/86, f]).
step_up([2, 0/86, f], [2, 0/87, f]).
step_up([2, 0/87, f], [2, 0/88, f]).
step_up([2, 0/88, f], [2, 0/89, f]).
step_up([2, 0/89, f], [2, 0/90, f]).
step_up([2, 0/90, f], [2, 0/91, f]).
step_up([2, 0/91, f], [2, 0/92, f]).
step_up([2, 0/92, f], [2, 0/93, f]).
step_up([2, 0/93, f], [2, 0/94, f]).
step_up([2, 0/94, f], [2, 0/95, f]).
step_up([2, 0/95, f], [2, 0/96, f]).
step_up([2, 0/96, f], [2, 0/97, f]).
step_up([2, 0/97, f], [2, 0/98, f]).
step_up([2, 0/100, f], [2, 0/101, f]).
step_up([2, 0/101, f], [2, 0/102, f]).
step_up([2, 0/102, f], [2, 0/103, f]).
step_up([2, 0/103, f], [2, 0/104, f]).
step_up([2, 0/104, f], [2, 0/105, f]).
step_up([2, 0/105, f], [2, 0/106, f]).
step_up([2, 0/106, f], [2, 0/107, f]).
step_up([2, 0/107, f], [2, 0/108, f]).
step_up([2, 0/108, f], [2, 0/109, f]).
step_up([2, 0/109, f], [2, 0/110, f]).
step_up([2, 0/112, f], [2, 0/113, f]).
step_up([2, 0/113, f], [2, 0/114, f]).
step_up([2, 0/114, f], [2, 0/115, f]).
step_up([2, 0/115, f], [2, 0/116, f]).
step_up([2, 0/116, f], [2, 0/117, f]).
step_up([2, 0/117, f], [2, 0/118, s]).
step_up([2, 2/0, f], [2, 2/1, f]).
step_up([2, 2/1, f], [2, 2/2, f]).
step_up([2, 2/2, f], [2, 2/3, f]).
step_up([2, 2/3, f], [2, 2/4, f]).
step_up([2, 2/6, f], [2, 2/7, f]).
step_up([2, 2/7, f], [2, 2/8, f]).
step_up([2, 2/8, f], [2, 2/9, f]).
step_up([2, 2/9, f], [2, 2/10, f]).
step_up([2, 2/16, f], [2, 2/17, f]).
step_up([2, 2/17, f], [2, 2/18, f]).
step_up([2, 2/18, f], [2, 2/19, f]).
step_up([2, 2/19, f], [2, 2/20, f]).
step_up([2, 2/24, f], [2, 2/25, f]).
step_up([2, 2/25, f], [2, 2/26, f]).
step_up([2, 2/26, f], [2, 2/27, f]).
step_up([2, 2/27, f], [2, 2/28, f]).
step_up([2, 2/32, f], [2, 2/33, f]).
step_up([2, 2/33, f], [2, 2/34, f]).
step_up([2, 2/36, f], [2, 2/37, f]).
step_up([2, 2/37, f], [2, 2/38, f]).
step_up([2, 2/40, f], [2, 2/41, f]).
step_up([2, 2/41, f], [2, 2/42, f]).
step_up([2, 2/42, f], [2, 2/43, f]).
step_up([2, 2/43, f], [2, 2/44, f]).
step_up([2, 2/52, f], [2, 2/53, f]).
step_up([2, 2/53, f], [2, 2/54, f]).
step_up([2, 2/54, f], [2, 2/55, f]).
step_up([2, 2/55, f], [2, 2/56, f]).
step_up([2, 2/60, f], [2, 2/61, f]).
step_up([2, 2/61, f], [2, 2/62, f]).
step_up([2, 2/66, f], [2, 2/67, f]).
step_up([2, 2/67, f], [2, 2/68, f]).
step_up([2, 2/68, f], [2, 2/69, f]).
step_up([2, 2/69, f], [2, 2/70, f]).
step_up([2, 2/70, f], [2, 2/71, f]).
step_up([2, 2/71, f], [2, 2/72, f]).
step_up([2, 2/74, f], [2, 2/75, f]).
step_up([2, 2/75, f], [2, 2/76, f]).
step_up([2, 2/78, f], [2, 2/79, f]).
step_up([2, 2/79, f], [2, 2/80, f]).
step_up([2, 2/84, f], [2, 2/85, f]).
step_up([2, 2/85, f], [2, 2/86, f]).
step_up([2, 2/86, f], [2, 2/87, f]).
step_up([2, 2/87, f], [2, 2/88, f]).
step_up([2, 2/90, f], [2, 2/91, f]).
step_up([2, 2/91, f], [2, 2/92, f]).
step_up([2, 2/92, f], [2, 2/93, f]).
step_up([2, 2/93, f], [2, 2/94, f]).
step_up([2, 2/96, f], [2, 2/97, f]).
step_up([2, 2/97, f], [2, 2/98, f]).
step_up([2, 2/104, f], [2, 2/105, f]).
step_up([2, 2/105, f], [2, 2/106, f]).
step_up([2, 2/108, f], [2, 2/109, f]).
step_up([2, 2/109, f], [2, 2/110, f]).
step_up([2, 2/110, f], [2, 2/111, f]).
step_up([2, 2/111, f], [2, 2/112, f]).
step_up([2, 2/114, f], [2, 2/115, f]).
step_up([2, 2/115, f], [2, 2/116, f]).
step_up([2, 2/116, f], [2, 2/117, f]).
step_up([2, 2/117, f], [2, 2/118, f]).
step_up([2, 4/2, f], [2, 4/3, f]).
step_up([2, 4/3, f], [2, 4/4, f]).
step_up([2, 4/8, f], [2, 4/9, f]).
step_up([2, 4/9, f], [2, 4/10, f]).
step_up([2, 4/14, f], [2, 4/15, f]).
step_up([2, 4/15, f], [2, 4/16, f]).
step_up([2, 4/16, f], [2, 4/17, f]).
step_up([2, 4/17, f], [2, 4/18, f]).
step_up([2, 4/22, f], [2, 4/23, f]).
step_up([2, 4/23, f], [2, 4/24, f]).
step_up([2, 4/24, f], [2, 4/25, f]).
step_up([2, 4/25, f], [2, 4/26, f]).
step_up([2, 4/32, f], [2, 4/33, f]).
step_up([2, 4/33, f], [2, 4/34, f]).
step_up([2, 4/34, f], [2, 4/35, f]).
step_up([2, 4/35, f], [2, 4/36, f]).
step_up([2, 4/36, f], [2, 4/37, f]).
step_up([2, 4/37, f], [2, 4/38, f]).
step_up([2, 4/38, f], [2, 4/39, f]).
step_up([2, 4/39, f], [2, 4/40, f]).
step_up([2, 4/40, f], [2, 4/41, f]).
step_up([2, 4/41, f], [2, 4/42, f]).
step_up([2, 4/42, f], [2, 4/43, f]).
step_up([2, 4/43, f], [2, 4/44, f]).
step_up([2, 4/48, f], [2, 4/49, f]).
step_up([2, 4/49, f], [2, 4/50, f]).
step_up([2, 4/54, f], [2, 4/55, f]).
step_up([2, 4/55, f], [2, 4/56, f]).
step_up([2, 4/56, f], [2, 4/57, f]).
step_up([2, 4/57, f], [2, 4/58, f]).
step_up([2, 4/68, f], [2, 4/69, f]).
step_up([2, 4/69, f], [2, 4/70, f]).
step_up([2, 4/70, f], [2, 4/71, f]).
step_up([2, 4/71, f], [2, 4/72, f]).
step_up([2, 4/72, f], [2, 4/73, f]).
step_up([2, 4/73, f], [2, 4/74, f]).
step_up([2, 4/76, f], [2, 4/77, f]).
step_up([2, 4/77, f], [2, 4/78, f]).
step_up([2, 4/84, f], [2, 4/85, f]).
step_up([2, 4/85, f], [2, 4/86, f]).
step_up([2, 4/90, f], [2, 4/91, f]).
step_up([2, 4/91, f], [2, 4/92, f]).
step_up([2, 4/94, f], [2, 4/95, f]).
step_up([2, 4/95, f], [2, 4/96, f]).
step_up([2, 4/98, f], [2, 4/99, f]).
step_up([2, 4/99, f], [2, 4/100, f]).
step_up([2, 4/104, f], [2, 4/105, f]).
step_up([2, 4/105, f], [2, 4/106, f]).
step_up([2, 4/108, f], [2, 4/109, f]).
step_up([2, 4/109, f], [2, 4/110, f]).
step_up([2, 4/112, f], [2, 4/113, f]).
step_up([2, 4/113, f], [2, 4/114, f]).
step_up([2, 6/2, f], [2, 6/3, f]).
step_up([2, 6/3, f], [2, 6/4, f]).
step_up([2, 6/6, f], [2, 6/7, f]).
step_up([2, 6/7, f], [2, 6/8, f]).
step_up([2, 6/10, f], [2, 6/11, f]).
step_up([2, 6/11, f], [2, 6/12, f]).
step_up([2, 6/14, f], [2, 6/15, f]).
step_up([2, 6/15, f], [2, 6/16, f]).
step_up([2, 6/18, f], [2, 6/19, f]).
step_up([2, 6/19, f], [2, 6/20, f]).
step_up([2, 6/20, f], [2, 6/21, f]).
step_up([2, 6/21, f], [2, 6/22, f]).
step_up([2, 6/22, f], [2, 6/23, f]).
step_up([2, 6/23, f], [2, 6/24, f]).
step_up([2, 6/26, f], [2, 6/27, f]).
step_up([2, 6/27, f], [2, 6/28, f]).
step_up([2, 6/32, f], [2, 6/33, f]).
step_up([2, 6/33, f], [2, 6/34, f]).
step_up([2, 6/34, f], [2, 6/35, f]).
step_up([2, 6/35, f], [2, 6/36, f]).
step_up([2, 6/38, f], [2, 6/39, f]).
step_up([2, 6/39, f], [2, 6/40, f]).
step_up([2, 6/42, f], [2, 6/43, f]).
step_up([2, 6/43, f], [2, 6/44, f]).
step_up([2, 6/46, f], [2, 6/47, f]).
step_up([2, 6/47, f], [2, 6/48, f]).
step_up([2, 6/48, f], [2, 6/49, f]).
step_up([2, 6/49, f], [2, 6/50, f]).
step_up([2, 6/52, f], [2, 6/53, f]).
step_up([2, 6/53, f], [2, 6/54, f]).
step_up([2, 6/58, f], [2, 6/59, f]).
step_up([2, 6/59, f], [2, 6/60, f]).
step_up([2, 6/62, f], [2, 6/63, f]).
step_up([2, 6/63, f], [2, 6/64, f]).
step_up([2, 6/64, f], [2, 6/65, f]).
step_up([2, 6/65, f], [2, 6/66, f]).
step_up([2, 6/68, f], [2, 6/69, f]).
step_up([2, 6/69, f], [2, 6/70, f]).
step_up([2, 6/70, f], [2, 6/71, f]).
step_up([2, 6/71, f], [2, 6/72, f]).
step_up([2, 6/72, f], [2, 6/73, f]).
step_up([2, 6/73, f], [2, 6/74, f]).
step_up([2, 6/74, f], [2, 6/75, f]).
step_up([2, 6/75, f], [2, 6/76, f]).
step_up([2, 6/80, f], [2, 6/81, f]).
step_up([2, 6/81, f], [2, 6/82, f]).
step_up([2, 6/90, f], [2, 6/91, f]).
step_up([2, 6/91, f], [2, 6/92, f]).
step_up([2, 6/94, f], [2, 6/95, f]).
step_up([2, 6/95, f], [2, 6/96, f]).
step_up([2, 6/96, f], [2, 6/97, f]).
step_up([2, 6/97, f], [2, 6/98, f]).
step_up([2, 6/98, f], [2, 6/99, f]).
step_up([2, 6/99, f], [2, 6/100, f]).
step_up([2, 6/100, f], [2, 6/101, f]).
step_up([2, 6/101, f], [2, 6/102, f]).
step_up([2, 6/108, f], [2, 6/109, f]).
step_up([2, 6/109, f], [2, 6/110, f]).
step_up([2, 6/114, f], [2, 6/115, f]).
step_up([2, 6/115, f], [2, 6/116, f]).
step_up([2, 6/116, f], [2, 6/117, f]).
step_up([2, 6/117, f], [2, 6/118, f]).
step_up([2, 8/2, f], [2, 8/3, f]).
step_up([2, 8/3, f], [2, 8/4, f]).
step_up([2, 8/6, f], [2, 8/7, f]).
step_up([2, 8/7, f], [2, 8/8, f]).
step_up([2, 8/10, f], [2, 8/11, f]).
step_up([2, 8/11, f], [2, 8/12, f]).
step_up([2, 8/12, f], [2, 8/13, f]).
step_up([2, 8/13, f], [2, 8/14, f]).
step_up([2, 8/18, f], [2, 8/19, f]).
step_up([2, 8/19, f], [2, 8/20, f]).
step_up([2, 8/22, f], [2, 8/23, f]).
step_up([2, 8/23, f], [2, 8/24, f]).
step_up([2, 8/24, f], [2, 8/25, f]).
step_up([2, 8/25, f], [2, 8/26, f]).
step_up([2, 8/26, f], [2, 8/27, f]).
step_up([2, 8/27, f], [2, 8/28, f]).
step_up([2, 8/28, f], [2, 8/29, f]).
step_up([2, 8/29, f], [2, 8/30, f]).
step_up([2, 8/32, f], [2, 8/33, f]).
step_up([2, 8/33, f], [2, 8/34, f]).
step_up([2, 8/36, f], [2, 8/37, f]).
step_up([2, 8/37, f], [2, 8/38, f]).
step_up([2, 8/42, f], [2, 8/43, f]).
step_up([2, 8/43, f], [2, 8/44, f]).
step_up([2, 8/48, f], [2, 8/49, f]).
step_up([2, 8/49, f], [2, 8/50, f]).
step_up([2, 8/50, f], [2, 8/51, f]).
step_up([2, 8/51, f], [2, 8/52, f]).
step_up([2, 8/54, f], [2, 8/55, f]).
step_up([2, 8/55, f], [2, 8/56, f]).
step_up([2, 8/60, f], [2, 8/61, f]).
step_up([2, 8/61, f], [2, 8/62, f]).
step_up([2, 8/64, f], [2, 8/65, f]).
step_up([2, 8/65, f], [2, 8/66, f]).
step_up([2, 8/66, f], [2, 8/67, f]).
step_up([2, 8/67, f], [2, 8/68, f]).
step_up([2, 8/68, f], [2, 8/69, f]).
step_up([2, 8/69, f], [2, 8/70, f]).
step_up([2, 8/70, f], [2, 8/71, f]).
step_up([2, 8/71, f], [2, 8/72, f]).
step_up([2, 8/72, f], [2, 8/73, f]).
step_up([2, 8/73, f], [2, 8/74, f]).
step_up([2, 8/74, f], [2, 8/75, f]).
step_up([2, 8/75, f], [2, 8/76, f]).
step_up([2, 8/80, f], [2, 8/81, f]).
step_up([2, 8/81, f], [2, 8/82, f]).
step_up([2, 8/90, f], [2, 8/91, f]).
step_up([2, 8/91, f], [2, 8/92, f]).
step_up([2, 8/92, f], [2, 8/93, f]).
step_up([2, 8/93, f], [2, 8/94, f]).
step_up([2, 8/94, f], [2, 8/95, f]).
step_up([2, 8/95, f], [2, 8/96, f]).
step_up([2, 8/96, f], [2, 8/97, f]).
step_up([2, 8/97, f], [2, 8/98, f]).
step_up([2, 8/102, f], [2, 8/103, f]).
step_up([2, 8/103, f], [2, 8/104, f]).
step_up([2, 8/104, f], [2, 8/105, f]).
step_up([2, 8/105, f], [2, 8/106, f]).
step_up([2, 8/110, f], [2, 8/111, f]).
step_up([2, 8/111, f], [2, 8/112, f]).
step_up([2, 8/114, f], [2, 8/115, f]).
step_up([2, 8/115, f], [2, 8/116, f]).
step_up([2, 8/116, f], [2, 8/117, f]).
step_up([2, 8/117, f], [2, 8/118, f]).
step_up([2, 10/0, f], [2, 10/1, f]).
step_up([2, 10/1, f], [2, 10/2, f]).
step_up([2, 10/4, f], [2, 10/5, f]).
step_up([2, 10/5, f], [2, 10/6, f]).
step_up([2, 10/10, f], [2, 10/11, f]).
step_up([2, 10/11, f], [2, 10/12, f]).
step_up([2, 10/14, f], [2, 10/15, f]).
step_up([2, 10/15, f], [2, 10/16, f]).
step_up([2, 10/20, f], [2, 10/21, f]).
step_up([2, 10/21, f], [2, 10/22, f]).
step_up([2, 10/24, f], [2, 10/25, f]).
step_up([2, 10/25, f], [2, 10/26, f]).
step_up([2, 10/28, f], [2, 10/29, f]).
step_up([2, 10/29, f], [2, 10/30, f]).
step_up([2, 10/30, f], [2, 10/31, f]).
step_up([2, 10/31, f], [2, 10/32, f]).
step_up([2, 10/36, f], [2, 10/37, f]).
step_up([2, 10/37, f], [2, 10/38, f]).
step_up([2, 10/40, f], [2, 10/41, f]).
step_up([2, 10/41, f], [2, 10/42, f]).
step_up([2, 10/42, f], [2, 10/43, f]).
step_up([2, 10/43, f], [2, 10/44, f]).
step_up([2, 10/46, f], [2, 10/47, f]).
step_up([2, 10/47, f], [2, 10/48, f]).
step_up([2, 10/48, f], [2, 10/49, f]).
step_up([2, 10/49, f], [2, 10/50, f]).
step_up([2, 10/52, f], [2, 10/53, f]).
step_up([2, 10/53, f], [2, 10/54, f]).
step_up([2, 10/58, f], [2, 10/59, f]).
step_up([2, 10/59, f], [2, 10/60, f]).
step_up([2, 10/64, f], [2, 10/65, f]).
step_up([2, 10/65, f], [2, 10/66, f]).
step_up([2, 10/66, f], [2, 10/67, f]).
step_up([2, 10/67, f], [2, 10/68, f]).
step_up([2, 10/68, f], [2, 10/69, f]).
step_up([2, 10/69, f], [2, 10/70, f]).
step_up([2, 10/70, f], [2, 10/71, f]).
step_up([2, 10/71, f], [2, 10/72, f]).
step_up([2, 10/72, f], [2, 10/73, f]).
step_up([2, 10/73, f], [2, 10/74, f]).
step_up([2, 10/78, f], [2, 10/79, f]).
step_up([2, 10/79, f], [2, 10/80, f]).
step_up([2, 10/80, f], [2, 10/81, f]).
step_up([2, 10/81, f], [2, 10/82, f]).
step_up([2, 10/86, f], [2, 10/87, f]).
step_up([2, 10/87, f], [2, 10/88, f]).
step_up([2, 10/90, f], [2, 10/91, f]).
step_up([2, 10/91, f], [2, 10/92, f]).
step_up([2, 10/96, f], [2, 10/97, f]).
step_up([2, 10/97, f], [2, 10/98, f]).
step_up([2, 10/100, f], [2, 10/101, f]).
step_up([2, 10/101, f], [2, 10/102, f]).
step_up([2, 10/106, f], [2, 10/107, f]).
step_up([2, 10/107, f], [2, 10/108, f]).
step_up([2, 10/110, f], [2, 10/111, f]).
step_up([2, 10/111, f], [2, 10/112, f]).
step_up([2, 10/114, f], [2, 10/115, f]).
step_up([2, 10/115, f], [2, 10/116, f]).
step_up([2, 10/116, f], [2, 10/117, f]).
step_up([2, 10/117, f], [2, 10/118, f]).
step_up([2, 12/2, f], [2, 12/3, f]).
step_up([2, 12/3, f], [2, 12/4, f]).
step_up([2, 12/4, f], [2, 12/5, f]).
step_up([2, 12/5, f], [2, 12/6, f]).
step_up([2, 12/8, f], [2, 12/9, f]).
step_up([2, 12/9, f], [2, 12/10, f]).
step_up([2, 12/14, f], [2, 12/15, f]).
step_up([2, 12/15, f], [2, 12/16, f]).
step_up([2, 12/20, f], [2, 12/21, f]).
step_up([2, 12/21, f], [2, 12/22, f]).
step_up([2, 12/30, f], [2, 12/31, f]).
step_up([2, 12/31, f], [2, 12/32, f]).
step_up([2, 12/34, f], [2, 12/35, f]).
step_up([2, 12/35, f], [2, 12/36, f]).
step_up([2, 12/38, f], [2, 12/39, f]).
step_up([2, 12/39, f], [2, 12/40, f]).
step_up([2, 12/44, f], [2, 12/45, f]).
step_up([2, 12/45, f], [2, 12/46, f]).
step_up([2, 12/46, f], [2, 12/47, f]).
step_up([2, 12/47, f], [2, 12/48, f]).
step_up([2, 12/50, f], [2, 12/51, f]).
step_up([2, 12/51, f], [2, 12/52, f]).
step_up([2, 12/52, f], [2, 12/53, f]).
step_up([2, 12/53, f], [2, 12/54, f]).
step_up([2, 12/58, f], [2, 12/59, f]).
step_up([2, 12/59, f], [2, 12/60, f]).
step_up([2, 12/62, f], [2, 12/63, f]).
step_up([2, 12/63, f], [2, 12/64, f]).
step_up([2, 12/68, f], [2, 12/69, f]).
step_up([2, 12/69, f], [2, 12/70, f]).
step_up([2, 12/70, f], [2, 12/71, f]).
step_up([2, 12/71, f], [2, 12/72, f]).
step_up([2, 12/74, f], [2, 12/75, f]).
step_up([2, 12/75, f], [2, 12/76, f]).
step_up([2, 12/78, f], [2, 12/79, f]).
step_up([2, 12/79, f], [2, 12/80, f]).
step_up([2, 12/80, f], [2, 12/81, f]).
step_up([2, 12/81, f], [2, 12/82, f]).
step_up([2, 12/86, f], [2, 12/87, f]).
step_up([2, 12/87, f], [2, 12/88, f]).
step_up([2, 12/92, f], [2, 12/93, f]).
step_up([2, 12/93, f], [2, 12/94, f]).
step_up([2, 12/98, f], [2, 12/99, f]).
step_up([2, 12/99, f], [2, 12/100, f]).
step_up([2, 12/102, f], [2, 12/103, f]).
step_up([2, 12/103, f], [2, 12/104, f]).
step_up([2, 12/108, f], [2, 12/109, f]).
step_up([2, 12/109, f], [2, 12/110, f]).
step_up([2, 12/112, f], [2, 12/113, f]).
step_up([2, 12/113, f], [2, 12/114, f]).
step_up([2, 12/114, f], [2, 12/115, f]).
step_up([2, 12/115, f], [2, 12/116, f]).
step_up([2, 14/2, f], [2, 14/3, f]).
step_up([2, 14/3, f], [2, 14/4, f]).
step_up([2, 14/4, f], [2, 14/5, f]).
step_up([2, 14/5, f], [2, 14/6, f]).
step_up([2, 14/8, f], [2, 14/9, f]).
step_up([2, 14/9, f], [2, 14/10, f]).
step_up([2, 14/10, f], [2, 14/11, f]).
step_up([2, 14/11, f], [2, 14/12, f]).
step_up([2, 14/12, f], [2, 14/13, f]).
step_up([2, 14/13, f], [2, 14/14, f]).
step_up([2, 14/20, f], [2, 14/21, f]).
step_up([2, 14/21, f], [2, 14/22, f]).
step_up([2, 14/32, f], [2, 14/33, f]).
step_up([2, 14/33, f], [2, 14/34, f]).
step_up([2, 14/38, f], [2, 14/39, f]).
step_up([2, 14/39, f], [2, 14/40, f]).
step_up([2, 14/42, f], [2, 14/43, f]).
step_up([2, 14/43, f], [2, 14/44, f]).
step_up([2, 14/48, f], [2, 14/49, f]).
step_up([2, 14/49, f], [2, 14/50, f]).
step_up([2, 14/52, f], [2, 14/53, f]).
step_up([2, 14/53, f], [2, 14/54, f]).
step_up([2, 14/54, f], [2, 14/55, f]).
step_up([2, 14/55, f], [2, 14/56, f]).
step_up([2, 14/60, f], [2, 14/61, f]).
step_up([2, 14/61, f], [2, 14/62, f]).
step_up([2, 14/66, f], [2, 14/67, f]).
step_up([2, 14/67, f], [2, 14/68, f]).
step_up([2, 14/70, f], [2, 14/71, f]).
step_up([2, 14/71, f], [2, 14/72, f]).
step_up([2, 14/72, f], [2, 14/73, f]).
step_up([2, 14/73, f], [2, 14/74, f]).
step_up([2, 14/74, f], [2, 14/75, f]).
step_up([2, 14/75, f], [2, 14/76, f]).
step_up([2, 14/84, f], [2, 14/85, f]).
step_up([2, 14/85, f], [2, 14/86, f]).
step_up([2, 14/90, f], [2, 14/91, f]).
step_up([2, 14/91, f], [2, 14/92, f]).
step_up([2, 14/94, f], [2, 14/95, f]).
step_up([2, 14/95, f], [2, 14/96, f]).
step_up([2, 14/100, f], [2, 14/101, f]).
step_up([2, 14/101, f], [2, 14/102, f]).
step_up([2, 14/106, f], [2, 14/107, f]).
step_up([2, 14/107, f], [2, 14/108, f]).
step_up([2, 16/2, f], [2, 16/3, f]).
step_up([2, 16/3, f], [2, 16/4, f]).
step_up([2, 16/6, f], [2, 16/7, f]).
step_up([2, 16/7, f], [2, 16/8, f]).
step_up([2, 16/8, f], [2, 16/9, f]).
step_up([2, 16/9, f], [2, 16/10, f]).
step_up([2, 16/12, f], [2, 16/13, f]).
step_up([2, 16/13, f], [2, 16/14, f]).
step_up([2, 16/14, f], [2, 16/15, f]).
step_up([2, 16/15, f], [2, 16/16, f]).
step_up([2, 16/22, f], [2, 16/23, f]).
step_up([2, 16/23, f], [2, 16/24, f]).
step_up([2, 16/28, f], [2, 16/29, f]).
step_up([2, 16/29, f], [2, 16/30, f]).
step_up([2, 16/36, f], [2, 16/37, f]).
step_up([2, 16/37, f], [2, 16/38, f]).
step_up([2, 16/40, f], [2, 16/41, f]).
step_up([2, 16/41, f], [2, 16/42, f]).
step_up([2, 16/44, f], [2, 16/45, f]).
step_up([2, 16/45, f], [2, 16/46, f]).
step_up([2, 16/54, f], [2, 16/55, f]).
step_up([2, 16/55, f], [2, 16/56, f]).
step_up([2, 16/56, f], [2, 16/57, f]).
step_up([2, 16/57, f], [2, 16/58, f]).
step_up([2, 16/60, f], [2, 16/61, f]).
step_up([2, 16/61, f], [2, 16/62, f]).
step_up([2, 16/62, f], [2, 16/63, f]).
step_up([2, 16/63, f], [2, 16/64, f]).
step_up([2, 16/68, f], [2, 16/69, f]).
step_up([2, 16/69, f], [2, 16/70, f]).
step_up([2, 16/70, f], [2, 16/71, f]).
step_up([2, 16/71, f], [2, 16/72, f]).
step_up([2, 16/72, f], [2, 16/73, f]).
step_up([2, 16/73, f], [2, 16/74, f]).
step_up([2, 16/74, f], [2, 16/75, f]).
step_up([2, 16/75, f], [2, 16/76, f]).
step_up([2, 16/76, f], [2, 16/77, f]).
step_up([2, 16/77, f], [2, 16/78, f]).
step_up([2, 16/80, f], [2, 16/81, f]).
step_up([2, 16/81, f], [2, 16/82, f]).
step_up([2, 16/82, f], [2, 16/83, f]).
step_up([2, 16/83, f], [2, 16/84, f]).
step_up([2, 16/86, f], [2, 16/87, f]).
step_up([2, 16/87, f], [2, 16/88, f]).
step_up([2, 16/88, f], [2, 16/89, f]).
step_up([2, 16/89, f], [2, 16/90, f]).
step_up([2, 16/92, f], [2, 16/93, f]).
step_up([2, 16/93, f], [2, 16/94, f]).
step_up([2, 16/94, f], [2, 16/95, f]).
step_up([2, 16/95, f], [2, 16/96, f]).
step_up([2, 16/100, f], [2, 16/101, f]).
step_up([2, 16/101, f], [2, 16/102, f]).
step_up([2, 16/104, f], [2, 16/105, f]).
step_up([2, 16/105, f], [2, 16/106, f]).
step_up([2, 16/106, f], [2, 16/107, f]).
step_up([2, 16/107, f], [2, 16/108, f]).
step_up([2, 16/108, f], [2, 16/109, f]).
step_up([2, 16/109, f], [2, 16/110, f]).
step_up([2, 16/110, f], [2, 16/111, f]).
step_up([2, 16/111, f], [2, 16/112, f]).
step_up([2, 16/114, f], [2, 16/115, f]).
step_up([2, 16/115, f], [2, 16/116, f]).
step_up([2, 16/116, f], [2, 16/117, f]).
step_up([2, 16/117, f], [2, 16/118, f]).
step_up([2, 18/0, f], [2, 18/1, f]).
step_up([2, 18/1, f], [2, 18/2, f]).
step_up([2, 18/6, f], [2, 18/7, f]).
step_up([2, 18/7, f], [2, 18/8, f]).
step_up([2, 18/10, f], [2, 18/11, f]).
step_up([2, 18/11, f], [2, 18/12, f]).
step_up([2, 18/14, f], [2, 18/15, f]).
step_up([2, 18/15, f], [2, 18/16, f]).
step_up([2, 18/20, f], [2, 18/21, f]).
step_up([2, 18/21, f], [2, 18/22, f]).
step_up([2, 18/26, f], [2, 18/27, f]).
step_up([2, 18/27, f], [2, 18/28, f]).
step_up([2, 18/30, f], [2, 18/31, f]).
step_up([2, 18/31, f], [2, 18/32, f]).
step_up([2, 18/34, f], [2, 18/35, f]).
step_up([2, 18/35, f], [2, 18/36, f]).
step_up([2, 18/40, f], [2, 18/41, f]).
step_up([2, 18/41, f], [2, 18/42, f]).
step_up([2, 18/44, f], [2, 18/45, f]).
step_up([2, 18/45, f], [2, 18/46, f]).
step_up([2, 18/46, f], [2, 18/47, f]).
step_up([2, 18/47, f], [2, 18/48, f]).
step_up([2, 18/50, f], [2, 18/51, f]).
step_up([2, 18/51, f], [2, 18/52, f]).
step_up([2, 18/54, f], [2, 18/55, f]).
step_up([2, 18/55, f], [2, 18/56, f]).
step_up([2, 18/62, f], [2, 18/63, f]).
step_up([2, 18/63, f], [2, 18/64, f]).
step_up([2, 18/64, f], [2, 18/65, f]).
step_up([2, 18/65, f], [2, 18/66, f]).
step_up([2, 18/72, f], [2, 18/73, f]).
step_up([2, 18/73, f], [2, 18/74, f]).
step_up([2, 18/74, f], [2, 18/75, f]).
step_up([2, 18/75, f], [2, 18/76, f]).
step_up([2, 18/76, f], [2, 18/77, f]).
step_up([2, 18/77, f], [2, 18/78, f]).
step_up([2, 18/80, f], [2, 18/81, f]).
step_up([2, 18/81, f], [2, 18/82, f]).
step_up([2, 18/82, f], [2, 18/83, f]).
step_up([2, 18/83, f], [2, 18/84, f]).
step_up([2, 18/84, f], [2, 18/85, f]).
step_up([2, 18/85, f], [2, 18/86, f]).
step_up([2, 18/88, f], [2, 18/89, f]).
step_up([2, 18/89, f], [2, 18/90, f]).
step_up([2, 18/94, f], [2, 18/95, f]).
step_up([2, 18/95, f], [2, 18/96, f]).
step_up([2, 18/98, f], [2, 18/99, f]).
step_up([2, 18/99, f], [2, 18/100, f]).
step_up([2, 18/102, f], [2, 18/103, f]).
step_up([2, 18/103, f], [2, 18/104, f]).
step_up([2, 18/106, f], [2, 18/107, f]).
step_up([2, 18/107, f], [2, 18/108, f]).
step_up([2, 18/108, f], [2, 18/109, f]).
step_up([2, 18/109, f], [2, 18/110, f]).
step_up([2, 18/110, f], [2, 18/111, f]).
step_up([2, 18/111, f], [2, 18/112, f]).
step_up([2, 18/112, f], [2, 18/113, f]).
step_up([2, 18/113, f], [2, 18/114, f]).
step_up([2, 18/114, f], [2, 18/115, f]).
step_up([2, 18/115, f], [2, 18/116, f]).
step_up([2, 18/116, f], [2, 18/117, f]).
step_up([2, 18/117, f], [2, 18/118, f]).
step_up([2, 20/4, f], [2, 20/5, f]).
step_up([2, 20/5, f], [2, 20/6, f]).
step_up([2, 20/8, f], [2, 20/9, f]).
step_up([2, 20/9, f], [2, 20/10, f]).
step_up([2, 20/20, f], [2, 20/21, f]).
step_up([2, 20/21, f], [2, 20/22, f]).
step_up([2, 20/24, f], [2, 20/25, f]).
step_up([2, 20/25, f], [2, 20/26, f]).
step_up([2, 20/26, f], [2, 20/27, f]).
step_up([2, 20/27, f], [2, 20/28, f]).
step_up([2, 20/30, f], [2, 20/31, f]).
step_up([2, 20/31, f], [2, 20/32, f]).
step_up([2, 20/36, f], [2, 20/37, f]).
step_up([2, 20/37, f], [2, 20/38, f]).
step_up([2, 20/38, f], [2, 20/39, f]).
step_up([2, 20/39, f], [2, 20/40, f]).
step_up([2, 20/42, f], [2, 20/43, f]).
step_up([2, 20/43, f], [2, 20/44, f]).
step_up([2, 20/46, f], [2, 20/47, f]).
step_up([2, 20/47, f], [2, 20/48, f]).
step_up([2, 20/48, f], [2, 20/49, f]).
step_up([2, 20/49, f], [2, 20/50, f]).
step_up([2, 20/52, f], [2, 20/53, f]).
step_up([2, 20/53, f], [2, 20/54, f]).
step_up([2, 20/56, f], [2, 20/57, f]).
step_up([2, 20/57, f], [2, 20/58, f]).
step_up([2, 20/58, f], [2, 20/59, f]).
step_up([2, 20/59, f], [2, 20/60, f]).
step_up([2, 20/64, f], [2, 20/65, f]).
step_up([2, 20/65, f], [2, 20/66, f]).
step_up([2, 20/70, f], [2, 20/71, f]).
step_up([2, 20/71, f], [2, 20/72, f]).
step_up([2, 20/74, f], [2, 20/75, f]).
step_up([2, 20/75, f], [2, 20/76, f]).
step_up([2, 20/76, f], [2, 20/77, f]).
step_up([2, 20/77, f], [2, 20/78, f]).
step_up([2, 20/78, f], [2, 20/79, f]).
step_up([2, 20/79, f], [2, 20/80, f]).
step_up([2, 20/88, f], [2, 20/89, f]).
step_up([2, 20/89, f], [2, 20/90, f]).
step_up([2, 20/96, f], [2, 20/97, f]).
step_up([2, 20/97, f], [2, 20/98, f]).
step_up([2, 20/100, f], [2, 20/101, f]).
step_up([2, 20/101, f], [2, 20/102, f]).
step_up([2, 20/104, f], [2, 20/105, f]).
step_up([2, 20/105, f], [2, 20/106, f]).
step_up([2, 20/106, f], [2, 20/107, f]).
step_up([2, 20/107, f], [2, 20/108, f]).
step_up([2, 20/108, f], [2, 20/109, f]).
step_up([2, 20/109, f], [2, 20/110, f]).
step_up([2, 20/112, f], [2, 20/113, f]).
step_up([2, 20/113, f], [2, 20/114, f]).
step_up([2, 20/114, f], [2, 20/115, f]).
step_up([2, 20/115, f], [2, 20/116, f]).
step_up([2, 20/116, f], [2, 20/117, f]).
step_up([2, 20/117, f], [2, 20/118, f]).
step_up([2, 22/6, f], [2, 22/7, f]).
step_up([2, 22/7, f], [2, 22/8, f]).
step_up([2, 22/10, f], [2, 22/11, f]).
step_up([2, 22/11, f], [2, 22/12, f]).
step_up([2, 22/16, f], [2, 22/17, f]).
step_up([2, 22/17, f], [2, 22/18, f]).
step_up([2, 22/24, f], [2, 22/25, f]).
step_up([2, 22/25, f], [2, 22/26, f]).
step_up([2, 22/28, f], [2, 22/29, f]).
step_up([2, 22/29, f], [2, 22/30, f]).
step_up([2, 22/36, f], [2, 22/37, f]).
step_up([2, 22/37, f], [2, 22/38, f]).
step_up([2, 22/40, f], [2, 22/41, f]).
step_up([2, 22/41, f], [2, 22/42, f]).
step_up([2, 22/42, f], [2, 22/43, f]).
step_up([2, 22/43, f], [2, 22/44, f]).
step_up([2, 22/44, f], [2, 22/45, f]).
step_up([2, 22/45, f], [2, 22/46, f]).
step_up([2, 22/50, f], [2, 22/51, f]).
step_up([2, 22/51, f], [2, 22/52, f]).
step_up([2, 22/54, f], [2, 22/55, f]).
step_up([2, 22/55, f], [2, 22/56, f]).
step_up([2, 22/56, f], [2, 22/57, f]).
step_up([2, 22/57, f], [2, 22/58, f]).
step_up([2, 22/60, f], [2, 22/61, f]).
step_up([2, 22/61, f], [2, 22/62, f]).
step_up([2, 22/68, f], [2, 22/69, f]).
step_up([2, 22/69, f], [2, 22/70, f]).
step_up([2, 22/76, f], [2, 22/77, f]).
step_up([2, 22/77, f], [2, 22/78, f]).
step_up([2, 22/78, f], [2, 22/79, f]).
step_up([2, 22/79, f], [2, 22/80, f]).
step_up([2, 22/80, f], [2, 22/81, f]).
step_up([2, 22/81, f], [2, 22/82, f]).
step_up([2, 22/84, f], [2, 22/85, f]).
step_up([2, 22/85, f], [2, 22/86, f]).
step_up([2, 22/86, f], [2, 22/87, f]).
step_up([2, 22/87, f], [2, 22/88, f]).
step_up([2, 22/88, f], [2, 22/89, f]).
step_up([2, 22/89, f], [2, 22/90, f]).
step_up([2, 22/94, f], [2, 22/95, f]).
step_up([2, 22/95, f], [2, 22/96, f]).
step_up([2, 22/102, f], [2, 22/103, f]).
step_up([2, 22/103, f], [2, 22/104, f]).
step_up([2, 22/104, f], [2, 22/105, f]).
step_up([2, 22/105, f], [2, 22/106, f]).
step_up([2, 22/108, f], [2, 22/109, f]).
step_up([2, 22/109, f], [2, 22/110, f]).
step_up([2, 22/114, f], [2, 22/115, f]).
step_up([2, 22/115, f], [2, 22/116, f]).
step_up([2, 22/116, f], [2, 22/117, f]).
step_up([2, 22/117, f], [2, 22/118, f]).
step_up([2, 24/0, f], [2, 24/1, f]).
step_up([2, 24/1, f], [2, 24/2, f]).
step_up([2, 24/4, f], [2, 24/5, f]).
step_up([2, 24/5, f], [2, 24/6, f]).
step_up([2, 24/6, f], [2, 24/7, f]).
step_up([2, 24/7, f], [2, 24/8, f]).
step_up([2, 24/10, f], [2, 24/11, f]).
step_up([2, 24/11, f], [2, 24/12, f]).
step_up([2, 24/16, f], [2, 24/17, f]).
step_up([2, 24/17, f], [2, 24/18, f]).
step_up([2, 24/18, f], [2, 24/19, f]).
step_up([2, 24/19, f], [2, 24/20, f]).
step_up([2, 24/24, f], [2, 24/25, f]).
step_up([2, 24/25, f], [2, 24/26, f]).
step_up([2, 24/30, f], [2, 24/31, f]).
step_up([2, 24/31, f], [2, 24/32, f]).
step_up([2, 24/34, f], [2, 24/35, f]).
step_up([2, 24/35, f], [2, 24/36, f]).
step_up([2, 24/38, f], [2, 24/39, f]).
step_up([2, 24/39, f], [2, 24/40, f]).
step_up([2, 24/42, f], [2, 24/43, f]).
step_up([2, 24/43, f], [2, 24/44, f]).
step_up([2, 24/44, f], [2, 24/45, f]).
step_up([2, 24/45, f], [2, 24/46, f]).
step_up([2, 24/46, f], [2, 24/47, f]).
step_up([2, 24/47, f], [2, 24/48, f]).
step_up([2, 24/52, f], [2, 24/53, f]).
step_up([2, 24/53, f], [2, 24/54, f]).
step_up([2, 24/56, f], [2, 24/57, f]).
step_up([2, 24/57, f], [2, 24/58, f]).
step_up([2, 24/58, f], [2, 24/59, f]).
step_up([2, 24/59, f], [2, 24/60, f]).
step_up([2, 24/68, f], [2, 24/69, f]).
step_up([2, 24/69, f], [2, 24/70, f]).
step_up([2, 24/78, f], [2, 24/79, f]).
step_up([2, 24/79, f], [2, 24/80, f]).
step_up([2, 24/80, f], [2, 24/81, f]).
step_up([2, 24/81, f], [2, 24/82, f]).
step_up([2, 24/82, f], [2, 24/83, f]).
step_up([2, 24/83, f], [2, 24/84, f]).
step_up([2, 24/84, f], [2, 24/85, f]).
step_up([2, 24/85, f], [2, 24/86, f]).
step_up([2, 24/86, f], [2, 24/87, f]).
step_up([2, 24/87, f], [2, 24/88, f]).
step_up([2, 24/88, f], [2, 24/89, f]).
step_up([2, 24/89, f], [2, 24/90, f]).
step_up([2, 24/90, f], [2, 24/91, f]).
step_up([2, 24/91, f], [2, 24/92, f]).
step_up([2, 24/92, f], [2, 24/93, f]).
step_up([2, 24/93, f], [2, 24/94, f]).
step_up([2, 24/96, f], [2, 24/97, f]).
step_up([2, 24/97, f], [2, 24/98, f]).
step_up([2, 24/100, f], [2, 24/101, f]).
step_up([2, 24/101, f], [2, 24/102, f]).
step_up([2, 24/102, f], [2, 24/103, f]).
step_up([2, 24/103, f], [2, 24/104, f]).
step_up([2, 24/106, f], [2, 24/107, f]).
step_up([2, 24/107, f], [2, 24/108, f]).
step_up([2, 24/116, f], [2, 24/117, f]).
step_up([2, 24/117, f], [2, 24/118, f]).
step_up([2, 26/0, f], [2, 26/1, f]).
step_up([2, 26/1, f], [2, 26/2, f]).
step_up([2, 26/6, f], [2, 26/7, f]).
step_up([2, 26/7, f], [2, 26/8, f]).
step_up([2, 26/10, f], [2, 26/11, f]).
step_up([2, 26/11, f], [2, 26/12, f]).
step_up([2, 26/14, f], [2, 26/15, f]).
step_up([2, 26/15, f], [2, 26/16, f]).
step_up([2, 26/16, f], [2, 26/17, f]).
step_up([2, 26/17, f], [2, 26/18, f]).
step_up([2, 26/20, f], [2, 26/21, f]).
step_up([2, 26/21, f], [2, 26/22, f]).
step_up([2, 26/22, f], [2, 26/23, f]).
step_up([2, 26/23, f], [2, 26/24, f]).
step_up([2, 26/24, f], [2, 26/25, f]).
step_up([2, 26/25, f], [2, 26/26, f]).
step_up([2, 26/28, f], [2, 26/29, f]).
step_up([2, 26/29, f], [2, 26/30, f]).
step_up([2, 26/30, f], [2, 26/31, f]).
step_up([2, 26/31, f], [2, 26/32, f]).
step_up([2, 26/32, f], [2, 26/33, f]).
step_up([2, 26/33, f], [2, 26/34, f]).
step_up([2, 26/38, f], [2, 26/39, f]).
step_up([2, 26/39, f], [2, 26/40, f]).
step_up([2, 26/42, f], [2, 26/43, f]).
step_up([2, 26/43, f], [2, 26/44, f]).
step_up([2, 26/48, f], [2, 26/49, f]).
step_up([2, 26/49, f], [2, 26/50, f]).
step_up([2, 26/52, f], [2, 26/53, f]).
step_up([2, 26/53, f], [2, 26/54, f]).
step_up([2, 26/54, f], [2, 26/55, f]).
step_up([2, 26/55, f], [2, 26/56, f]).
step_up([2, 26/56, f], [2, 26/57, f]).
step_up([2, 26/57, f], [2, 26/58, f]).
step_up([2, 26/62, f], [2, 26/63, f]).
step_up([2, 26/63, f], [2, 26/64, f]).
step_up([2, 26/66, f], [2, 26/67, f]).
step_up([2, 26/67, f], [2, 26/68, f]).
step_up([2, 26/70, f], [2, 26/71, f]).
step_up([2, 26/71, f], [2, 26/72, f]).
step_up([2, 26/78, f], [2, 26/79, f]).
step_up([2, 26/79, f], [2, 26/80, f]).
step_up([2, 26/80, f], [2, 26/81, f]).
step_up([2, 26/81, f], [2, 26/82, f]).
step_up([2, 26/84, f], [2, 26/85, f]).
step_up([2, 26/85, f], [2, 26/86, f]).
step_up([2, 26/86, f], [2, 26/87, f]).
step_up([2, 26/87, f], [2, 26/88, f]).
step_up([2, 26/90, f], [2, 26/91, f]).
step_up([2, 26/91, f], [2, 26/92, f]).
step_up([2, 26/94, f], [2, 26/95, f]).
step_up([2, 26/95, f], [2, 26/96, f]).
step_up([2, 26/98, f], [2, 26/99, f]).
step_up([2, 26/99, f], [2, 26/100, f]).
step_up([2, 26/102, f], [2, 26/103, f]).
step_up([2, 26/103, f], [2, 26/104, f]).
step_up([2, 26/106, f], [2, 26/107, f]).
step_up([2, 26/107, f], [2, 26/108, f]).
step_up([2, 26/112, f], [2, 26/113, f]).
step_up([2, 26/113, f], [2, 26/114, f]).
step_up([2, 26/114, f], [2, 26/115, f]).
step_up([2, 26/115, f], [2, 26/116, f]).
step_up([2, 26/116, f], [2, 26/117, f]).
step_up([2, 26/117, f], [2, 26/118, f]).
step_up([2, 28/10, f], [2, 28/11, f]).
step_up([2, 28/11, f], [2, 28/12, f]).
step_up([2, 28/12, f], [2, 28/13, f]).
step_up([2, 28/13, f], [2, 28/14, f]).
step_up([2, 28/16, f], [2, 28/17, f]).
step_up([2, 28/17, f], [2, 28/18, f]).
step_up([2, 28/22, f], [2, 28/23, f]).
step_up([2, 28/23, f], [2, 28/24, f]).
step_up([2, 28/24, f], [2, 28/25, f]).
step_up([2, 28/25, f], [2, 28/26, f]).
step_up([2, 28/26, f], [2, 28/27, f]).
step_up([2, 28/27, f], [2, 28/28, f]).
step_up([2, 28/30, f], [2, 28/31, f]).
step_up([2, 28/31, f], [2, 28/32, f]).
step_up([2, 28/38, f], [2, 28/39, f]).
step_up([2, 28/39, f], [2, 28/40, f]).
step_up([2, 28/42, f], [2, 28/43, f]).
step_up([2, 28/43, f], [2, 28/44, f]).
step_up([2, 28/46, f], [2, 28/47, f]).
step_up([2, 28/47, f], [2, 28/48, f]).
step_up([2, 28/48, f], [2, 28/49, f]).
step_up([2, 28/49, f], [2, 28/50, f]).
step_up([2, 28/52, f], [2, 28/53, f]).
step_up([2, 28/53, f], [2, 28/54, f]).
step_up([2, 28/60, f], [2, 28/61, f]).
step_up([2, 28/61, f], [2, 28/62, f]).
step_up([2, 28/62, f], [2, 28/63, f]).
step_up([2, 28/63, f], [2, 28/64, f]).
step_up([2, 28/66, f], [2, 28/67, f]).
step_up([2, 28/67, f], [2, 28/68, f]).
step_up([2, 28/68, f], [2, 28/69, f]).
step_up([2, 28/69, f], [2, 28/70, f]).
step_up([2, 28/76, f], [2, 28/77, f]).
step_up([2, 28/77, f], [2, 28/78, f]).
step_up([2, 28/80, f], [2, 28/81, f]).
step_up([2, 28/81, f], [2, 28/82, f]).
step_up([2, 28/82, f], [2, 28/83, f]).
step_up([2, 28/83, f], [2, 28/84, f]).
step_up([2, 28/84, f], [2, 28/85, f]).
step_up([2, 28/85, f], [2, 28/86, f]).
step_up([2, 28/88, f], [2, 28/89, f]).
step_up([2, 28/89, f], [2, 28/90, f]).
step_up([2, 28/90, f], [2, 28/91, f]).
step_up([2, 28/91, f], [2, 28/92, f]).
step_up([2, 28/92, f], [2, 28/93, f]).
step_up([2, 28/93, f], [2, 28/94, f]).
step_up([2, 28/96, f], [2, 28/97, f]).
step_up([2, 28/97, f], [2, 28/98, f]).
step_up([2, 28/100, f], [2, 28/101, f]).
step_up([2, 28/101, f], [2, 28/102, f]).
step_up([2, 28/104, f], [2, 28/105, f]).
step_up([2, 28/105, f], [2, 28/106, f]).
step_up([2, 28/106, f], [2, 28/107, f]).
step_up([2, 28/107, f], [2, 28/108, f]).
step_up([2, 28/110, f], [2, 28/111, f]).
step_up([2, 28/111, f], [2, 28/112, f]).
step_up([2, 28/112, f], [2, 28/113, f]).
step_up([2, 28/113, f], [2, 28/114, f]).
step_up([2, 28/116, f], [2, 28/117, f]).
step_up([2, 28/117, f], [2, 28/118, f]).
step_up([2, 30/2, f], [2, 30/3, f]).
step_up([2, 30/3, f], [2, 30/4, f]).
step_up([2, 30/4, f], [2, 30/5, f]).
step_up([2, 30/5, f], [2, 30/6, f]).
step_up([2, 30/10, f], [2, 30/11, f]).
step_up([2, 30/11, f], [2, 30/12, f]).
step_up([2, 30/14, f], [2, 30/15, f]).
step_up([2, 30/15, f], [2, 30/16, f]).
step_up([2, 30/18, f], [2, 30/19, f]).
step_up([2, 30/19, f], [2, 30/20, f]).
step_up([2, 30/22, f], [2, 30/23, f]).
step_up([2, 30/23, f], [2, 30/24, f]).
step_up([2, 30/26, f], [2, 30/27, f]).
step_up([2, 30/27, f], [2, 30/28, f]).
step_up([2, 30/30, f], [2, 30/31, f]).
step_up([2, 30/31, f], [2, 30/32, f]).
step_up([2, 30/40, f], [2, 30/41, f]).
step_up([2, 30/41, f], [2, 30/42, f]).
step_up([2, 30/42, f], [2, 30/43, f]).
step_up([2, 30/43, f], [2, 30/44, f]).
step_up([2, 30/44, f], [2, 30/45, f]).
step_up([2, 30/45, f], [2, 30/46, f]).
step_up([2, 30/46, f], [2, 30/47, f]).
step_up([2, 30/47, f], [2, 30/48, f]).
step_up([2, 30/50, f], [2, 30/51, f]).
step_up([2, 30/51, f], [2, 30/52, f]).
step_up([2, 30/54, f], [2, 30/55, f]).
step_up([2, 30/55, f], [2, 30/56, f]).
step_up([2, 30/60, f], [2, 30/61, f]).
step_up([2, 30/61, f], [2, 30/62, f]).
step_up([2, 30/74, f], [2, 30/75, f]).
step_up([2, 30/75, f], [2, 30/76, f]).
step_up([2, 30/78, f], [2, 30/79, f]).
step_up([2, 30/79, f], [2, 30/80, f]).
step_up([2, 30/84, f], [2, 30/85, f]).
step_up([2, 30/85, f], [2, 30/86, f]).
step_up([2, 30/88, f], [2, 30/89, f]).
step_up([2, 30/89, f], [2, 30/90, f]).
step_up([2, 30/92, f], [2, 30/93, f]).
step_up([2, 30/93, f], [2, 30/94, f]).
step_up([2, 30/96, f], [2, 30/97, f]).
step_up([2, 30/97, f], [2, 30/98, f]).
step_up([2, 30/98, f], [2, 30/99, f]).
step_up([2, 30/99, f], [2, 30/100, f]).
step_up([2, 30/100, f], [2, 30/101, f]).
step_up([2, 30/101, f], [2, 30/102, f]).
step_up([2, 30/104, f], [2, 30/105, f]).
step_up([2, 30/105, f], [2, 30/106, f]).
step_up([2, 30/106, f], [2, 30/107, f]).
step_up([2, 30/107, f], [2, 30/108, f]).
step_up([2, 30/108, f], [2, 30/109, f]).
step_up([2, 30/109, f], [2, 30/110, f]).
step_up([2, 30/112, f], [2, 30/113, f]).
step_up([2, 30/113, f], [2, 30/114, f]).
step_up([2, 30/114, f], [2, 30/115, f]).
step_up([2, 30/115, f], [2, 30/116, f]).
step_up([2, 32/0, f], [2, 32/1, f]).
step_up([2, 32/1, f], [2, 32/2, f]).
step_up([2, 32/2, f], [2, 32/3, f]).
step_up([2, 32/3, f], [2, 32/4, f]).
step_up([2, 32/4, f], [2, 32/5, f]).
step_up([2, 32/5, f], [2, 32/6, f]).
step_up([2, 32/12, f], [2, 32/13, f]).
step_up([2, 32/13, f], [2, 32/14, f]).
step_up([2, 32/14, f], [2, 32/15, f]).
step_up([2, 32/15, f], [2, 32/16, f]).
step_up([2, 32/20, f], [2, 32/21, f]).
step_up([2, 32/21, f], [2, 32/22, f]).
step_up([2, 32/24, f], [2, 32/25, f]).
step_up([2, 32/25, f], [2, 32/26, f]).
step_up([2, 32/38, f], [2, 32/39, f]).
step_up([2, 32/39, f], [2, 32/40, f]).
step_up([2, 32/40, f], [2, 32/41, f]).
step_up([2, 32/41, f], [2, 32/42, f]).
step_up([2, 32/42, f], [2, 32/43, f]).
step_up([2, 32/43, f], [2, 32/44, f]).
step_up([2, 32/50, f], [2, 32/51, f]).
step_up([2, 32/51, f], [2, 32/52, f]).
step_up([2, 32/52, f], [2, 32/53, f]).
step_up([2, 32/53, f], [2, 32/54, f]).
step_up([2, 32/56, f], [2, 32/57, f]).
step_up([2, 32/57, f], [2, 32/58, f]).
step_up([2, 32/66, f], [2, 32/67, f]).
step_up([2, 32/67, f], [2, 32/68, f]).
step_up([2, 32/72, f], [2, 32/73, f]).
step_up([2, 32/73, f], [2, 32/74, f]).
step_up([2, 32/76, f], [2, 32/77, f]).
step_up([2, 32/77, f], [2, 32/78, f]).
step_up([2, 32/86, f], [2, 32/87, f]).
step_up([2, 32/87, f], [2, 32/88, f]).
step_up([2, 32/88, f], [2, 32/89, f]).
step_up([2, 32/89, f], [2, 32/90, f]).
step_up([2, 32/90, f], [2, 32/91, f]).
step_up([2, 32/91, f], [2, 32/92, f]).
step_up([2, 32/94, f], [2, 32/95, f]).
step_up([2, 32/95, f], [2, 32/96, f]).
step_up([2, 32/98, f], [2, 32/99, f]).
step_up([2, 32/99, f], [2, 32/100, f]).
step_up([2, 32/104, f], [2, 32/105, f]).
step_up([2, 32/105, f], [2, 32/106, f]).
step_up([2, 32/106, f], [2, 32/107, f]).
step_up([2, 32/107, f], [2, 32/108, f]).
step_up([2, 32/110, f], [2, 32/111, f]).
step_up([2, 32/111, f], [2, 32/112, f]).
step_up([2, 32/112, f], [2, 32/113, f]).
step_up([2, 32/113, f], [2, 32/114, f]).
step_up([2, 32/114, f], [2, 32/115, f]).
step_up([2, 32/115, f], [2, 32/116, f]).
step_up([2, 34/0, f], [2, 34/1, f]).
step_up([2, 34/1, f], [2, 34/2, f]).
step_up([2, 34/6, f], [2, 34/7, f]).
step_up([2, 34/7, f], [2, 34/8, f]).
step_up([2, 34/10, f], [2, 34/11, f]).
step_up([2, 34/11, f], [2, 34/12, f]).
step_up([2, 34/14, f], [2, 34/15, f]).
step_up([2, 34/15, f], [2, 34/16, f]).
step_up([2, 34/16, f], [2, 34/17, f]).
step_up([2, 34/17, f], [2, 34/18, f]).
step_up([2, 34/28, f], [2, 34/29, f]).
step_up([2, 34/29, f], [2, 34/30, f]).
step_up([2, 34/38, f], [2, 34/39, f]).
step_up([2, 34/39, f], [2, 34/40, f]).
step_up([2, 34/40, f], [2, 34/41, f]).
step_up([2, 34/41, f], [2, 34/42, f]).
step_up([2, 34/42, f], [2, 34/43, f]).
step_up([2, 34/43, f], [2, 34/44, f]).
step_up([2, 34/46, f], [2, 34/47, f]).
step_up([2, 34/47, f], [2, 34/48, f]).
step_up([2, 34/50, f], [2, 34/51, f]).
step_up([2, 34/51, f], [2, 34/52, f]).
step_up([2, 34/54, f], [2, 34/55, f]).
step_up([2, 34/55, f], [2, 34/56, f]).
step_up([2, 34/66, f], [2, 34/67, f]).
step_up([2, 34/67, f], [2, 34/68, f]).
step_up([2, 34/72, f], [2, 34/73, f]).
step_up([2, 34/73, f], [2, 34/74, f]).
step_up([2, 34/80, f], [2, 34/81, f]).
step_up([2, 34/81, f], [2, 34/82, f]).
step_up([2, 34/86, f], [2, 34/87, f]).
step_up([2, 34/87, f], [2, 34/88, f]).
step_up([2, 34/88, f], [2, 34/89, f]).
step_up([2, 34/89, f], [2, 34/90, f]).
step_up([2, 34/90, f], [2, 34/91, f]).
step_up([2, 34/91, f], [2, 34/92, f]).
step_up([2, 34/92, f], [2, 34/93, f]).
step_up([2, 34/93, f], [2, 34/94, f]).
step_up([2, 34/96, f], [2, 34/97, f]).
step_up([2, 34/97, f], [2, 34/98, f]).
step_up([2, 34/104, f], [2, 34/105, f]).
step_up([2, 34/105, f], [2, 34/106, f]).
step_up([2, 34/106, f], [2, 34/107, f]).
step_up([2, 34/107, f], [2, 34/108, f]).
step_up([2, 34/108, f], [2, 34/109, f]).
step_up([2, 34/109, f], [2, 34/110, f]).
step_up([2, 34/112, f], [2, 34/113, f]).
step_up([2, 34/113, f], [2, 34/114, f]).
step_up([2, 36/4, f], [2, 36/5, f]).
step_up([2, 36/5, f], [2, 36/6, f]).
step_up([2, 36/8, f], [2, 36/9, f]).
step_up([2, 36/9, f], [2, 36/10, f]).
step_up([2, 36/12, f], [2, 36/13, f]).
step_up([2, 36/13, f], [2, 36/14, f]).
step_up([2, 36/16, f], [2, 36/17, f]).
step_up([2, 36/17, f], [2, 36/18, f]).
step_up([2, 36/18, f], [2, 36/19, f]).
step_up([2, 36/19, f], [2, 36/20, f]).
step_up([2, 36/22, f], [2, 36/23, f]).
step_up([2, 36/23, f], [2, 36/24, f]).
step_up([2, 36/30, f], [2, 36/31, f]).
step_up([2, 36/31, f], [2, 36/32, f]).
step_up([2, 36/36, f], [2, 36/37, f]).
step_up([2, 36/37, f], [2, 36/38, f]).
step_up([2, 36/40, f], [2, 36/41, f]).
step_up([2, 36/41, f], [2, 36/42, f]).
step_up([2, 36/42, f], [2, 36/43, f]).
step_up([2, 36/43, f], [2, 36/44, f]).
step_up([2, 36/44, f], [2, 36/45, f]).
step_up([2, 36/45, f], [2, 36/46, f]).
step_up([2, 36/46, f], [2, 36/47, f]).
step_up([2, 36/47, f], [2, 36/48, f]).
step_up([2, 36/48, f], [2, 36/49, f]).
step_up([2, 36/49, f], [2, 36/50, f]).
step_up([2, 36/58, f], [2, 36/59, f]).
step_up([2, 36/59, f], [2, 36/60, f]).
step_up([2, 36/62, f], [2, 36/63, f]).
step_up([2, 36/63, f], [2, 36/64, f]).
step_up([2, 36/64, f], [2, 36/65, f]).
step_up([2, 36/65, f], [2, 36/66, f]).
step_up([2, 36/66, f], [2, 36/67, f]).
step_up([2, 36/67, f], [2, 36/68, f]).
step_up([2, 36/68, f], [2, 36/69, f]).
step_up([2, 36/69, f], [2, 36/70, f]).
step_up([2, 36/74, f], [2, 36/75, f]).
step_up([2, 36/75, f], [2, 36/76, f]).
step_up([2, 36/78, f], [2, 36/79, f]).
step_up([2, 36/79, f], [2, 36/80, f]).
step_up([2, 36/84, f], [2, 36/85, f]).
step_up([2, 36/85, f], [2, 36/86, f]).
step_up([2, 36/92, f], [2, 36/93, f]).
step_up([2, 36/93, f], [2, 36/94, f]).
step_up([2, 36/100, f], [2, 36/101, f]).
step_up([2, 36/101, f], [2, 36/102, f]).
step_up([2, 36/106, f], [2, 36/107, f]).
step_up([2, 36/107, f], [2, 36/108, f]).
step_up([2, 36/112, f], [2, 36/113, f]).
step_up([2, 36/113, f], [2, 36/114, f]).
step_up([2, 36/116, f], [2, 36/117, f]).
step_up([2, 36/117, f], [2, 36/118, f]).
step_up([2, 38/4, f], [2, 38/5, f]).
step_up([2, 38/5, f], [2, 38/6, f]).
step_up([2, 38/10, f], [2, 38/11, f]).
step_up([2, 38/11, f], [2, 38/12, f]).
step_up([2, 38/14, f], [2, 38/15, f]).
step_up([2, 38/15, f], [2, 38/16, f]).
step_up([2, 38/20, f], [2, 38/21, f]).
step_up([2, 38/21, f], [2, 38/22, f]).
step_up([2, 38/24, f], [2, 38/25, f]).
step_up([2, 38/25, f], [2, 38/26, f]).
step_up([2, 38/26, f], [2, 38/27, f]).
step_up([2, 38/27, f], [2, 38/28, f]).
step_up([2, 38/38, f], [2, 38/39, f]).
step_up([2, 38/39, f], [2, 38/40, f]).
step_up([2, 38/40, f], [2, 38/41, f]).
step_up([2, 38/41, f], [2, 38/42, f]).
step_up([2, 38/42, f], [2, 38/43, f]).
step_up([2, 38/43, f], [2, 38/44, f]).
step_up([2, 38/46, f], [2, 38/47, f]).
step_up([2, 38/47, f], [2, 38/48, f]).
step_up([2, 38/48, f], [2, 38/49, f]).
step_up([2, 38/49, f], [2, 38/50, f]).
step_up([2, 38/52, f], [2, 38/53, f]).
step_up([2, 38/53, f], [2, 38/54, f]).
step_up([2, 38/58, f], [2, 38/59, f]).
step_up([2, 38/59, f], [2, 38/60, f]).
step_up([2, 38/60, f], [2, 38/61, f]).
step_up([2, 38/61, f], [2, 38/62, f]).
step_up([2, 38/62, f], [2, 38/63, f]).
step_up([2, 38/63, f], [2, 38/64, f]).
step_up([2, 38/64, f], [2, 38/65, f]).
step_up([2, 38/65, f], [2, 38/66, f]).
step_up([2, 38/66, f], [2, 38/67, f]).
step_up([2, 38/67, f], [2, 38/68, f]).
step_up([2, 38/68, f], [2, 38/69, f]).
step_up([2, 38/69, f], [2, 38/70, f]).
step_up([2, 38/76, f], [2, 38/77, f]).
step_up([2, 38/77, f], [2, 38/78, f]).
step_up([2, 38/78, f], [2, 38/79, f]).
step_up([2, 38/79, f], [2, 38/80, f]).
step_up([2, 38/84, f], [2, 38/85, f]).
step_up([2, 38/85, f], [2, 38/86, f]).
step_up([2, 38/90, f], [2, 38/91, f]).
step_up([2, 38/91, f], [2, 38/92, f]).
step_up([2, 38/94, f], [2, 38/95, f]).
step_up([2, 38/95, f], [2, 38/96, f]).
step_up([2, 38/96, f], [2, 38/97, f]).
step_up([2, 38/97, f], [2, 38/98, f]).
step_up([2, 38/100, f], [2, 38/101, f]).
step_up([2, 38/101, f], [2, 38/102, f]).
step_up([2, 38/104, f], [2, 38/105, f]).
step_up([2, 38/105, f], [2, 38/106, f]).
step_up([2, 38/112, f], [2, 38/113, f]).
step_up([2, 38/113, f], [2, 38/114, f]).
step_up([2, 40/4, f], [2, 40/5, f]).
step_up([2, 40/5, f], [2, 40/6, f]).
step_up([2, 40/16, f], [2, 40/17, f]).
step_up([2, 40/17, f], [2, 40/18, f]).
step_up([2, 40/22, f], [2, 40/23, f]).
step_up([2, 40/23, f], [2, 40/24, f]).
step_up([2, 40/24, f], [2, 40/25, f]).
step_up([2, 40/25, f], [2, 40/26, f]).
step_up([2, 40/28, f], [2, 40/29, f]).
step_up([2, 40/29, f], [2, 40/30, f]).
step_up([2, 40/32, f], [2, 40/33, f]).
step_up([2, 40/33, f], [2, 40/34, f]).
step_up([2, 40/38, f], [2, 40/39, f]).
step_up([2, 40/39, f], [2, 40/40, f]).
step_up([2, 40/40, f], [2, 40/41, f]).
step_up([2, 40/41, f], [2, 40/42, f]).
step_up([2, 40/44, f], [2, 40/45, f]).
step_up([2, 40/45, f], [2, 40/46, f]).
step_up([2, 40/46, f], [2, 40/47, f]).
step_up([2, 40/47, f], [2, 40/48, f]).
step_up([2, 40/50, f], [2, 40/51, f]).
step_up([2, 40/51, f], [2, 40/52, f]).
step_up([2, 40/56, f], [2, 40/57, f]).
step_up([2, 40/57, f], [2, 40/58, f]).
step_up([2, 40/60, f], [2, 40/61, f]).
step_up([2, 40/61, f], [2, 40/62, f]).
step_up([2, 40/62, f], [2, 40/63, f]).
step_up([2, 40/63, f], [2, 40/64, f]).
step_up([2, 40/64, f], [2, 40/65, f]).
step_up([2, 40/65, f], [2, 40/66, f]).
step_up([2, 40/66, f], [2, 40/67, f]).
step_up([2, 40/67, f], [2, 40/68, f]).
step_up([2, 40/68, f], [2, 40/69, f]).
step_up([2, 40/69, f], [2, 40/70, f]).
step_up([2, 40/72, f], [2, 40/73, f]).
step_up([2, 40/73, f], [2, 40/74, f]).
step_up([2, 40/78, f], [2, 40/79, f]).
step_up([2, 40/79, f], [2, 40/80, f]).
step_up([2, 40/84, f], [2, 40/85, f]).
step_up([2, 40/85, f], [2, 40/86, f]).
step_up([2, 40/88, f], [2, 40/89, f]).
step_up([2, 40/89, f], [2, 40/90, f]).
step_up([2, 40/92, f], [2, 40/93, f]).
step_up([2, 40/93, f], [2, 40/94, f]).
step_up([2, 40/94, f], [2, 40/95, f]).
step_up([2, 40/95, f], [2, 40/96, f]).
step_up([2, 40/98, f], [2, 40/99, f]).
step_up([2, 40/99, f], [2, 40/100, f]).
step_up([2, 40/100, f], [2, 40/101, f]).
step_up([2, 40/101, f], [2, 40/102, f]).
step_up([2, 40/104, f], [2, 40/105, f]).
step_up([2, 40/105, f], [2, 40/106, f]).
step_up([2, 40/108, f], [2, 40/109, f]).
step_up([2, 40/109, f], [2, 40/110, f]).
step_up([2, 40/112, f], [2, 40/113, f]).
step_up([2, 40/113, f], [2, 40/114, f]).
step_up([2, 40/116, f], [2, 40/117, f]).
step_up([2, 40/117, f], [2, 40/118, f]).
step_up([2, 42/0, f], [2, 42/1, f]).
step_up([2, 42/1, f], [2, 42/2, f]).
step_up([2, 42/18, f], [2, 42/19, f]).
step_up([2, 42/19, f], [2, 42/20, f]).
step_up([2, 42/20, f], [2, 42/21, f]).
step_up([2, 42/21, f], [2, 42/22, f]).
step_up([2, 42/22, f], [2, 42/23, f]).
step_up([2, 42/23, f], [2, 42/24, f]).
step_up([2, 42/26, f], [2, 42/27, f]).
step_up([2, 42/27, f], [2, 42/28, f]).
step_up([2, 42/30, f], [2, 42/31, f]).
step_up([2, 42/31, f], [2, 42/32, f]).
step_up([2, 42/34, f], [2, 42/35, f]).
step_up([2, 42/35, f], [2, 42/36, f]).
step_up([2, 42/40, f], [2, 42/41, f]).
step_up([2, 42/41, f], [2, 42/42, f]).
step_up([2, 42/42, f], [2, 42/43, f]).
step_up([2, 42/43, f], [2, 42/44, f]).
step_up([2, 42/44, f], [2, 42/45, f]).
step_up([2, 42/45, f], [2, 42/46, f]).
step_up([2, 42/48, f], [2, 42/49, f]).
step_up([2, 42/49, f], [2, 42/50, f]).
step_up([2, 42/56, f], [2, 42/57, f]).
step_up([2, 42/57, f], [2, 42/58, f]).
step_up([2, 42/62, f], [2, 42/63, f]).
step_up([2, 42/63, f], [2, 42/64, f]).
step_up([2, 42/64, f], [2, 42/65, f]).
step_up([2, 42/65, f], [2, 42/66, f]).
step_up([2, 42/66, f], [2, 42/67, f]).
step_up([2, 42/67, f], [2, 42/68, f]).
step_up([2, 42/68, f], [2, 42/69, f]).
step_up([2, 42/69, f], [2, 42/70, f]).
step_up([2, 42/74, f], [2, 42/75, f]).
step_up([2, 42/75, f], [2, 42/76, f]).
step_up([2, 42/80, f], [2, 42/81, f]).
step_up([2, 42/81, f], [2, 42/82, f]).
step_up([2, 42/82, f], [2, 42/83, f]).
step_up([2, 42/83, f], [2, 42/84, f]).
step_up([2, 42/88, f], [2, 42/89, f]).
step_up([2, 42/89, f], [2, 42/90, f]).
step_up([2, 42/94, f], [2, 42/95, f]).
step_up([2, 42/95, f], [2, 42/96, f]).
step_up([2, 42/100, f], [2, 42/101, f]).
step_up([2, 42/101, f], [2, 42/102, f]).
step_up([2, 42/102, f], [2, 42/103, f]).
step_up([2, 42/103, f], [2, 42/104, f]).
step_up([2, 42/106, f], [2, 42/107, f]).
step_up([2, 42/107, f], [2, 42/108, f]).
step_up([2, 42/114, f], [2, 42/115, f]).
step_up([2, 42/115, f], [2, 42/116, f]).
step_up([2, 42/116, f], [2, 42/117, f]).
step_up([2, 42/117, f], [2, 42/118, f]).
step_up([2, 44/0, f], [2, 44/1, f]).
step_up([2, 44/1, f], [2, 44/2, f]).
step_up([2, 44/2, f], [2, 44/3, f]).
step_up([2, 44/3, f], [2, 44/4, f]).
step_up([2, 44/6, f], [2, 44/7, f]).
step_up([2, 44/7, f], [2, 44/8, f]).
step_up([2, 44/20, f], [2, 44/21, f]).
step_up([2, 44/21, f], [2, 44/22, f]).
step_up([2, 44/22, f], [2, 44/23, f]).
step_up([2, 44/23, f], [2, 44/24, f]).
step_up([2, 44/24, f], [2, 44/25, f]).
step_up([2, 44/25, f], [2, 44/26, f]).
step_up([2, 44/28, f], [2, 44/29, f]).
step_up([2, 44/29, f], [2, 44/30, f]).
step_up([2, 44/32, f], [2, 44/33, f]).
step_up([2, 44/33, f], [2, 44/34, f]).
step_up([2, 44/38, f], [2, 44/39, f]).
step_up([2, 44/39, f], [2, 44/40, f]).
step_up([2, 44/40, f], [2, 44/41, f]).
step_up([2, 44/41, f], [2, 44/42, f]).
step_up([2, 44/42, f], [2, 44/43, f]).
step_up([2, 44/43, f], [2, 44/44, f]).
step_up([2, 44/46, f], [2, 44/47, f]).
step_up([2, 44/47, f], [2, 44/48, f]).
step_up([2, 44/52, f], [2, 44/53, f]).
step_up([2, 44/53, f], [2, 44/54, f]).
step_up([2, 44/58, f], [2, 44/59, f]).
step_up([2, 44/59, f], [2, 44/60, f]).
step_up([2, 44/64, f], [2, 44/65, f]).
step_up([2, 44/65, f], [2, 44/66, f]).
step_up([2, 44/66, f], [2, 44/67, f]).
step_up([2, 44/67, f], [2, 44/68, f]).
step_up([2, 44/70, f], [2, 44/71, f]).
step_up([2, 44/71, f], [2, 44/72, f]).
step_up([2, 44/76, f], [2, 44/77, f]).
step_up([2, 44/77, f], [2, 44/78, f]).
step_up([2, 44/78, f], [2, 44/79, f]).
step_up([2, 44/79, f], [2, 44/80, f]).
step_up([2, 44/82, f], [2, 44/83, f]).
step_up([2, 44/83, f], [2, 44/84, f]).
step_up([2, 44/84, f], [2, 44/85, f]).
step_up([2, 44/85, f], [2, 44/86, f]).
step_up([2, 44/90, f], [2, 44/91, f]).
step_up([2, 44/91, f], [2, 44/92, f]).
step_up([2, 44/98, f], [2, 44/99, f]).
step_up([2, 44/99, f], [2, 44/100, f]).
step_up([2, 44/104, f], [2, 44/105, f]).
step_up([2, 44/105, f], [2, 44/106, f]).
step_up([2, 44/108, f], [2, 44/109, f]).
step_up([2, 44/109, f], [2, 44/110, f]).
step_up([2, 44/112, f], [2, 44/113, f]).
step_up([2, 44/113, f], [2, 44/114, f]).
step_up([2, 44/114, f], [2, 44/115, f]).
step_up([2, 44/115, f], [2, 44/116, f]).
step_up([2, 46/0, f], [2, 46/1, f]).
step_up([2, 46/1, f], [2, 46/2, f]).
step_up([2, 46/6, f], [2, 46/7, f]).
step_up([2, 46/7, f], [2, 46/8, f]).
step_up([2, 46/8, f], [2, 46/9, f]).
step_up([2, 46/9, f], [2, 46/10, f]).
step_up([2, 46/12, f], [2, 46/13, f]).
step_up([2, 46/13, f], [2, 46/14, f]).
step_up([2, 46/16, f], [2, 46/17, f]).
step_up([2, 46/17, f], [2, 46/18, f]).
step_up([2, 46/20, f], [2, 46/21, f]).
step_up([2, 46/21, f], [2, 46/22, f]).
step_up([2, 46/22, f], [2, 46/23, f]).
step_up([2, 46/23, f], [2, 46/24, f]).
step_up([2, 46/24, f], [2, 46/25, f]).
step_up([2, 46/25, f], [2, 46/26, f]).
step_up([2, 46/26, f], [2, 46/27, f]).
step_up([2, 46/27, f], [2, 46/28, f]).
step_up([2, 46/34, f], [2, 46/35, f]).
step_up([2, 46/35, f], [2, 46/36, f]).
step_up([2, 46/36, f], [2, 46/37, f]).
step_up([2, 46/37, f], [2, 46/38, f]).
step_up([2, 46/38, f], [2, 46/39, f]).
step_up([2, 46/39, f], [2, 46/40, f]).
step_up([2, 46/40, f], [2, 46/41, f]).
step_up([2, 46/41, f], [2, 46/42, f]).
step_up([2, 46/42, f], [2, 46/43, f]).
step_up([2, 46/43, f], [2, 46/44, f]).
step_up([2, 46/52, f], [2, 46/53, f]).
step_up([2, 46/53, f], [2, 46/54, f]).
step_up([2, 46/60, f], [2, 46/61, f]).
step_up([2, 46/61, f], [2, 46/62, f]).
step_up([2, 46/66, f], [2, 46/67, f]).
step_up([2, 46/67, f], [2, 46/68, f]).
step_up([2, 46/76, f], [2, 46/77, f]).
step_up([2, 46/77, f], [2, 46/78, f]).
step_up([2, 46/80, f], [2, 46/81, f]).
step_up([2, 46/81, f], [2, 46/82, f]).
step_up([2, 46/84, f], [2, 46/85, f]).
step_up([2, 46/85, f], [2, 46/86, f]).
step_up([2, 46/88, f], [2, 46/89, f]).
step_up([2, 46/89, f], [2, 46/90, f]).
step_up([2, 46/98, f], [2, 46/99, f]).
step_up([2, 46/99, f], [2, 46/100, f]).
step_up([2, 46/102, f], [2, 46/103, f]).
step_up([2, 46/103, f], [2, 46/104, f]).
step_up([2, 46/106, f], [2, 46/107, f]).
step_up([2, 46/107, f], [2, 46/108, f]).
step_up([2, 46/110, f], [2, 46/111, f]).
step_up([2, 46/111, f], [2, 46/112, f]).
step_up([2, 46/112, f], [2, 46/113, f]).
step_up([2, 46/113, f], [2, 46/114, f]).
step_up([2, 46/116, f], [2, 46/117, f]).
step_up([2, 46/117, f], [2, 46/118, f]).
step_up([2, 48/0, f], [2, 48/1, f]).
step_up([2, 48/1, f], [2, 48/2, f]).
step_up([2, 48/4, f], [2, 48/5, f]).
step_up([2, 48/5, f], [2, 48/6, f]).
step_up([2, 48/6, f], [2, 48/7, f]).
step_up([2, 48/7, f], [2, 48/8, f]).
step_up([2, 48/8, f], [2, 48/9, f]).
step_up([2, 48/9, f], [2, 48/10, f]).
step_up([2, 48/14, f], [2, 48/15, f]).
step_up([2, 48/15, f], [2, 48/16, f]).
step_up([2, 48/18, f], [2, 48/19, f]).
step_up([2, 48/19, f], [2, 48/20, f]).
step_up([2, 48/20, f], [2, 48/21, f]).
step_up([2, 48/21, f], [2, 48/22, f]).
step_up([2, 48/22, f], [2, 48/23, f]).
step_up([2, 48/23, f], [2, 48/24, f]).
step_up([2, 48/26, f], [2, 48/27, f]).
step_up([2, 48/27, f], [2, 48/28, f]).
step_up([2, 48/30, f], [2, 48/31, f]).
step_up([2, 48/31, f], [2, 48/32, f]).
step_up([2, 48/32, f], [2, 48/33, f]).
step_up([2, 48/33, f], [2, 48/34, f]).
step_up([2, 48/34, f], [2, 48/35, f]).
step_up([2, 48/35, f], [2, 48/36, f]).
step_up([2, 48/36, f], [2, 48/37, f]).
step_up([2, 48/37, f], [2, 48/38, f]).
step_up([2, 48/40, f], [2, 48/41, f]).
step_up([2, 48/41, f], [2, 48/42, f]).
step_up([2, 48/44, f], [2, 48/45, f]).
step_up([2, 48/45, f], [2, 48/46, f]).
step_up([2, 48/52, f], [2, 48/53, f]).
step_up([2, 48/53, f], [2, 48/54, f]).
step_up([2, 48/56, f], [2, 48/57, f]).
step_up([2, 48/57, f], [2, 48/58, f]).
step_up([2, 48/62, f], [2, 48/63, f]).
step_up([2, 48/63, f], [2, 48/64, f]).
step_up([2, 48/66, f], [2, 48/67, f]).
step_up([2, 48/67, f], [2, 48/68, f]).
step_up([2, 48/70, f], [2, 48/71, f]).
step_up([2, 48/71, f], [2, 48/72, f]).
step_up([2, 48/74, f], [2, 48/75, f]).
step_up([2, 48/75, f], [2, 48/76, f]).
step_up([2, 48/78, f], [2, 48/79, f]).
step_up([2, 48/79, f], [2, 48/80, f]).
step_up([2, 48/82, f], [2, 48/83, f]).
step_up([2, 48/83, f], [2, 48/84, f]).
step_up([2, 48/86, f], [2, 48/87, f]).
step_up([2, 48/87, f], [2, 48/88, f]).
step_up([2, 48/90, f], [2, 48/91, f]).
step_up([2, 48/91, f], [2, 48/92, f]).
step_up([2, 48/94, f], [2, 48/95, f]).
step_up([2, 48/95, f], [2, 48/96, f]).
step_up([2, 48/100, f], [2, 48/101, f]).
step_up([2, 48/101, f], [2, 48/102, f]).
step_up([2, 48/102, f], [2, 48/103, f]).
step_up([2, 48/103, f], [2, 48/104, f]).
step_up([2, 48/104, f], [2, 48/105, f]).
step_up([2, 48/105, f], [2, 48/106, f]).
step_up([2, 48/108, f], [2, 48/109, f]).
step_up([2, 48/109, f], [2, 48/110, f]).
step_up([2, 48/110, f], [2, 48/111, f]).
step_up([2, 48/111, f], [2, 48/112, f]).
step_up([2, 48/114, f], [2, 48/115, f]).
step_up([2, 48/115, f], [2, 48/116, f]).
step_up([2, 50/0, f], [2, 50/1, f]).
step_up([2, 50/1, f], [2, 50/2, f]).
step_up([2, 50/2, f], [2, 50/3, f]).
step_up([2, 50/3, f], [2, 50/4, f]).
step_up([2, 50/4, f], [2, 50/5, f]).
step_up([2, 50/5, f], [2, 50/6, f]).
step_up([2, 50/8, f], [2, 50/9, f]).
step_up([2, 50/9, f], [2, 50/10, f]).
step_up([2, 50/12, f], [2, 50/13, f]).
step_up([2, 50/13, f], [2, 50/14, f]).
step_up([2, 50/16, f], [2, 50/17, f]).
step_up([2, 50/17, f], [2, 50/18, f]).
step_up([2, 50/18, f], [2, 50/19, f]).
step_up([2, 50/19, f], [2, 50/20, f]).
step_up([2, 50/20, f], [2, 50/21, f]).
step_up([2, 50/21, f], [2, 50/22, f]).
step_up([2, 50/24, f], [2, 50/25, f]).
step_up([2, 50/25, f], [2, 50/26, f]).
step_up([2, 50/28, f], [2, 50/29, f]).
step_up([2, 50/29, f], [2, 50/30, f]).
step_up([2, 50/34, f], [2, 50/35, f]).
step_up([2, 50/35, f], [2, 50/36, f]).
step_up([2, 50/36, f], [2, 50/37, f]).
step_up([2, 50/37, f], [2, 50/38, f]).
step_up([2, 50/38, f], [2, 50/39, f]).
step_up([2, 50/39, f], [2, 50/40, f]).
step_up([2, 50/42, f], [2, 50/43, f]).
step_up([2, 50/43, f], [2, 50/44, f]).
step_up([2, 50/46, f], [2, 50/47, f]).
step_up([2, 50/47, f], [2, 50/48, f]).
step_up([2, 50/48, f], [2, 50/49, f]).
step_up([2, 50/49, f], [2, 50/50, f]).
step_up([2, 50/52, f], [2, 50/53, f]).
step_up([2, 50/53, f], [2, 50/54, f]).
step_up([2, 50/54, f], [2, 50/55, f]).
step_up([2, 50/55, f], [2, 50/56, f]).
step_up([2, 50/58, f], [2, 50/59, f]).
step_up([2, 50/59, f], [2, 50/60, f]).
step_up([2, 50/64, f], [2, 50/65, f]).
step_up([2, 50/65, f], [2, 50/66, f]).
step_up([2, 50/70, f], [2, 50/71, f]).
step_up([2, 50/71, f], [2, 50/72, f]).
step_up([2, 50/74, f], [2, 50/75, f]).
step_up([2, 50/75, f], [2, 50/76, f]).
step_up([2, 50/76, f], [2, 50/77, f]).
step_up([2, 50/77, f], [2, 50/78, f]).
step_up([2, 50/80, f], [2, 50/81, f]).
step_up([2, 50/81, f], [2, 50/82, f]).
step_up([2, 50/84, f], [2, 50/85, f]).
step_up([2, 50/85, f], [2, 50/86, f]).
step_up([2, 50/88, f], [2, 50/89, f]).
step_up([2, 50/89, f], [2, 50/90, f]).
step_up([2, 50/98, f], [2, 50/99, f]).
step_up([2, 50/99, f], [2, 50/100, f]).
step_up([2, 50/102, f], [2, 50/103, f]).
step_up([2, 50/103, f], [2, 50/104, f]).
step_up([2, 50/104, f], [2, 50/105, f]).
step_up([2, 50/105, f], [2, 50/106, f]).
step_up([2, 50/106, f], [2, 50/107, f]).
step_up([2, 50/107, f], [2, 50/108, f]).
step_up([2, 50/110, f], [2, 50/111, f]).
step_up([2, 50/111, f], [2, 50/112, f]).
step_up([2, 50/114, f], [2, 50/115, f]).
step_up([2, 50/115, f], [2, 50/116, f]).
step_up([2, 52/2, f], [2, 52/3, f]).
step_up([2, 52/3, f], [2, 52/4, f]).
step_up([2, 52/8, f], [2, 52/9, f]).
step_up([2, 52/9, f], [2, 52/10, f]).
step_up([2, 52/10, f], [2, 52/11, f]).
step_up([2, 52/11, f], [2, 52/12, f]).
step_up([2, 52/14, f], [2, 52/15, f]).
step_up([2, 52/15, f], [2, 52/16, f]).
step_up([2, 52/16, f], [2, 52/17, f]).
step_up([2, 52/17, f], [2, 52/18, f]).
step_up([2, 52/20, f], [2, 52/21, f]).
step_up([2, 52/21, f], [2, 52/22, f]).
step_up([2, 52/26, f], [2, 52/27, f]).
step_up([2, 52/27, f], [2, 52/28, f]).
step_up([2, 52/30, f], [2, 52/31, f]).
step_up([2, 52/31, f], [2, 52/32, f]).
step_up([2, 52/36, f], [2, 52/37, f]).
step_up([2, 52/37, f], [2, 52/38, f]).
step_up([2, 52/40, f], [2, 52/41, f]).
step_up([2, 52/41, f], [2, 52/42, f]).
step_up([2, 52/46, f], [2, 52/47, f]).
step_up([2, 52/47, f], [2, 52/48, f]).
step_up([2, 52/50, f], [2, 52/51, f]).
step_up([2, 52/51, f], [2, 52/52, f]).
step_up([2, 52/52, f], [2, 52/53, f]).
step_up([2, 52/53, f], [2, 52/54, f]).
step_up([2, 52/54, f], [2, 52/55, f]).
step_up([2, 52/55, f], [2, 52/56, f]).
step_up([2, 52/56, f], [2, 52/57, f]).
step_up([2, 52/57, f], [2, 52/58, f]).
step_up([2, 52/60, f], [2, 52/61, f]).
step_up([2, 52/61, f], [2, 52/62, f]).
step_up([2, 52/66, f], [2, 52/67, f]).
step_up([2, 52/67, f], [2, 52/68, f]).
step_up([2, 52/70, f], [2, 52/71, f]).
step_up([2, 52/71, f], [2, 52/72, f]).
step_up([2, 52/72, f], [2, 52/73, f]).
step_up([2, 52/73, f], [2, 52/74, f]).
step_up([2, 52/76, f], [2, 52/77, f]).
step_up([2, 52/77, f], [2, 52/78, f]).
step_up([2, 52/78, f], [2, 52/79, f]).
step_up([2, 52/79, f], [2, 52/80, f]).
step_up([2, 52/84, f], [2, 52/85, f]).
step_up([2, 52/85, f], [2, 52/86, f]).
step_up([2, 52/90, f], [2, 52/91, f]).
step_up([2, 52/91, f], [2, 52/92, f]).
step_up([2, 52/96, f], [2, 52/97, f]).
step_up([2, 52/97, f], [2, 52/98, f]).
step_up([2, 52/98, f], [2, 52/99, f]).
step_up([2, 52/99, f], [2, 52/100, f]).
step_up([2, 52/100, f], [2, 52/101, f]).
step_up([2, 52/101, f], [2, 52/102, f]).
step_up([2, 52/102, f], [2, 52/103, f]).
step_up([2, 52/103, f], [2, 52/104, f]).
step_up([2, 52/104, f], [2, 52/105, f]).
step_up([2, 52/105, f], [2, 52/106, f]).
step_up([2, 52/110, f], [2, 52/111, f]).
step_up([2, 52/111, f], [2, 52/112, f]).
step_up([2, 54/0, f], [2, 54/1, f]).
step_up([2, 54/1, f], [2, 54/2, f]).
step_up([2, 54/8, f], [2, 54/9, f]).
step_up([2, 54/9, f], [2, 54/10, f]).
step_up([2, 54/12, f], [2, 54/13, f]).
step_up([2, 54/13, f], [2, 54/14, f]).
step_up([2, 54/16, f], [2, 54/17, f]).
step_up([2, 54/17, f], [2, 54/18, f]).
step_up([2, 54/18, f], [2, 54/19, f]).
step_up([2, 54/19, f], [2, 54/20, f]).
step_up([2, 54/22, f], [2, 54/23, f]).
step_up([2, 54/23, f], [2, 54/24, f]).
step_up([2, 54/26, f], [2, 54/27, f]).
step_up([2, 54/27, f], [2, 54/28, f]).
step_up([2, 54/30, f], [2, 54/31, f]).
step_up([2, 54/31, f], [2, 54/32, f]).
step_up([2, 54/32, f], [2, 54/33, f]).
step_up([2, 54/33, f], [2, 54/34, f]).
step_up([2, 54/36, f], [2, 54/37, f]).
step_up([2, 54/37, f], [2, 54/38, f]).
step_up([2, 54/38, f], [2, 54/39, f]).
step_up([2, 54/39, f], [2, 54/40, f]).
step_up([2, 54/42, f], [2, 54/43, f]).
step_up([2, 54/43, f], [2, 54/44, f]).
step_up([2, 54/46, f], [2, 54/47, f]).
step_up([2, 54/47, f], [2, 54/48, f]).
step_up([2, 54/52, f], [2, 54/53, f]).
step_up([2, 54/53, f], [2, 54/54, f]).
step_up([2, 54/56, f], [2, 54/57, f]).
step_up([2, 54/57, f], [2, 54/58, f]).
step_up([2, 54/58, f], [2, 54/59, f]).
step_up([2, 54/59, f], [2, 54/60, f]).
step_up([2, 54/62, f], [2, 54/63, f]).
step_up([2, 54/63, f], [2, 54/64, f]).
step_up([2, 54/66, f], [2, 54/67, f]).
step_up([2, 54/67, f], [2, 54/68, f]).
step_up([2, 54/68, f], [2, 54/69, f]).
step_up([2, 54/69, f], [2, 54/70, f]).
step_up([2, 54/70, f], [2, 54/71, f]).
step_up([2, 54/71, f], [2, 54/72, f]).
step_up([2, 54/72, f], [2, 54/73, f]).
step_up([2, 54/73, f], [2, 54/74, f]).
step_up([2, 54/76, f], [2, 54/77, f]).
step_up([2, 54/77, f], [2, 54/78, f]).
step_up([2, 54/80, f], [2, 54/81, f]).
step_up([2, 54/81, f], [2, 54/82, f]).
step_up([2, 54/84, f], [2, 54/85, f]).
step_up([2, 54/85, f], [2, 54/86, f]).
step_up([2, 54/88, f], [2, 54/89, f]).
step_up([2, 54/89, f], [2, 54/90, f]).
step_up([2, 54/92, f], [2, 54/93, f]).
step_up([2, 54/93, f], [2, 54/94, f]).
step_up([2, 54/94, f], [2, 54/95, f]).
step_up([2, 54/95, f], [2, 54/96, f]).
step_up([2, 54/96, f], [2, 54/97, f]).
step_up([2, 54/97, f], [2, 54/98, f]).
step_up([2, 54/98, f], [2, 54/99, f]).
step_up([2, 54/99, f], [2, 54/100, f]).
step_up([2, 54/102, f], [2, 54/103, f]).
step_up([2, 54/103, f], [2, 54/104, f]).
step_up([2, 54/104, f], [2, 54/105, f]).
step_up([2, 54/105, f], [2, 54/106, f]).
step_up([2, 54/112, f], [2, 54/113, f]).
step_up([2, 54/113, f], [2, 54/114, f]).
step_up([2, 54/116, f], [2, 54/117, f]).
step_up([2, 54/117, f], [2, 54/118, f]).
step_up([2, 56/0, f], [2, 56/1, f]).
step_up([2, 56/1, f], [2, 56/2, f]).
step_up([2, 56/2, f], [2, 56/3, f]).
step_up([2, 56/3, f], [2, 56/4, f]).
step_up([2, 56/6, f], [2, 56/7, f]).
step_up([2, 56/7, f], [2, 56/8, f]).
step_up([2, 56/10, f], [2, 56/11, f]).
step_up([2, 56/11, f], [2, 56/12, f]).
step_up([2, 56/14, f], [2, 56/15, f]).
step_up([2, 56/15, f], [2, 56/16, f]).
step_up([2, 56/18, f], [2, 56/19, f]).
step_up([2, 56/19, f], [2, 56/20, f]).
step_up([2, 56/22, f], [2, 56/23, f]).
step_up([2, 56/23, f], [2, 56/24, f]).
step_up([2, 56/24, f], [2, 56/25, f]).
step_up([2, 56/25, f], [2, 56/26, f]).
step_up([2, 56/28, f], [2, 56/29, f]).
step_up([2, 56/29, f], [2, 56/30, f]).
step_up([2, 56/30, f], [2, 56/31, f]).
step_up([2, 56/31, f], [2, 56/32, f]).
step_up([2, 56/32, f], [2, 56/33, f]).
step_up([2, 56/33, f], [2, 56/34, f]).
step_up([2, 56/34, f], [2, 56/35, f]).
step_up([2, 56/35, f], [2, 56/36, f]).
step_up([2, 56/38, f], [2, 56/39, f]).
step_up([2, 56/39, f], [2, 56/40, f]).
step_up([2, 56/42, f], [2, 56/43, f]).
step_up([2, 56/43, f], [2, 56/44, f]).
step_up([2, 56/44, f], [2, 56/45, f]).
step_up([2, 56/45, f], [2, 56/46, f]).
step_up([2, 56/48, f], [2, 56/49, f]).
step_up([2, 56/49, f], [2, 56/50, f]).
step_up([2, 56/50, f], [2, 56/51, f]).
step_up([2, 56/51, f], [2, 56/52, f]).
step_up([2, 56/54, f], [2, 56/55, f]).
step_up([2, 56/55, f], [2, 56/56, f]).
step_up([2, 56/56, f], [2, 56/57, f]).
step_up([2, 56/57, f], [2, 56/58, f]).
step_up([2, 56/60, f], [2, 56/61, f]).
step_up([2, 56/61, f], [2, 56/62, f]).
step_up([2, 56/62, f], [2, 56/63, f]).
step_up([2, 56/63, f], [2, 56/64, f]).
step_up([2, 56/66, f], [2, 56/67, f]).
step_up([2, 56/67, f], [2, 56/68, f]).
step_up([2, 56/70, f], [2, 56/71, f]).
step_up([2, 56/71, f], [2, 56/72, f]).
step_up([2, 56/72, f], [2, 56/73, f]).
step_up([2, 56/73, f], [2, 56/74, f]).
step_up([2, 56/74, f], [2, 56/75, f]).
step_up([2, 56/75, f], [2, 56/76, f]).
step_up([2, 56/78, f], [2, 56/79, f]).
step_up([2, 56/79, f], [2, 56/80, f]).
step_up([2, 56/92, f], [2, 56/93, f]).
step_up([2, 56/93, f], [2, 56/94, f]).
step_up([2, 56/96, f], [2, 56/97, f]).
step_up([2, 56/97, f], [2, 56/98, f]).
step_up([2, 56/98, f], [2, 56/99, f]).
step_up([2, 56/99, f], [2, 56/100, f]).
step_up([2, 56/102, f], [2, 56/103, f]).
step_up([2, 56/103, f], [2, 56/104, f]).
step_up([2, 56/104, f], [2, 56/105, f]).
step_up([2, 56/105, f], [2, 56/106, f]).
step_up([2, 56/108, f], [2, 56/109, f]).
step_up([2, 56/109, f], [2, 56/110, f]).
step_up([2, 56/112, f], [2, 56/113, f]).
step_up([2, 56/113, f], [2, 56/114, f]).
step_up([2, 58/4, f], [2, 58/5, f]).
step_up([2, 58/5, f], [2, 58/6, f]).
step_up([2, 58/10, f], [2, 58/11, f]).
step_up([2, 58/11, f], [2, 58/12, f]).
step_up([2, 58/12, f], [2, 58/13, f]).
step_up([2, 58/13, f], [2, 58/14, f]).
step_up([2, 58/16, f], [2, 58/17, f]).
step_up([2, 58/17, f], [2, 58/18, f]).
step_up([2, 58/18, f], [2, 58/19, f]).
step_up([2, 58/19, f], [2, 58/20, f]).
step_up([2, 58/20, f], [2, 58/21, f]).
step_up([2, 58/21, f], [2, 58/22, f]).
step_up([2, 58/24, f], [2, 58/25, f]).
step_up([2, 58/25, f], [2, 58/26, f]).
step_up([2, 58/28, f], [2, 58/29, f]).
step_up([2, 58/29, f], [2, 58/30, f]).
step_up([2, 58/30, f], [2, 58/31, f]).
step_up([2, 58/31, f], [2, 58/32, f]).
step_up([2, 58/32, f], [2, 58/33, f]).
step_up([2, 58/33, f], [2, 58/34, f]).
step_up([2, 58/36, f], [2, 58/37, f]).
step_up([2, 58/37, f], [2, 58/38, f]).
step_up([2, 58/42, f], [2, 58/43, f]).
step_up([2, 58/43, f], [2, 58/44, f]).
step_up([2, 58/44, f], [2, 58/45, f]).
step_up([2, 58/45, f], [2, 58/46, f]).
step_up([2, 58/46, f], [2, 58/47, f]).
step_up([2, 58/47, f], [2, 58/48, f]).
step_up([2, 58/48, f], [2, 58/49, f]).
step_up([2, 58/49, f], [2, 58/50, f]).
step_up([2, 58/54, f], [2, 58/55, f]).
step_up([2, 58/55, f], [2, 58/56, f]).
step_up([2, 58/56, f], [2, 58/57, f]).
step_up([2, 58/57, f], [2, 58/58, f]).
step_up([2, 58/58, f], [2, 58/59, f]).
step_up([2, 58/59, f], [2, 58/60, f]).
step_up([2, 58/62, f], [2, 58/63, f]).
step_up([2, 58/63, f], [2, 58/64, f]).
step_up([2, 58/68, f], [2, 58/69, f]).
step_up([2, 58/69, f], [2, 58/70, f]).
step_up([2, 58/72, f], [2, 58/73, f]).
step_up([2, 58/73, f], [2, 58/74, f]).
step_up([2, 58/74, f], [2, 58/75, f]).
step_up([2, 58/75, f], [2, 58/76, f]).
step_up([2, 58/76, f], [2, 58/77, f]).
step_up([2, 58/77, f], [2, 58/78, f]).
step_up([2, 58/78, f], [2, 58/79, f]).
step_up([2, 58/79, f], [2, 58/80, f]).
step_up([2, 58/82, f], [2, 58/83, f]).
step_up([2, 58/83, f], [2, 58/84, f]).
step_up([2, 58/90, f], [2, 58/91, f]).
step_up([2, 58/91, f], [2, 58/92, f]).
step_up([2, 58/94, f], [2, 58/95, f]).
step_up([2, 58/95, f], [2, 58/96, f]).
step_up([2, 58/98, f], [2, 58/99, f]).
step_up([2, 58/99, f], [2, 58/100, f]).
step_up([2, 58/102, f], [2, 58/103, f]).
step_up([2, 58/103, f], [2, 58/104, f]).
step_up([2, 58/106, f], [2, 58/107, f]).
step_up([2, 58/107, f], [2, 58/108, f]).
step_up([2, 58/112, f], [2, 58/113, f]).
step_up([2, 58/113, f], [2, 58/114, f]).
step_up([2, 58/116, f], [2, 58/117, f]).
step_up([2, 58/117, f], [2, 58/118, f]).
step_up([2, 60/0, f], [2, 60/1, f]).
step_up([2, 60/1, f], [2, 60/2, f]).
step_up([2, 60/6, f], [2, 60/7, f]).
step_up([2, 60/7, f], [2, 60/8, f]).
step_up([2, 60/12, f], [2, 60/13, f]).
step_up([2, 60/13, f], [2, 60/14, f]).
step_up([2, 60/14, f], [2, 60/15, f]).
step_up([2, 60/15, f], [2, 60/16, f]).
step_up([2, 60/16, f], [2, 60/17, f]).
step_up([2, 60/17, f], [2, 60/18, f]).
step_up([2, 60/18, f], [2, 60/19, f]).
step_up([2, 60/19, f], [2, 60/20, f]).
step_up([2, 60/20, f], [2, 60/21, f]).
step_up([2, 60/21, f], [2, 60/22, f]).
step_up([2, 60/24, f], [2, 60/25, f]).
step_up([2, 60/25, f], [2, 60/26, f]).
step_up([2, 60/26, f], [2, 60/27, f]).
step_up([2, 60/27, f], [2, 60/28, f]).
step_up([2, 60/30, f], [2, 60/31, f]).
step_up([2, 60/31, f], [2, 60/32, f]).
step_up([2, 60/34, f], [2, 60/35, f]).
step_up([2, 60/35, f], [2, 60/36, f]).
step_up([2, 60/36, f], [2, 60/37, f]).
step_up([2, 60/37, f], [2, 60/38, f]).
step_up([2, 60/40, f], [2, 60/41, f]).
step_up([2, 60/41, f], [2, 60/42, f]).
step_up([2, 60/42, f], [2, 60/43, f]).
step_up([2, 60/43, f], [2, 60/44, f]).
step_up([2, 60/46, f], [2, 60/47, f]).
step_up([2, 60/47, f], [2, 60/48, f]).
step_up([2, 60/48, f], [2, 60/49, f]).
step_up([2, 60/49, f], [2, 60/50, f]).
step_up([2, 60/56, f], [2, 60/57, f]).
step_up([2, 60/57, f], [2, 60/58, f]).
step_up([2, 60/62, f], [2, 60/63, f]).
step_up([2, 60/63, f], [2, 60/64, f]).
step_up([2, 60/66, f], [2, 60/67, f]).
step_up([2, 60/67, f], [2, 60/68, f]).
step_up([2, 60/70, f], [2, 60/71, f]).
step_up([2, 60/71, f], [2, 60/72, f]).
step_up([2, 60/74, f], [2, 60/75, f]).
step_up([2, 60/75, f], [2, 60/76, f]).
step_up([2, 60/76, f], [2, 60/77, f]).
step_up([2, 60/77, f], [2, 60/78, f]).
step_up([2, 60/82, f], [2, 60/83, f]).
step_up([2, 60/83, f], [2, 60/84, f]).
step_up([2, 60/84, f], [2, 60/85, f]).
step_up([2, 60/85, f], [2, 60/86, f]).
step_up([2, 60/88, f], [2, 60/89, f]).
step_up([2, 60/89, f], [2, 60/90, f]).
step_up([2, 60/92, f], [2, 60/93, f]).
step_up([2, 60/93, f], [2, 60/94, f]).
step_up([2, 60/96, f], [2, 60/97, f]).
step_up([2, 60/97, f], [2, 60/98, f]).
step_up([2, 60/100, f], [2, 60/101, f]).
step_up([2, 60/101, f], [2, 60/102, f]).
step_up([2, 60/104, f], [2, 60/105, f]).
step_up([2, 60/105, f], [2, 60/106, f]).
step_up([2, 60/108, f], [2, 60/109, f]).
step_up([2, 60/109, f], [2, 60/110, f]).
step_up([2, 60/110, f], [2, 60/111, f]).
step_up([2, 60/111, f], [2, 60/112, f]).
step_up([2, 60/114, f], [2, 60/115, f]).
step_up([2, 60/115, f], [2, 60/116, f]).
step_up([2, 62/2, f], [2, 62/3, f]).
step_up([2, 62/3, f], [2, 62/4, f]).
step_up([2, 62/16, f], [2, 62/17, f]).
step_up([2, 62/17, f], [2, 62/18, f]).
step_up([2, 62/20, f], [2, 62/21, f]).
step_up([2, 62/21, f], [2, 62/22, f]).
step_up([2, 62/24, f], [2, 62/25, f]).
step_up([2, 62/25, f], [2, 62/26, f]).
step_up([2, 62/28, f], [2, 62/29, f]).
step_up([2, 62/29, f], [2, 62/30, f]).
step_up([2, 62/32, f], [2, 62/33, f]).
step_up([2, 62/33, f], [2, 62/34, f]).
step_up([2, 62/38, f], [2, 62/39, f]).
step_up([2, 62/39, f], [2, 62/40, f]).
step_up([2, 62/42, f], [2, 62/43, f]).
step_up([2, 62/43, f], [2, 62/44, f]).
step_up([2, 62/48, f], [2, 62/49, f]).
step_up([2, 62/49, f], [2, 62/50, f]).
step_up([2, 62/50, f], [2, 62/51, f]).
step_up([2, 62/51, f], [2, 62/52, f]).
step_up([2, 62/52, f], [2, 62/53, f]).
step_up([2, 62/53, f], [2, 62/54, f]).
step_up([2, 62/56, f], [2, 62/57, f]).
step_up([2, 62/57, f], [2, 62/58, f]).
step_up([2, 62/62, f], [2, 62/63, f]).
step_up([2, 62/63, f], [2, 62/64, f]).
step_up([2, 62/64, f], [2, 62/65, f]).
step_up([2, 62/65, f], [2, 62/66, f]).
step_up([2, 62/68, f], [2, 62/69, f]).
step_up([2, 62/69, f], [2, 62/70, f]).
step_up([2, 62/70, f], [2, 62/71, f]).
step_up([2, 62/71, f], [2, 62/72, f]).
step_up([2, 62/72, f], [2, 62/73, f]).
step_up([2, 62/73, f], [2, 62/74, f]).
step_up([2, 62/76, f], [2, 62/77, f]).
step_up([2, 62/77, f], [2, 62/78, f]).
step_up([2, 62/80, f], [2, 62/81, f]).
step_up([2, 62/81, f], [2, 62/82, f]).
step_up([2, 62/84, f], [2, 62/85, f]).
step_up([2, 62/85, f], [2, 62/86, f]).
step_up([2, 62/86, f], [2, 62/87, f]).
step_up([2, 62/87, f], [2, 62/88, f]).
step_up([2, 62/88, f], [2, 62/89, f]).
step_up([2, 62/89, f], [2, 62/90, f]).
step_up([2, 62/92, f], [2, 62/93, f]).
step_up([2, 62/93, f], [2, 62/94, f]).
step_up([2, 62/94, f], [2, 62/95, f]).
step_up([2, 62/95, f], [2, 62/96, f]).
step_up([2, 62/96, f], [2, 62/97, f]).
step_up([2, 62/97, f], [2, 62/98, f]).
step_up([2, 62/102, f], [2, 62/103, f]).
step_up([2, 62/103, f], [2, 62/104, f]).
step_up([2, 62/106, f], [2, 62/107, f]).
step_up([2, 62/107, f], [2, 62/108, f]).
step_up([2, 62/112, f], [2, 62/113, f]).
step_up([2, 62/113, f], [2, 62/114, f]).
step_up([2, 62/114, f], [2, 62/115, f]).
step_up([2, 62/115, f], [2, 62/116, f]).
step_up([2, 64/0, f], [2, 64/1, f]).
step_up([2, 64/1, f], [2, 64/2, f]).
step_up([2, 64/2, f], [2, 64/3, f]).
step_up([2, 64/3, f], [2, 64/4, f]).
step_up([2, 64/4, f], [2, 64/5, f]).
step_up([2, 64/5, f], [2, 64/6, f]).
step_up([2, 64/8, f], [2, 64/9, f]).
step_up([2, 64/9, f], [2, 64/10, f]).
step_up([2, 64/14, f], [2, 64/15, f]).
step_up([2, 64/15, f], [2, 64/16, f]).
step_up([2, 64/18, f], [2, 64/19, f]).
step_up([2, 64/19, f], [2, 64/20, f]).
step_up([2, 64/20, f], [2, 64/21, f]).
step_up([2, 64/21, f], [2, 64/22, f]).
step_up([2, 64/22, f], [2, 64/23, f]).
step_up([2, 64/23, f], [2, 64/24, f]).
step_up([2, 64/26, f], [2, 64/27, f]).
step_up([2, 64/27, f], [2, 64/28, f]).
step_up([2, 64/34, f], [2, 64/35, f]).
step_up([2, 64/35, f], [2, 64/36, f]).
step_up([2, 64/36, f], [2, 64/37, f]).
step_up([2, 64/37, f], [2, 64/38, f]).
step_up([2, 64/40, f], [2, 64/41, f]).
step_up([2, 64/41, f], [2, 64/42, f]).
step_up([2, 64/42, f], [2, 64/43, f]).
step_up([2, 64/43, f], [2, 64/44, f]).
step_up([2, 64/50, f], [2, 64/51, f]).
step_up([2, 64/51, f], [2, 64/52, f]).
step_up([2, 64/54, f], [2, 64/55, f]).
step_up([2, 64/55, f], [2, 64/56, f]).
step_up([2, 64/60, f], [2, 64/61, f]).
step_up([2, 64/61, f], [2, 64/62, f]).
step_up([2, 64/62, f], [2, 64/63, f]).
step_up([2, 64/63, f], [2, 64/64, f]).
step_up([2, 64/64, f], [2, 64/65, f]).
step_up([2, 64/65, f], [2, 64/66, f]).
step_up([2, 64/66, f], [2, 64/67, f]).
step_up([2, 64/67, f], [2, 64/68, f]).
step_up([2, 64/72, f], [2, 64/73, f]).
step_up([2, 64/73, f], [2, 64/74, f]).
step_up([2, 64/78, f], [2, 64/79, f]).
step_up([2, 64/79, f], [2, 64/80, f]).
step_up([2, 64/86, f], [2, 64/87, f]).
step_up([2, 64/87, f], [2, 64/88, f]).
step_up([2, 64/88, f], [2, 64/89, f]).
step_up([2, 64/89, f], [2, 64/90, f]).
step_up([2, 64/90, f], [2, 64/91, f]).
step_up([2, 64/91, f], [2, 64/92, f]).
step_up([2, 64/94, f], [2, 64/95, f]).
step_up([2, 64/95, f], [2, 64/96, f]).
step_up([2, 64/100, f], [2, 64/101, f]).
step_up([2, 64/101, f], [2, 64/102, f]).
step_up([2, 64/104, f], [2, 64/105, f]).
step_up([2, 64/105, f], [2, 64/106, f]).
step_up([2, 64/110, f], [2, 64/111, f]).
step_up([2, 64/111, f], [2, 64/112, f]).
step_up([2, 66/0, f], [2, 66/1, f]).
step_up([2, 66/1, f], [2, 66/2, f]).
step_up([2, 66/2, f], [2, 66/3, f]).
step_up([2, 66/3, f], [2, 66/4, f]).
step_up([2, 66/8, f], [2, 66/9, f]).
step_up([2, 66/9, f], [2, 66/10, f]).
step_up([2, 66/10, f], [2, 66/11, f]).
step_up([2, 66/11, f], [2, 66/12, f]).
step_up([2, 66/14, f], [2, 66/15, f]).
step_up([2, 66/15, f], [2, 66/16, f]).
step_up([2, 66/16, f], [2, 66/17, f]).
step_up([2, 66/17, f], [2, 66/18, f]).
step_up([2, 66/18, f], [2, 66/19, f]).
step_up([2, 66/19, f], [2, 66/20, f]).
step_up([2, 66/22, f], [2, 66/23, f]).
step_up([2, 66/23, f], [2, 66/24, f]).
step_up([2, 66/26, f], [2, 66/27, f]).
step_up([2, 66/27, f], [2, 66/28, f]).
step_up([2, 66/32, f], [2, 66/33, f]).
step_up([2, 66/33, f], [2, 66/34, f]).
step_up([2, 66/40, f], [2, 66/41, f]).
step_up([2, 66/41, f], [2, 66/42, f]).
step_up([2, 66/46, f], [2, 66/47, f]).
step_up([2, 66/47, f], [2, 66/48, f]).
step_up([2, 66/52, f], [2, 66/53, f]).
step_up([2, 66/53, f], [2, 66/54, f]).
step_up([2, 66/56, f], [2, 66/57, f]).
step_up([2, 66/57, f], [2, 66/58, f]).
step_up([2, 66/60, f], [2, 66/61, f]).
step_up([2, 66/61, f], [2, 66/62, f]).
step_up([2, 66/62, f], [2, 66/63, f]).
step_up([2, 66/63, f], [2, 66/64, f]).
step_up([2, 66/64, f], [2, 66/65, f]).
step_up([2, 66/65, f], [2, 66/66, f]).
step_up([2, 66/66, f], [2, 66/67, f]).
step_up([2, 66/67, f], [2, 66/68, f]).
step_up([2, 66/76, f], [2, 66/77, f]).
step_up([2, 66/77, f], [2, 66/78, f]).
step_up([2, 66/80, f], [2, 66/81, f]).
step_up([2, 66/81, f], [2, 66/82, f]).
step_up([2, 66/84, f], [2, 66/85, f]).
step_up([2, 66/85, f], [2, 66/86, f]).
step_up([2, 66/88, f], [2, 66/89, f]).
step_up([2, 66/89, f], [2, 66/90, f]).
step_up([2, 66/90, f], [2, 66/91, f]).
step_up([2, 66/91, f], [2, 66/92, f]).
step_up([2, 66/96, f], [2, 66/97, f]).
step_up([2, 66/97, f], [2, 66/98, f]).
step_up([2, 66/100, f], [2, 66/101, f]).
step_up([2, 66/101, f], [2, 66/102, f]).
step_up([2, 66/104, f], [2, 66/105, f]).
step_up([2, 66/105, f], [2, 66/106, f]).
step_up([2, 66/110, f], [2, 66/111, f]).
step_up([2, 66/111, f], [2, 66/112, f]).
step_up([2, 68/2, f], [2, 68/3, f]).
step_up([2, 68/3, f], [2, 68/4, f]).
step_up([2, 68/10, f], [2, 68/11, f]).
step_up([2, 68/11, f], [2, 68/12, f]).
step_up([2, 68/14, f], [2, 68/15, f]).
step_up([2, 68/15, f], [2, 68/16, f]).
step_up([2, 68/16, f], [2, 68/17, f]).
step_up([2, 68/17, f], [2, 68/18, f]).
step_up([2, 68/22, f], [2, 68/23, f]).
step_up([2, 68/23, f], [2, 68/24, f]).
step_up([2, 68/24, f], [2, 68/25, f]).
step_up([2, 68/25, f], [2, 68/26, f]).
step_up([2, 68/28, f], [2, 68/29, f]).
step_up([2, 68/29, f], [2, 68/30, f]).
step_up([2, 68/38, f], [2, 68/39, f]).
step_up([2, 68/39, f], [2, 68/40, f]).
step_up([2, 68/42, f], [2, 68/43, f]).
step_up([2, 68/43, f], [2, 68/44, f]).
step_up([2, 68/46, f], [2, 68/47, f]).
step_up([2, 68/47, f], [2, 68/48, f]).
step_up([2, 68/48, f], [2, 68/49, f]).
step_up([2, 68/49, f], [2, 68/50, f]).
step_up([2, 68/52, f], [2, 68/53, f]).
step_up([2, 68/53, f], [2, 68/54, f]).
step_up([2, 68/54, f], [2, 68/55, f]).
step_up([2, 68/55, f], [2, 68/56, f]).
step_up([2, 68/56, f], [2, 68/57, f]).
step_up([2, 68/57, f], [2, 68/58, f]).
step_up([2, 68/58, f], [2, 68/59, f]).
step_up([2, 68/59, f], [2, 68/60, f]).
step_up([2, 68/62, f], [2, 68/63, f]).
step_up([2, 68/63, f], [2, 68/64, f]).
step_up([2, 68/64, f], [2, 68/65, f]).
step_up([2, 68/65, f], [2, 68/66, f]).
step_up([2, 68/68, f], [2, 68/69, f]).
step_up([2, 68/69, f], [2, 68/70, f]).
step_up([2, 68/70, f], [2, 68/71, f]).
step_up([2, 68/71, f], [2, 68/72, f]).
step_up([2, 68/74, f], [2, 68/75, f]).
step_up([2, 68/75, f], [2, 68/76, f]).
step_up([2, 68/76, f], [2, 68/77, f]).
step_up([2, 68/77, f], [2, 68/78, f]).
step_up([2, 68/78, f], [2, 68/79, f]).
step_up([2, 68/79, f], [2, 68/80, f]).
step_up([2, 68/80, f], [2, 68/81, f]).
step_up([2, 68/81, f], [2, 68/82, f]).
step_up([2, 68/82, f], [2, 68/83, f]).
step_up([2, 68/83, f], [2, 68/84, f]).
step_up([2, 68/96, f], [2, 68/97, f]).
step_up([2, 68/97, f], [2, 68/98, f]).
step_up([2, 68/98, f], [2, 68/99, f]).
step_up([2, 68/99, f], [2, 68/100, f]).
step_up([2, 68/102, f], [2, 68/103, f]).
step_up([2, 68/103, f], [2, 68/104, f]).
step_up([2, 68/108, f], [2, 68/109, f]).
step_up([2, 68/109, f], [2, 68/110, f]).
step_up([2, 68/112, f], [2, 68/113, f]).
step_up([2, 68/113, f], [2, 68/114, f]).
step_up([2, 70/0, f], [2, 70/1, f]).
step_up([2, 70/1, f], [2, 70/2, f]).
step_up([2, 70/4, f], [2, 70/5, f]).
step_up([2, 70/5, f], [2, 70/6, f]).
step_up([2, 70/8, f], [2, 70/9, f]).
step_up([2, 70/9, f], [2, 70/10, f]).
step_up([2, 70/12, f], [2, 70/13, f]).
step_up([2, 70/13, f], [2, 70/14, f]).
step_up([2, 70/14, f], [2, 70/15, f]).
step_up([2, 70/15, f], [2, 70/16, f]).
step_up([2, 70/24, f], [2, 70/25, f]).
step_up([2, 70/25, f], [2, 70/26, f]).
step_up([2, 70/26, f], [2, 70/27, f]).
step_up([2, 70/27, f], [2, 70/28, f]).
step_up([2, 70/30, f], [2, 70/31, f]).
step_up([2, 70/31, f], [2, 70/32, f]).
step_up([2, 70/32, f], [2, 70/33, f]).
step_up([2, 70/33, f], [2, 70/34, f]).
step_up([2, 70/36, f], [2, 70/37, f]).
step_up([2, 70/37, f], [2, 70/38, f]).
step_up([2, 70/38, f], [2, 70/39, f]).
step_up([2, 70/39, f], [2, 70/40, f]).
step_up([2, 70/42, f], [2, 70/43, f]).
step_up([2, 70/43, f], [2, 70/44, f]).
step_up([2, 70/44, f], [2, 70/45, f]).
step_up([2, 70/45, f], [2, 70/46, f]).
step_up([2, 70/46, f], [2, 70/47, f]).
step_up([2, 70/47, f], [2, 70/48, f]).
step_up([2, 70/48, f], [2, 70/49, f]).
step_up([2, 70/49, f], [2, 70/50, f]).
step_up([2, 70/52, f], [2, 70/53, f]).
step_up([2, 70/53, f], [2, 70/54, f]).
step_up([2, 70/54, f], [2, 70/55, f]).
step_up([2, 70/55, f], [2, 70/56, f]).
step_up([2, 70/58, f], [2, 70/59, f]).
step_up([2, 70/59, f], [2, 70/60, f]).
step_up([2, 70/62, f], [2, 70/63, f]).
step_up([2, 70/63, f], [2, 70/64, f]).
step_up([2, 70/70, f], [2, 70/71, f]).
step_up([2, 70/71, f], [2, 70/72, f]).
step_up([2, 70/72, f], [2, 70/73, f]).
step_up([2, 70/73, f], [2, 70/74, f]).
step_up([2, 70/76, f], [2, 70/77, f]).
step_up([2, 70/77, f], [2, 70/78, f]).
step_up([2, 70/78, f], [2, 70/79, f]).
step_up([2, 70/79, f], [2, 70/80, f]).
step_up([2, 70/80, f], [2, 70/81, f]).
step_up([2, 70/81, f], [2, 70/82, f]).
step_up([2, 70/84, f], [2, 70/85, f]).
step_up([2, 70/85, f], [2, 70/86, f]).
step_up([2, 70/88, f], [2, 70/89, f]).
step_up([2, 70/89, f], [2, 70/90, f]).
step_up([2, 70/94, f], [2, 70/95, f]).
step_up([2, 70/95, f], [2, 70/96, f]).
step_up([2, 70/96, f], [2, 70/97, f]).
step_up([2, 70/97, f], [2, 70/98, f]).
step_up([2, 70/102, f], [2, 70/103, f]).
step_up([2, 70/103, f], [2, 70/104, f]).
step_up([2, 70/110, f], [2, 70/111, f]).
step_up([2, 70/111, f], [2, 70/112, f]).
step_up([2, 70/114, f], [2, 70/115, f]).
step_up([2, 70/115, f], [2, 70/116, f]).
step_up([2, 70/116, f], [2, 70/117, f]).
step_up([2, 70/117, f], [2, 70/118, f]).
step_up([2, 72/0, f], [2, 72/1, f]).
step_up([2, 72/1, f], [2, 72/2, f]).
step_up([2, 72/4, f], [2, 72/5, f]).
step_up([2, 72/5, f], [2, 72/6, f]).
step_up([2, 72/6, f], [2, 72/7, f]).
step_up([2, 72/7, f], [2, 72/8, f]).
step_up([2, 72/10, f], [2, 72/11, f]).
step_up([2, 72/11, f], [2, 72/12, f]).
step_up([2, 72/12, f], [2, 72/13, f]).
step_up([2, 72/13, f], [2, 72/14, f]).
step_up([2, 72/20, f], [2, 72/21, f]).
step_up([2, 72/21, f], [2, 72/22, f]).
step_up([2, 72/26, f], [2, 72/27, f]).
step_up([2, 72/27, f], [2, 72/28, f]).
step_up([2, 72/28, f], [2, 72/29, f]).
step_up([2, 72/29, f], [2, 72/30, f]).
step_up([2, 72/30, f], [2, 72/31, f]).
step_up([2, 72/31, f], [2, 72/32, f]).
step_up([2, 72/34, f], [2, 72/35, f]).
step_up([2, 72/35, f], [2, 72/36, f]).
step_up([2, 72/36, f], [2, 72/37, f]).
step_up([2, 72/37, f], [2, 72/38, f]).
step_up([2, 72/40, f], [2, 72/41, f]).
step_up([2, 72/41, f], [2, 72/42, f]).
step_up([2, 72/42, f], [2, 72/43, f]).
step_up([2, 72/43, f], [2, 72/44, f]).
step_up([2, 72/44, f], [2, 72/45, f]).
step_up([2, 72/45, f], [2, 72/46, f]).
step_up([2, 72/46, f], [2, 72/47, f]).
step_up([2, 72/47, f], [2, 72/48, f]).
step_up([2, 72/50, f], [2, 72/51, f]).
step_up([2, 72/51, f], [2, 72/52, f]).
step_up([2, 72/54, f], [2, 72/55, f]).
step_up([2, 72/55, f], [2, 72/56, f]).
step_up([2, 72/60, f], [2, 72/61, f]).
step_up([2, 72/61, f], [2, 72/62, f]).
step_up([2, 72/62, f], [2, 72/63, f]).
step_up([2, 72/63, f], [2, 72/64, f]).
step_up([2, 72/68, f], [2, 72/69, f]).
step_up([2, 72/69, f], [2, 72/70, f]).
step_up([2, 72/76, f], [2, 72/77, f]).
step_up([2, 72/77, f], [2, 72/78, f]).
step_up([2, 72/82, f], [2, 72/83, f]).
step_up([2, 72/83, f], [2, 72/84, f]).
step_up([2, 72/84, f], [2, 72/85, f]).
step_up([2, 72/85, f], [2, 72/86, f]).
step_up([2, 72/86, f], [2, 72/87, f]).
step_up([2, 72/87, f], [2, 72/88, f]).
step_up([2, 72/94, f], [2, 72/95, f]).
step_up([2, 72/95, f], [2, 72/96, f]).
step_up([2, 72/98, f], [2, 72/99, f]).
step_up([2, 72/99, f], [2, 72/100, f]).
step_up([2, 72/104, f], [2, 72/105, f]).
step_up([2, 72/105, f], [2, 72/106, f]).
step_up([2, 72/114, f], [2, 72/115, f]).
step_up([2, 72/115, f], [2, 72/116, f]).
step_up([2, 72/116, f], [2, 72/117, f]).
step_up([2, 72/117, f], [2, 72/118, f]).
step_up([2, 74/2, f], [2, 74/3, f]).
step_up([2, 74/3, f], [2, 74/4, f]).
step_up([2, 74/4, f], [2, 74/5, f]).
step_up([2, 74/5, f], [2, 74/6, f]).
step_up([2, 74/6, f], [2, 74/7, f]).
step_up([2, 74/7, f], [2, 74/8, f]).
step_up([2, 74/8, f], [2, 74/9, f]).
step_up([2, 74/9, f], [2, 74/10, f]).
step_up([2, 74/12, f], [2, 74/13, f]).
step_up([2, 74/13, f], [2, 74/14, f]).
step_up([2, 74/24, f], [2, 74/25, f]).
step_up([2, 74/25, f], [2, 74/26, f]).
step_up([2, 74/26, f], [2, 74/27, f]).
step_up([2, 74/27, f], [2, 74/28, f]).
step_up([2, 74/30, f], [2, 74/31, f]).
step_up([2, 74/31, f], [2, 74/32, f]).
step_up([2, 74/32, f], [2, 74/33, f]).
step_up([2, 74/33, f], [2, 74/34, f]).
step_up([2, 74/34, f], [2, 74/35, f]).
step_up([2, 74/35, f], [2, 74/36, f]).
step_up([2, 74/40, f], [2, 74/41, f]).
step_up([2, 74/41, f], [2, 74/42, f]).
step_up([2, 74/44, f], [2, 74/45, f]).
step_up([2, 74/45, f], [2, 74/46, f]).
step_up([2, 74/48, f], [2, 74/49, f]).
step_up([2, 74/49, f], [2, 74/50, f]).
step_up([2, 74/58, f], [2, 74/59, f]).
step_up([2, 74/59, f], [2, 74/60, f]).
step_up([2, 74/62, f], [2, 74/63, f]).
step_up([2, 74/63, f], [2, 74/64, f]).
step_up([2, 74/68, f], [2, 74/69, f]).
step_up([2, 74/69, f], [2, 74/70, f]).
step_up([2, 74/70, f], [2, 74/71, f]).
step_up([2, 74/71, f], [2, 74/72, f]).
step_up([2, 74/72, f], [2, 74/73, f]).
step_up([2, 74/73, f], [2, 74/74, f]).
step_up([2, 74/76, f], [2, 74/77, f]).
step_up([2, 74/77, f], [2, 74/78, f]).
step_up([2, 74/80, f], [2, 74/81, f]).
step_up([2, 74/81, f], [2, 74/82, f]).
step_up([2, 74/82, f], [2, 74/83, f]).
step_up([2, 74/83, f], [2, 74/84, f]).
step_up([2, 74/84, f], [2, 74/85, f]).
step_up([2, 74/85, f], [2, 74/86, f]).
step_up([2, 74/88, f], [2, 74/89, f]).
step_up([2, 74/89, f], [2, 74/90, f]).
step_up([2, 74/92, f], [2, 74/93, f]).
step_up([2, 74/93, f], [2, 74/94, f]).
step_up([2, 74/96, f], [2, 74/97, f]).
step_up([2, 74/97, f], [2, 74/98, f]).
step_up([2, 74/98, f], [2, 74/99, f]).
step_up([2, 74/99, f], [2, 74/100, f]).
step_up([2, 74/100, f], [2, 74/101, f]).
step_up([2, 74/101, f], [2, 74/102, f]).
step_up([2, 74/106, f], [2, 74/107, f]).
step_up([2, 74/107, f], [2, 74/108, f]).
step_up([2, 74/110, f], [2, 74/111, f]).
step_up([2, 74/111, f], [2, 74/112, f]).
step_up([2, 74/112, f], [2, 74/113, f]).
step_up([2, 74/113, f], [2, 74/114, f]).
step_up([2, 74/114, f], [2, 74/115, f]).
step_up([2, 74/115, f], [2, 74/116, f]).
step_up([2, 76/0, f], [2, 76/1, f]).
step_up([2, 76/1, f], [2, 76/2, f]).
step_up([2, 76/2, f], [2, 76/3, f]).
step_up([2, 76/3, f], [2, 76/4, f]).
step_up([2, 76/10, f], [2, 76/11, f]).
step_up([2, 76/11, f], [2, 76/12, f]).
step_up([2, 76/20, f], [2, 76/21, f]).
step_up([2, 76/21, f], [2, 76/22, f]).
step_up([2, 76/22, f], [2, 76/23, f]).
step_up([2, 76/23, f], [2, 76/24, f]).
step_up([2, 76/26, f], [2, 76/27, f]).
step_up([2, 76/27, f], [2, 76/28, f]).
step_up([2, 76/30, f], [2, 76/31, f]).
step_up([2, 76/31, f], [2, 76/32, f]).
step_up([2, 76/38, f], [2, 76/39, f]).
step_up([2, 76/39, f], [2, 76/40, f]).
step_up([2, 76/46, f], [2, 76/47, f]).
step_up([2, 76/47, f], [2, 76/48, f]).
step_up([2, 76/48, f], [2, 76/49, f]).
step_up([2, 76/49, f], [2, 76/50, f]).
step_up([2, 76/58, f], [2, 76/59, f]).
step_up([2, 76/59, f], [2, 76/60, f]).
step_up([2, 76/64, f], [2, 76/65, f]).
step_up([2, 76/65, f], [2, 76/66, f]).
step_up([2, 76/70, f], [2, 76/71, f]).
step_up([2, 76/71, f], [2, 76/72, f]).
step_up([2, 76/72, f], [2, 76/73, f]).
step_up([2, 76/73, f], [2, 76/74, f]).
step_up([2, 76/78, f], [2, 76/79, f]).
step_up([2, 76/79, f], [2, 76/80, f]).
step_up([2, 76/80, f], [2, 76/81, f]).
step_up([2, 76/81, f], [2, 76/82, f]).
step_up([2, 76/88, f], [2, 76/89, f]).
step_up([2, 76/89, f], [2, 76/90, f]).
step_up([2, 76/96, f], [2, 76/97, f]).
step_up([2, 76/97, f], [2, 76/98, f]).
step_up([2, 76/98, f], [2, 76/99, f]).
step_up([2, 76/99, f], [2, 76/100, f]).
step_up([2, 76/102, f], [2, 76/103, f]).
step_up([2, 76/103, f], [2, 76/104, f]).
step_up([2, 76/104, f], [2, 76/105, f]).
step_up([2, 76/105, f], [2, 76/106, f]).
step_up([2, 76/106, f], [2, 76/107, f]).
step_up([2, 76/107, f], [2, 76/108, f]).
step_up([2, 76/108, f], [2, 76/109, f]).
step_up([2, 76/109, f], [2, 76/110, f]).
step_up([2, 76/112, f], [2, 76/113, f]).
step_up([2, 76/113, f], [2, 76/114, f]).
step_up([2, 76/116, f], [2, 76/117, f]).
step_up([2, 76/117, f], [2, 76/118, f]).
step_up([2, 78/0, f], [2, 78/1, f]).
step_up([2, 78/1, f], [2, 78/2, f]).
step_up([2, 78/2, f], [2, 78/3, f]).
step_up([2, 78/3, f], [2, 78/4, f]).
step_up([2, 78/4, f], [2, 78/5, f]).
step_up([2, 78/5, f], [2, 78/6, f]).
step_up([2, 78/8, f], [2, 78/9, f]).
step_up([2, 78/9, f], [2, 78/10, f]).
step_up([2, 78/10, f], [2, 78/11, f]).
step_up([2, 78/11, f], [2, 78/12, f]).
step_up([2, 78/14, f], [2, 78/15, f]).
step_up([2, 78/15, f], [2, 78/16, f]).
step_up([2, 78/18, f], [2, 78/19, f]).
step_up([2, 78/19, f], [2, 78/20, f]).
step_up([2, 78/20, f], [2, 78/21, f]).
step_up([2, 78/21, f], [2, 78/22, f]).
step_up([2, 78/24, f], [2, 78/25, f]).
step_up([2, 78/25, f], [2, 78/26, f]).
step_up([2, 78/28, f], [2, 78/29, f]).
step_up([2, 78/29, f], [2, 78/30, f]).
step_up([2, 78/32, f], [2, 78/33, f]).
step_up([2, 78/33, f], [2, 78/34, f]).
step_up([2, 78/40, f], [2, 78/41, f]).
step_up([2, 78/41, f], [2, 78/42, f]).
step_up([2, 78/42, f], [2, 78/43, f]).
step_up([2, 78/43, f], [2, 78/44, f]).
step_up([2, 78/46, f], [2, 78/47, f]).
step_up([2, 78/47, f], [2, 78/48, f]).
step_up([2, 78/48, f], [2, 78/49, f]).
step_up([2, 78/49, f], [2, 78/50, f]).
step_up([2, 78/50, f], [2, 78/51, f]).
step_up([2, 78/51, f], [2, 78/52, f]).
step_up([2, 78/56, f], [2, 78/57, f]).
step_up([2, 78/57, f], [2, 78/58, f]).
step_up([2, 78/58, f], [2, 78/59, f]).
step_up([2, 78/59, f], [2, 78/60, f]).
step_up([2, 78/62, f], [2, 78/63, f]).
step_up([2, 78/63, f], [2, 78/64, f]).
step_up([2, 78/64, f], [2, 78/65, f]).
step_up([2, 78/65, f], [2, 78/66, f]).
step_up([2, 78/68, f], [2, 78/69, f]).
step_up([2, 78/69, f], [2, 78/70, f]).
step_up([2, 78/76, f], [2, 78/77, f]).
step_up([2, 78/77, f], [2, 78/78, f]).
step_up([2, 78/82, f], [2, 78/83, f]).
step_up([2, 78/83, f], [2, 78/84, f]).
step_up([2, 78/88, f], [2, 78/89, f]).
step_up([2, 78/89, f], [2, 78/90, f]).
step_up([2, 78/92, f], [2, 78/93, f]).
step_up([2, 78/93, f], [2, 78/94, f]).
step_up([2, 78/96, f], [2, 78/97, f]).
step_up([2, 78/97, f], [2, 78/98, f]).
step_up([2, 78/100, f], [2, 78/101, f]).
step_up([2, 78/101, f], [2, 78/102, f]).
step_up([2, 78/106, f], [2, 78/107, f]).
step_up([2, 78/107, f], [2, 78/108, f]).
step_up([2, 78/110, f], [2, 78/111, f]).
step_up([2, 78/111, f], [2, 78/112, f]).
step_up([2, 78/114, f], [2, 78/115, f]).
step_up([2, 78/115, f], [2, 78/116, f]).
step_up([2, 78/116, f], [2, 78/117, f]).
step_up([2, 78/117, f], [2, 78/118, f]).
step_up([2, 80/2, f], [2, 80/3, f]).
step_up([2, 80/3, f], [2, 80/4, f]).
step_up([2, 80/4, f], [2, 80/5, f]).
step_up([2, 80/5, f], [2, 80/6, f]).
step_up([2, 80/8, f], [2, 80/9, f]).
step_up([2, 80/9, f], [2, 80/10, f]).
step_up([2, 80/10, f], [2, 80/11, f]).
step_up([2, 80/11, f], [2, 80/12, f]).
step_up([2, 80/14, f], [2, 80/15, f]).
step_up([2, 80/15, f], [2, 80/16, f]).
step_up([2, 80/18, f], [2, 80/19, f]).
step_up([2, 80/19, f], [2, 80/20, f]).
step_up([2, 80/22, f], [2, 80/23, f]).
step_up([2, 80/23, f], [2, 80/24, f]).
step_up([2, 80/24, f], [2, 80/25, f]).
step_up([2, 80/25, f], [2, 80/26, f]).
step_up([2, 80/30, f], [2, 80/31, f]).
step_up([2, 80/31, f], [2, 80/32, f]).
step_up([2, 80/34, f], [2, 80/35, f]).
step_up([2, 80/35, f], [2, 80/36, f]).
step_up([2, 80/36, f], [2, 80/37, f]).
step_up([2, 80/37, f], [2, 80/38, f]).
step_up([2, 80/40, f], [2, 80/41, f]).
step_up([2, 80/41, f], [2, 80/42, f]).
step_up([2, 80/42, f], [2, 80/43, f]).
step_up([2, 80/43, f], [2, 80/44, f]).
step_up([2, 80/44, f], [2, 80/45, f]).
step_up([2, 80/45, f], [2, 80/46, f]).
step_up([2, 80/46, f], [2, 80/47, f]).
step_up([2, 80/47, f], [2, 80/48, f]).
step_up([2, 80/50, f], [2, 80/51, f]).
step_up([2, 80/51, f], [2, 80/52, f]).
step_up([2, 80/54, f], [2, 80/55, f]).
step_up([2, 80/55, f], [2, 80/56, f]).
step_up([2, 80/56, f], [2, 80/57, f]).
step_up([2, 80/57, f], [2, 80/58, f]).
step_up([2, 80/60, f], [2, 80/61, f]).
step_up([2, 80/61, f], [2, 80/62, f]).
step_up([2, 80/62, f], [2, 80/63, f]).
step_up([2, 80/63, f], [2, 80/64, f]).
step_up([2, 80/66, f], [2, 80/67, f]).
step_up([2, 80/67, f], [2, 80/68, f]).
step_up([2, 80/68, f], [2, 80/69, f]).
step_up([2, 80/69, f], [2, 80/70, f]).
step_up([2, 80/70, f], [2, 80/71, f]).
step_up([2, 80/71, f], [2, 80/72, f]).
step_up([2, 80/78, f], [2, 80/79, f]).
step_up([2, 80/79, f], [2, 80/80, f]).
step_up([2, 80/82, f], [2, 80/83, f]).
step_up([2, 80/83, f], [2, 80/84, f]).
step_up([2, 80/84, f], [2, 80/85, f]).
step_up([2, 80/85, f], [2, 80/86, f]).
step_up([2, 80/86, f], [2, 80/87, f]).
step_up([2, 80/87, f], [2, 80/88, f]).
step_up([2, 80/88, f], [2, 80/89, f]).
step_up([2, 80/89, f], [2, 80/90, f]).
step_up([2, 80/94, f], [2, 80/95, f]).
step_up([2, 80/95, f], [2, 80/96, f]).
step_up([2, 80/96, f], [2, 80/97, f]).
step_up([2, 80/97, f], [2, 80/98, f]).
step_up([2, 80/100, f], [2, 80/101, f]).
step_up([2, 80/101, f], [2, 80/102, f]).
step_up([2, 80/110, f], [2, 80/111, f]).
step_up([2, 80/111, f], [2, 80/112, f]).
step_up([2, 80/114, f], [2, 80/115, f]).
step_up([2, 80/115, f], [2, 80/116, f]).
step_up([2, 82/0, f], [2, 82/1, f]).
step_up([2, 82/1, f], [2, 82/2, f]).
step_up([2, 82/2, f], [2, 82/3, f]).
step_up([2, 82/3, f], [2, 82/4, f]).
step_up([2, 82/4, f], [2, 82/5, f]).
step_up([2, 82/5, f], [2, 82/6, f]).
step_up([2, 82/6, f], [2, 82/7, f]).
step_up([2, 82/7, f], [2, 82/8, f]).
step_up([2, 82/10, f], [2, 82/11, f]).
step_up([2, 82/11, f], [2, 82/12, f]).
step_up([2, 82/12, f], [2, 82/13, f]).
step_up([2, 82/13, f], [2, 82/14, f]).
step_up([2, 82/18, f], [2, 82/19, f]).
step_up([2, 82/19, f], [2, 82/20, f]).
step_up([2, 82/22, f], [2, 82/23, f]).
step_up([2, 82/23, f], [2, 82/24, f]).
step_up([2, 82/26, f], [2, 82/27, f]).
step_up([2, 82/27, f], [2, 82/28, f]).
step_up([2, 82/32, f], [2, 82/33, f]).
step_up([2, 82/33, f], [2, 82/34, f]).
step_up([2, 82/36, f], [2, 82/37, f]).
step_up([2, 82/37, f], [2, 82/38, f]).
step_up([2, 82/38, f], [2, 82/39, f]).
step_up([2, 82/39, f], [2, 82/40, f]).
step_up([2, 82/42, f], [2, 82/43, f]).
step_up([2, 82/43, f], [2, 82/44, f]).
step_up([2, 82/44, f], [2, 82/45, f]).
step_up([2, 82/45, f], [2, 82/46, f]).
step_up([2, 82/46, f], [2, 82/47, f]).
step_up([2, 82/47, f], [2, 82/48, f]).
step_up([2, 82/50, f], [2, 82/51, f]).
step_up([2, 82/51, f], [2, 82/52, f]).
step_up([2, 82/52, f], [2, 82/53, f]).
step_up([2, 82/53, f], [2, 82/54, f]).
step_up([2, 82/54, f], [2, 82/55, f]).
step_up([2, 82/55, f], [2, 82/56, f]).
step_up([2, 82/58, f], [2, 82/59, f]).
step_up([2, 82/59, f], [2, 82/60, f]).
step_up([2, 82/62, f], [2, 82/63, f]).
step_up([2, 82/63, f], [2, 82/64, f]).
step_up([2, 82/64, f], [2, 82/65, f]).
step_up([2, 82/65, f], [2, 82/66, f]).
step_up([2, 82/68, f], [2, 82/69, f]).
step_up([2, 82/69, f], [2, 82/70, f]).
step_up([2, 82/74, f], [2, 82/75, f]).
step_up([2, 82/75, f], [2, 82/76, f]).
step_up([2, 82/84, f], [2, 82/85, f]).
step_up([2, 82/85, f], [2, 82/86, f]).
step_up([2, 82/86, f], [2, 82/87, f]).
step_up([2, 82/87, f], [2, 82/88, f]).
step_up([2, 82/90, f], [2, 82/91, f]).
step_up([2, 82/91, f], [2, 82/92, f]).
step_up([2, 82/92, f], [2, 82/93, f]).
step_up([2, 82/93, f], [2, 82/94, f]).
step_up([2, 82/102, f], [2, 82/103, f]).
step_up([2, 82/103, f], [2, 82/104, f]).
step_up([2, 82/106, f], [2, 82/107, f]).
step_up([2, 82/107, f], [2, 82/108, f]).
step_up([2, 82/112, f], [2, 82/113, f]).
step_up([2, 82/113, f], [2, 82/114, f]).
step_up([2, 82/114, f], [2, 82/115, f]).
step_up([2, 82/115, f], [2, 82/116, f]).
step_up([2, 82/116, f], [2, 82/117, f]).
step_up([2, 82/117, f], [2, 82/118, f]).
step_up([2, 84/2, f], [2, 84/3, f]).
step_up([2, 84/3, f], [2, 84/4, f]).
step_up([2, 84/4, f], [2, 84/5, f]).
step_up([2, 84/5, f], [2, 84/6, f]).
step_up([2, 84/6, f], [2, 84/7, f]).
step_up([2, 84/7, f], [2, 84/8, f]).
step_up([2, 84/10, f], [2, 84/11, f]).
step_up([2, 84/11, f], [2, 84/12, f]).
step_up([2, 84/12, f], [2, 84/13, f]).
step_up([2, 84/13, f], [2, 84/14, f]).
step_up([2, 84/16, f], [2, 84/17, f]).
step_up([2, 84/17, f], [2, 84/18, f]).
step_up([2, 84/26, f], [2, 84/27, f]).
step_up([2, 84/27, f], [2, 84/28, f]).
step_up([2, 84/30, f], [2, 84/31, f]).
step_up([2, 84/31, f], [2, 84/32, f]).
step_up([2, 84/38, f], [2, 84/39, f]).
step_up([2, 84/39, f], [2, 84/40, f]).
step_up([2, 84/42, f], [2, 84/43, f]).
step_up([2, 84/43, f], [2, 84/44, f]).
step_up([2, 84/44, f], [2, 84/45, f]).
step_up([2, 84/45, f], [2, 84/46, f]).
step_up([2, 84/48, f], [2, 84/49, f]).
step_up([2, 84/49, f], [2, 84/50, f]).
step_up([2, 84/50, f], [2, 84/51, f]).
step_up([2, 84/51, f], [2, 84/52, f]).
step_up([2, 84/54, f], [2, 84/55, f]).
step_up([2, 84/55, f], [2, 84/56, f]).
step_up([2, 84/56, f], [2, 84/57, f]).
step_up([2, 84/57, f], [2, 84/58, f]).
step_up([2, 84/60, f], [2, 84/61, f]).
step_up([2, 84/61, f], [2, 84/62, f]).
step_up([2, 84/62, f], [2, 84/63, f]).
step_up([2, 84/63, f], [2, 84/64, f]).
step_up([2, 84/64, f], [2, 84/65, f]).
step_up([2, 84/65, f], [2, 84/66, f]).
step_up([2, 84/66, f], [2, 84/67, f]).
step_up([2, 84/67, f], [2, 84/68, f]).
step_up([2, 84/70, f], [2, 84/71, f]).
step_up([2, 84/71, f], [2, 84/72, f]).
step_up([2, 84/72, f], [2, 84/73, f]).
step_up([2, 84/73, f], [2, 84/74, f]).
step_up([2, 84/82, f], [2, 84/83, f]).
step_up([2, 84/83, f], [2, 84/84, f]).
step_up([2, 84/86, f], [2, 84/87, f]).
step_up([2, 84/87, f], [2, 84/88, f]).
step_up([2, 84/88, f], [2, 84/89, f]).
step_up([2, 84/89, f], [2, 84/90, f]).
step_up([2, 84/90, f], [2, 84/91, f]).
step_up([2, 84/91, f], [2, 84/92, f]).
step_up([2, 84/100, f], [2, 84/101, f]).
step_up([2, 84/101, f], [2, 84/102, f]).
step_up([2, 84/104, f], [2, 84/105, f]).
step_up([2, 84/105, f], [2, 84/106, f]).
step_up([2, 84/106, f], [2, 84/107, f]).
step_up([2, 84/107, f], [2, 84/108, f]).
step_up([2, 84/108, f], [2, 84/109, f]).
step_up([2, 84/109, f], [2, 84/110, f]).
step_up([2, 84/112, f], [2, 84/113, f]).
step_up([2, 84/113, f], [2, 84/114, f]).
step_up([2, 84/114, f], [2, 84/115, f]).
step_up([2, 84/115, f], [2, 84/116, f]).
step_up([2, 86/0, f], [2, 86/1, f]).
step_up([2, 86/1, f], [2, 86/2, f]).
step_up([2, 86/2, f], [2, 86/3, f]).
step_up([2, 86/3, f], [2, 86/4, f]).
step_up([2, 86/6, f], [2, 86/7, f]).
step_up([2, 86/7, f], [2, 86/8, f]).
step_up([2, 86/8, f], [2, 86/9, f]).
step_up([2, 86/9, f], [2, 86/10, f]).
step_up([2, 86/10, f], [2, 86/11, f]).
step_up([2, 86/11, f], [2, 86/12, f]).
step_up([2, 86/14, f], [2, 86/15, f]).
step_up([2, 86/15, f], [2, 86/16, f]).
step_up([2, 86/18, f], [2, 86/19, f]).
step_up([2, 86/19, f], [2, 86/20, f]).
step_up([2, 86/24, f], [2, 86/25, f]).
step_up([2, 86/25, f], [2, 86/26, f]).
step_up([2, 86/28, f], [2, 86/29, f]).
step_up([2, 86/29, f], [2, 86/30, f]).
step_up([2, 86/30, f], [2, 86/31, f]).
step_up([2, 86/31, f], [2, 86/32, f]).
step_up([2, 86/34, f], [2, 86/35, f]).
step_up([2, 86/35, f], [2, 86/36, f]).
step_up([2, 86/40, f], [2, 86/41, f]).
step_up([2, 86/41, f], [2, 86/42, f]).
step_up([2, 86/44, f], [2, 86/45, f]).
step_up([2, 86/45, f], [2, 86/46, f]).
step_up([2, 86/56, f], [2, 86/57, f]).
step_up([2, 86/57, f], [2, 86/58, f]).
step_up([2, 86/62, f], [2, 86/63, f]).
step_up([2, 86/63, f], [2, 86/64, f]).
step_up([2, 86/64, f], [2, 86/65, f]).
step_up([2, 86/65, f], [2, 86/66, f]).
step_up([2, 86/66, f], [2, 86/67, f]).
step_up([2, 86/67, f], [2, 86/68, f]).
step_up([2, 86/68, f], [2, 86/69, f]).
step_up([2, 86/69, f], [2, 86/70, f]).
step_up([2, 86/72, f], [2, 86/73, f]).
step_up([2, 86/73, f], [2, 86/74, f]).
step_up([2, 86/84, f], [2, 86/85, f]).
step_up([2, 86/85, f], [2, 86/86, f]).
step_up([2, 86/86, f], [2, 86/87, f]).
step_up([2, 86/87, f], [2, 86/88, f]).
step_up([2, 86/90, f], [2, 86/91, f]).
step_up([2, 86/91, f], [2, 86/92, f]).
step_up([2, 86/92, f], [2, 86/93, f]).
step_up([2, 86/93, f], [2, 86/94, f]).
step_up([2, 86/96, f], [2, 86/97, f]).
step_up([2, 86/97, f], [2, 86/98, f]).
step_up([2, 86/98, f], [2, 86/99, f]).
step_up([2, 86/99, f], [2, 86/100, f]).
step_up([2, 86/100, f], [2, 86/101, f]).
step_up([2, 86/101, f], [2, 86/102, f]).
step_up([2, 86/102, f], [2, 86/103, f]).
step_up([2, 86/103, f], [2, 86/104, f]).
step_up([2, 86/106, f], [2, 86/107, f]).
step_up([2, 86/107, f], [2, 86/108, f]).
step_up([2, 86/108, f], [2, 86/109, f]).
step_up([2, 86/109, f], [2, 86/110, f]).
step_up([2, 86/110, f], [2, 86/111, f]).
step_up([2, 86/111, f], [2, 86/112, f]).
step_up([2, 86/112, f], [2, 86/113, f]).
step_up([2, 86/113, f], [2, 86/114, f]).
step_up([2, 86/116, f], [2, 86/117, f]).
step_up([2, 86/117, f], [2, 86/118, f]).
step_up([2, 88/0, f], [2, 88/1, f]).
step_up([2, 88/1, f], [2, 88/2, f]).
step_up([2, 88/4, f], [2, 88/5, f]).
step_up([2, 88/5, f], [2, 88/6, f]).
step_up([2, 88/8, f], [2, 88/9, f]).
step_up([2, 88/9, f], [2, 88/10, f]).
step_up([2, 88/10, f], [2, 88/11, f]).
step_up([2, 88/11, f], [2, 88/12, f]).
step_up([2, 88/14, f], [2, 88/15, f]).
step_up([2, 88/15, f], [2, 88/16, f]).
step_up([2, 88/16, f], [2, 88/17, f]).
step_up([2, 88/17, f], [2, 88/18, f]).
step_up([2, 88/22, f], [2, 88/23, f]).
step_up([2, 88/23, f], [2, 88/24, f]).
step_up([2, 88/24, f], [2, 88/25, f]).
step_up([2, 88/25, f], [2, 88/26, f]).
step_up([2, 88/26, f], [2, 88/27, f]).
step_up([2, 88/27, f], [2, 88/28, f]).
step_up([2, 88/28, f], [2, 88/29, f]).
step_up([2, 88/29, f], [2, 88/30, f]).
step_up([2, 88/32, f], [2, 88/33, f]).
step_up([2, 88/33, f], [2, 88/34, f]).
step_up([2, 88/34, f], [2, 88/35, f]).
step_up([2, 88/35, f], [2, 88/36, f]).
step_up([2, 88/42, f], [2, 88/43, f]).
step_up([2, 88/43, f], [2, 88/44, f]).
step_up([2, 88/46, f], [2, 88/47, f]).
step_up([2, 88/47, f], [2, 88/48, f]).
step_up([2, 88/50, f], [2, 88/51, f]).
step_up([2, 88/51, f], [2, 88/52, f]).
step_up([2, 88/52, f], [2, 88/53, f]).
step_up([2, 88/53, f], [2, 88/54, f]).
step_up([2, 88/56, f], [2, 88/57, f]).
step_up([2, 88/57, f], [2, 88/58, f]).
step_up([2, 88/60, f], [2, 88/61, f]).
step_up([2, 88/61, f], [2, 88/62, f]).
step_up([2, 88/62, f], [2, 88/63, f]).
step_up([2, 88/63, f], [2, 88/64, f]).
step_up([2, 88/64, f], [2, 88/65, f]).
step_up([2, 88/65, f], [2, 88/66, f]).
step_up([2, 88/68, f], [2, 88/69, f]).
step_up([2, 88/69, f], [2, 88/70, f]).
step_up([2, 88/70, f], [2, 88/71, f]).
step_up([2, 88/71, f], [2, 88/72, f]).
step_up([2, 88/78, f], [2, 88/79, f]).
step_up([2, 88/79, f], [2, 88/80, f]).
step_up([2, 88/82, f], [2, 88/83, f]).
step_up([2, 88/83, f], [2, 88/84, f]).
step_up([2, 88/88, f], [2, 88/89, f]).
step_up([2, 88/89, f], [2, 88/90, f]).
step_up([2, 88/92, f], [2, 88/93, f]).
step_up([2, 88/93, f], [2, 88/94, f]).
step_up([2, 88/98, f], [2, 88/99, f]).
step_up([2, 88/99, f], [2, 88/100, f]).
step_up([2, 88/100, f], [2, 88/101, f]).
step_up([2, 88/101, f], [2, 88/102, f]).
step_up([2, 88/102, f], [2, 88/103, f]).
step_up([2, 88/103, f], [2, 88/104, f]).
step_up([2, 88/106, f], [2, 88/107, f]).
step_up([2, 88/107, f], [2, 88/108, f]).
step_up([2, 88/108, f], [2, 88/109, f]).
step_up([2, 88/109, f], [2, 88/110, f]).
step_up([2, 90/2, f], [2, 90/3, f]).
step_up([2, 90/3, f], [2, 90/4, f]).
step_up([2, 90/4, f], [2, 90/5, f]).
step_up([2, 90/5, f], [2, 90/6, f]).
step_up([2, 90/8, f], [2, 90/9, f]).
step_up([2, 90/9, f], [2, 90/10, f]).
step_up([2, 90/12, f], [2, 90/13, f]).
step_up([2, 90/13, f], [2, 90/14, f]).
step_up([2, 90/16, f], [2, 90/17, f]).
step_up([2, 90/17, f], [2, 90/18, f]).
step_up([2, 90/20, f], [2, 90/21, f]).
step_up([2, 90/21, f], [2, 90/22, f]).
step_up([2, 90/22, f], [2, 90/23, f]).
step_up([2, 90/23, f], [2, 90/24, f]).
step_up([2, 90/26, f], [2, 90/27, f]).
step_up([2, 90/27, f], [2, 90/28, f]).
step_up([2, 90/30, f], [2, 90/31, f]).
step_up([2, 90/31, f], [2, 90/32, f]).
step_up([2, 90/34, f], [2, 90/35, f]).
step_up([2, 90/35, f], [2, 90/36, f]).
step_up([2, 90/36, f], [2, 90/37, f]).
step_up([2, 90/37, f], [2, 90/38, f]).
step_up([2, 90/40, f], [2, 90/41, f]).
step_up([2, 90/41, f], [2, 90/42, f]).
step_up([2, 90/44, f], [2, 90/45, f]).
step_up([2, 90/45, f], [2, 90/46, f]).
step_up([2, 90/46, f], [2, 90/47, f]).
step_up([2, 90/47, f], [2, 90/48, f]).
step_up([2, 90/50, f], [2, 90/51, f]).
step_up([2, 90/51, f], [2, 90/52, f]).
step_up([2, 90/54, f], [2, 90/55, f]).
step_up([2, 90/55, f], [2, 90/56, f]).
step_up([2, 90/60, f], [2, 90/61, f]).
step_up([2, 90/61, f], [2, 90/62, f]).
step_up([2, 90/62, f], [2, 90/63, f]).
step_up([2, 90/63, f], [2, 90/64, f]).
step_up([2, 90/64, f], [2, 90/65, f]).
step_up([2, 90/65, f], [2, 90/66, f]).
step_up([2, 90/68, f], [2, 90/69, f]).
step_up([2, 90/69, f], [2, 90/70, f]).
step_up([2, 90/74, f], [2, 90/75, f]).
step_up([2, 90/75, f], [2, 90/76, f]).
step_up([2, 90/80, f], [2, 90/81, f]).
step_up([2, 90/81, f], [2, 90/82, f]).
step_up([2, 90/84, f], [2, 90/85, f]).
step_up([2, 90/85, f], [2, 90/86, f]).
step_up([2, 90/86, f], [2, 90/87, f]).
step_up([2, 90/87, f], [2, 90/88, f]).
step_up([2, 90/92, f], [2, 90/93, f]).
step_up([2, 90/93, f], [2, 90/94, f]).
step_up([2, 90/94, f], [2, 90/95, f]).
step_up([2, 90/95, f], [2, 90/96, f]).
step_up([2, 90/96, f], [2, 90/97, f]).
step_up([2, 90/97, f], [2, 90/98, f]).
step_up([2, 90/100, f], [2, 90/101, f]).
step_up([2, 90/101, f], [2, 90/102, f]).
step_up([2, 90/102, f], [2, 90/103, f]).
step_up([2, 90/103, f], [2, 90/104, f]).
step_up([2, 90/104, f], [2, 90/105, f]).
step_up([2, 90/105, f], [2, 90/106, f]).
step_up([2, 90/108, f], [2, 90/109, f]).
step_up([2, 90/109, f], [2, 90/110, f]).
step_up([2, 90/110, f], [2, 90/111, f]).
step_up([2, 90/111, f], [2, 90/112, f]).
step_up([2, 90/112, f], [2, 90/113, f]).
step_up([2, 90/113, f], [2, 90/114, f]).
step_up([2, 92/0, f], [2, 92/1, f]).
step_up([2, 92/1, f], [2, 92/2, f]).
step_up([2, 92/4, f], [2, 92/5, f]).
step_up([2, 92/5, f], [2, 92/6, f]).
step_up([2, 92/6, f], [2, 92/7, f]).
step_up([2, 92/7, f], [2, 92/8, f]).
step_up([2, 92/8, f], [2, 92/9, f]).
step_up([2, 92/9, f], [2, 92/10, f]).
step_up([2, 92/10, f], [2, 92/11, f]).
step_up([2, 92/11, f], [2, 92/12, f]).
step_up([2, 92/14, f], [2, 92/15, f]).
step_up([2, 92/15, f], [2, 92/16, f]).
step_up([2, 92/16, f], [2, 92/17, f]).
step_up([2, 92/17, f], [2, 92/18, f]).
step_up([2, 92/20, f], [2, 92/21, f]).
step_up([2, 92/21, f], [2, 92/22, f]).
step_up([2, 92/22, f], [2, 92/23, f]).
step_up([2, 92/23, f], [2, 92/24, f]).
step_up([2, 92/24, f], [2, 92/25, f]).
step_up([2, 92/25, f], [2, 92/26, f]).
step_up([2, 92/32, f], [2, 92/33, f]).
step_up([2, 92/33, f], [2, 92/34, f]).
step_up([2, 92/36, f], [2, 92/37, f]).
step_up([2, 92/37, f], [2, 92/38, f]).
step_up([2, 92/40, f], [2, 92/41, f]).
step_up([2, 92/41, f], [2, 92/42, f]).
step_up([2, 92/46, f], [2, 92/47, f]).
step_up([2, 92/47, f], [2, 92/48, f]).
step_up([2, 92/48, f], [2, 92/49, f]).
step_up([2, 92/49, f], [2, 92/50, f]).
step_up([2, 92/54, f], [2, 92/55, f]).
step_up([2, 92/55, f], [2, 92/56, f]).
step_up([2, 92/58, f], [2, 92/59, f]).
step_up([2, 92/59, f], [2, 92/60, f]).
step_up([2, 92/62, f], [2, 92/63, f]).
step_up([2, 92/63, f], [2, 92/64, f]).
step_up([2, 92/66, f], [2, 92/67, f]).
step_up([2, 92/67, f], [2, 92/68, f]).
step_up([2, 92/70, f], [2, 92/71, f]).
step_up([2, 92/71, f], [2, 92/72, f]).
step_up([2, 92/74, f], [2, 92/75, f]).
step_up([2, 92/75, f], [2, 92/76, f]).
step_up([2, 92/76, f], [2, 92/77, f]).
step_up([2, 92/77, f], [2, 92/78, f]).
step_up([2, 92/82, f], [2, 92/83, f]).
step_up([2, 92/83, f], [2, 92/84, f]).
step_up([2, 92/84, f], [2, 92/85, f]).
step_up([2, 92/85, f], [2, 92/86, f]).
step_up([2, 92/90, f], [2, 92/91, f]).
step_up([2, 92/91, f], [2, 92/92, f]).
step_up([2, 92/92, f], [2, 92/93, f]).
step_up([2, 92/93, f], [2, 92/94, f]).
step_up([2, 92/96, f], [2, 92/97, f]).
step_up([2, 92/97, f], [2, 92/98, f]).
step_up([2, 92/100, f], [2, 92/101, f]).
step_up([2, 92/101, f], [2, 92/102, f]).
step_up([2, 92/106, f], [2, 92/107, f]).
step_up([2, 92/107, f], [2, 92/108, f]).
step_up([2, 92/108, f], [2, 92/109, f]).
step_up([2, 92/109, f], [2, 92/110, f]).
step_up([2, 92/112, f], [2, 92/113, f]).
step_up([2, 92/113, f], [2, 92/114, f]).
step_up([2, 92/114, f], [2, 92/115, f]).
step_up([2, 92/115, f], [2, 92/116, f]).
step_up([2, 92/116, f], [2, 92/117, f]).
step_up([2, 92/117, f], [2, 92/118, f]).
step_up([2, 94/0, f], [2, 94/1, f]).
step_up([2, 94/1, f], [2, 94/2, f]).
step_up([2, 94/2, f], [2, 94/3, f]).
step_up([2, 94/3, f], [2, 94/4, f]).
step_up([2, 94/6, f], [2, 94/7, f]).
step_up([2, 94/7, f], [2, 94/8, f]).
step_up([2, 94/12, f], [2, 94/13, f]).
step_up([2, 94/13, f], [2, 94/14, f]).
step_up([2, 94/16, f], [2, 94/17, f]).
step_up([2, 94/17, f], [2, 94/18, f]).
step_up([2, 94/24, f], [2, 94/25, f]).
step_up([2, 94/25, f], [2, 94/26, f]).
step_up([2, 94/26, f], [2, 94/27, f]).
step_up([2, 94/27, f], [2, 94/28, f]).
step_up([2, 94/28, f], [2, 94/29, f]).
step_up([2, 94/29, f], [2, 94/30, f]).
step_up([2, 94/32, f], [2, 94/33, f]).
step_up([2, 94/33, f], [2, 94/34, f]).
step_up([2, 94/38, f], [2, 94/39, f]).
step_up([2, 94/39, f], [2, 94/40, f]).
step_up([2, 94/40, f], [2, 94/41, f]).
step_up([2, 94/41, f], [2, 94/42, f]).
step_up([2, 94/46, f], [2, 94/47, f]).
step_up([2, 94/47, f], [2, 94/48, f]).
step_up([2, 94/48, f], [2, 94/49, f]).
step_up([2, 94/49, f], [2, 94/50, f]).
step_up([2, 94/50, f], [2, 94/51, f]).
step_up([2, 94/51, f], [2, 94/52, f]).
step_up([2, 94/54, f], [2, 94/55, f]).
step_up([2, 94/55, f], [2, 94/56, f]).
step_up([2, 94/56, f], [2, 94/57, f]).
step_up([2, 94/57, f], [2, 94/58, f]).
step_up([2, 94/58, f], [2, 94/59, f]).
step_up([2, 94/59, f], [2, 94/60, f]).
step_up([2, 94/62, f], [2, 94/63, f]).
step_up([2, 94/63, f], [2, 94/64, f]).
step_up([2, 94/64, f], [2, 94/65, f]).
step_up([2, 94/65, f], [2, 94/66, f]).
step_up([2, 94/66, f], [2, 94/67, f]).
step_up([2, 94/67, f], [2, 94/68, f]).
step_up([2, 94/70, f], [2, 94/71, f]).
step_up([2, 94/71, f], [2, 94/72, f]).
step_up([2, 94/74, f], [2, 94/75, f]).
step_up([2, 94/75, f], [2, 94/76, f]).
step_up([2, 94/80, f], [2, 94/81, f]).
step_up([2, 94/81, f], [2, 94/82, f]).
step_up([2, 94/84, f], [2, 94/85, f]).
step_up([2, 94/85, f], [2, 94/86, f]).
step_up([2, 94/88, f], [2, 94/89, f]).
step_up([2, 94/89, f], [2, 94/90, f]).
step_up([2, 94/90, f], [2, 94/91, f]).
step_up([2, 94/91, f], [2, 94/92, f]).
step_up([2, 94/92, f], [2, 94/93, f]).
step_up([2, 94/93, f], [2, 94/94, f]).
step_up([2, 94/98, f], [2, 94/99, f]).
step_up([2, 94/99, f], [2, 94/100, f]).
step_up([2, 94/102, f], [2, 94/103, f]).
step_up([2, 94/103, f], [2, 94/104, f]).
step_up([2, 94/108, f], [2, 94/109, f]).
step_up([2, 94/109, f], [2, 94/110, f]).
step_up([2, 94/112, f], [2, 94/113, f]).
step_up([2, 94/113, f], [2, 94/114, f]).
step_up([2, 94/114, f], [2, 94/115, f]).
step_up([2, 94/115, f], [2, 94/116, f]).
step_up([2, 96/0, f], [2, 96/1, f]).
step_up([2, 96/1, f], [2, 96/2, f]).
step_up([2, 96/2, f], [2, 96/3, f]).
step_up([2, 96/3, f], [2, 96/4, f]).
step_up([2, 96/8, f], [2, 96/9, f]).
step_up([2, 96/9, f], [2, 96/10, f]).
step_up([2, 96/12, f], [2, 96/13, f]).
step_up([2, 96/13, f], [2, 96/14, f]).
step_up([2, 96/18, f], [2, 96/19, f]).
step_up([2, 96/19, f], [2, 96/20, f]).
step_up([2, 96/26, f], [2, 96/27, f]).
step_up([2, 96/27, f], [2, 96/28, f]).
step_up([2, 96/28, f], [2, 96/29, f]).
step_up([2, 96/29, f], [2, 96/30, f]).
step_up([2, 96/30, f], [2, 96/31, f]).
step_up([2, 96/31, f], [2, 96/32, f]).
step_up([2, 96/36, f], [2, 96/37, f]).
step_up([2, 96/37, f], [2, 96/38, f]).
step_up([2, 96/38, f], [2, 96/39, f]).
step_up([2, 96/39, f], [2, 96/40, f]).
step_up([2, 96/42, f], [2, 96/43, f]).
step_up([2, 96/43, f], [2, 96/44, f]).
step_up([2, 96/44, f], [2, 96/45, f]).
step_up([2, 96/45, f], [2, 96/46, f]).
step_up([2, 96/48, f], [2, 96/49, f]).
step_up([2, 96/49, f], [2, 96/50, f]).
step_up([2, 96/50, f], [2, 96/51, f]).
step_up([2, 96/51, f], [2, 96/52, f]).
step_up([2, 96/58, f], [2, 96/59, f]).
step_up([2, 96/59, f], [2, 96/60, f]).
step_up([2, 96/66, f], [2, 96/67, f]).
step_up([2, 96/67, f], [2, 96/68, f]).
step_up([2, 96/72, f], [2, 96/73, f]).
step_up([2, 96/73, f], [2, 96/74, f]).
step_up([2, 96/80, f], [2, 96/81, f]).
step_up([2, 96/81, f], [2, 96/82, f]).
step_up([2, 96/86, f], [2, 96/87, f]).
step_up([2, 96/87, f], [2, 96/88, f]).
step_up([2, 96/90, f], [2, 96/91, f]).
step_up([2, 96/91, f], [2, 96/92, f]).
step_up([2, 96/92, f], [2, 96/93, f]).
step_up([2, 96/93, f], [2, 96/94, f]).
step_up([2, 96/96, f], [2, 96/97, f]).
step_up([2, 96/97, f], [2, 96/98, f]).
step_up([2, 96/100, f], [2, 96/101, f]).
step_up([2, 96/101, f], [2, 96/102, f]).
step_up([2, 96/104, f], [2, 96/105, f]).
step_up([2, 96/105, f], [2, 96/106, f]).
step_up([2, 96/110, f], [2, 96/111, f]).
step_up([2, 96/111, f], [2, 96/112, f]).
step_up([2, 96/114, f], [2, 96/115, f]).
step_up([2, 96/115, f], [2, 96/116, f]).
step_up([2, 98/4, f], [2, 98/5, f]).
step_up([2, 98/5, f], [2, 98/6, f]).
step_up([2, 98/8, f], [2, 98/9, f]).
step_up([2, 98/9, f], [2, 98/10, f]).
step_up([2, 98/10, f], [2, 98/11, f]).
step_up([2, 98/11, f], [2, 98/12, f]).
step_up([2, 98/20, f], [2, 98/21, f]).
step_up([2, 98/21, f], [2, 98/22, f]).
step_up([2, 98/30, f], [2, 98/31, f]).
step_up([2, 98/31, f], [2, 98/32, f]).
step_up([2, 98/34, f], [2, 98/35, f]).
step_up([2, 98/35, f], [2, 98/36, f]).
step_up([2, 98/36, f], [2, 98/37, f]).
step_up([2, 98/37, f], [2, 98/38, f]).
step_up([2, 98/44, f], [2, 98/45, f]).
step_up([2, 98/45, f], [2, 98/46, f]).
step_up([2, 98/52, f], [2, 98/53, f]).
step_up([2, 98/53, f], [2, 98/54, f]).
step_up([2, 98/58, f], [2, 98/59, f]).
step_up([2, 98/59, f], [2, 98/60, f]).
step_up([2, 98/62, f], [2, 98/63, f]).
step_up([2, 98/63, f], [2, 98/64, f]).
step_up([2, 98/68, f], [2, 98/69, f]).
step_up([2, 98/69, f], [2, 98/70, f]).
step_up([2, 98/74, f], [2, 98/75, f]).
step_up([2, 98/75, f], [2, 98/76, f]).
step_up([2, 98/76, f], [2, 98/77, f]).
step_up([2, 98/77, f], [2, 98/78, f]).
step_up([2, 98/80, f], [2, 98/81, f]).
step_up([2, 98/81, f], [2, 98/82, f]).
step_up([2, 98/88, f], [2, 98/89, f]).
step_up([2, 98/89, f], [2, 98/90, f]).
step_up([2, 98/90, f], [2, 98/91, f]).
step_up([2, 98/91, f], [2, 98/92, f]).
step_up([2, 98/94, f], [2, 98/95, f]).
step_up([2, 98/95, f], [2, 98/96, f]).
step_up([2, 98/96, f], [2, 98/97, f]).
step_up([2, 98/97, f], [2, 98/98, f]).
step_up([2, 98/100, f], [2, 98/101, f]).
step_up([2, 98/101, f], [2, 98/102, f]).
step_up([2, 98/102, f], [2, 98/103, f]).
step_up([2, 98/103, f], [2, 98/104, f]).
step_up([2, 98/104, f], [2, 98/105, f]).
step_up([2, 98/105, f], [2, 98/106, f]).
step_up([2, 98/110, f], [2, 98/111, f]).
step_up([2, 98/111, f], [2, 98/112, f]).
step_up([2, 100/2, f], [2, 100/3, f]).
step_up([2, 100/3, f], [2, 100/4, f]).
step_up([2, 100/4, f], [2, 100/5, f]).
step_up([2, 100/5, f], [2, 100/6, f]).
step_up([2, 100/6, f], [2, 100/7, f]).
step_up([2, 100/7, f], [2, 100/8, f]).
step_up([2, 100/8, f], [2, 100/9, f]).
step_up([2, 100/9, f], [2, 100/10, f]).
step_up([2, 100/16, f], [2, 100/17, f]).
step_up([2, 100/17, f], [2, 100/18, f]).
step_up([2, 100/30, f], [2, 100/31, f]).
step_up([2, 100/31, f], [2, 100/32, f]).
step_up([2, 100/32, f], [2, 100/33, f]).
step_up([2, 100/33, f], [2, 100/34, f]).
step_up([2, 100/34, f], [2, 100/35, f]).
step_up([2, 100/35, f], [2, 100/36, f]).
step_up([2, 100/40, f], [2, 100/41, f]).
step_up([2, 100/41, f], [2, 100/42, f]).
step_up([2, 100/46, f], [2, 100/47, f]).
step_up([2, 100/47, f], [2, 100/48, f]).
step_up([2, 100/50, f], [2, 100/51, f]).
step_up([2, 100/51, f], [2, 100/52, f]).
step_up([2, 100/56, f], [2, 100/57, f]).
step_up([2, 100/57, f], [2, 100/58, f]).
step_up([2, 100/62, f], [2, 100/63, f]).
step_up([2, 100/63, f], [2, 100/64, f]).
step_up([2, 100/66, f], [2, 100/67, f]).
step_up([2, 100/67, f], [2, 100/68, f]).
step_up([2, 100/70, f], [2, 100/71, f]).
step_up([2, 100/71, f], [2, 100/72, f]).
step_up([2, 100/72, f], [2, 100/73, f]).
step_up([2, 100/73, f], [2, 100/74, f]).
step_up([2, 100/74, f], [2, 100/75, f]).
step_up([2, 100/75, f], [2, 100/76, f]).
step_up([2, 100/76, f], [2, 100/77, f]).
step_up([2, 100/77, f], [2, 100/78, f]).
step_up([2, 100/78, f], [2, 100/79, f]).
step_up([2, 100/79, f], [2, 100/80, f]).
step_up([2, 100/80, f], [2, 100/81, f]).
step_up([2, 100/81, f], [2, 100/82, f]).
step_up([2, 100/82, f], [2, 100/83, f]).
step_up([2, 100/83, f], [2, 100/84, f]).
step_up([2, 100/90, f], [2, 100/91, f]).
step_up([2, 100/91, f], [2, 100/92, f]).
step_up([2, 100/94, f], [2, 100/95, f]).
step_up([2, 100/95, f], [2, 100/96, f]).
step_up([2, 100/96, f], [2, 100/97, f]).
step_up([2, 100/97, f], [2, 100/98, f]).
step_up([2, 100/100, f], [2, 100/101, f]).
step_up([2, 100/101, f], [2, 100/102, f]).
step_up([2, 100/108, f], [2, 100/109, f]).
step_up([2, 100/109, f], [2, 100/110, f]).
step_up([2, 100/114, f], [2, 100/115, f]).
step_up([2, 100/115, f], [2, 100/116, f]).
step_up([2, 100/116, f], [2, 100/117, f]).
step_up([2, 100/117, f], [2, 100/118, f]).
step_up([2, 102/2, f], [2, 102/3, f]).
step_up([2, 102/3, f], [2, 102/4, f]).
step_up([2, 102/6, f], [2, 102/7, f]).
step_up([2, 102/7, f], [2, 102/8, f]).
step_up([2, 102/8, f], [2, 102/9, f]).
step_up([2, 102/9, f], [2, 102/10, f]).
step_up([2, 102/10, f], [2, 102/11, f]).
step_up([2, 102/11, f], [2, 102/12, f]).
step_up([2, 102/12, f], [2, 102/13, f]).
step_up([2, 102/13, f], [2, 102/14, f]).
step_up([2, 102/16, f], [2, 102/17, f]).
step_up([2, 102/17, f], [2, 102/18, f]).
step_up([2, 102/18, f], [2, 102/19, f]).
step_up([2, 102/19, f], [2, 102/20, f]).
step_up([2, 102/20, f], [2, 102/21, f]).
step_up([2, 102/21, f], [2, 102/22, f]).
step_up([2, 102/24, f], [2, 102/25, f]).
step_up([2, 102/25, f], [2, 102/26, f]).
step_up([2, 102/28, f], [2, 102/29, f]).
step_up([2, 102/29, f], [2, 102/30, f]).
step_up([2, 102/30, f], [2, 102/31, f]).
step_up([2, 102/31, f], [2, 102/32, f]).
step_up([2, 102/32, f], [2, 102/33, f]).
step_up([2, 102/33, f], [2, 102/34, f]).
step_up([2, 102/36, f], [2, 102/37, f]).
step_up([2, 102/37, f], [2, 102/38, f]).
step_up([2, 102/38, f], [2, 102/39, f]).
step_up([2, 102/39, f], [2, 102/40, f]).
step_up([2, 102/40, f], [2, 102/41, f]).
step_up([2, 102/41, f], [2, 102/42, f]).
step_up([2, 102/44, f], [2, 102/45, f]).
step_up([2, 102/45, f], [2, 102/46, f]).
step_up([2, 102/50, f], [2, 102/51, f]).
step_up([2, 102/51, f], [2, 102/52, f]).
step_up([2, 102/56, f], [2, 102/57, f]).
step_up([2, 102/57, f], [2, 102/58, f]).
step_up([2, 102/64, f], [2, 102/65, f]).
step_up([2, 102/65, f], [2, 102/66, f]).
step_up([2, 102/66, f], [2, 102/67, f]).
step_up([2, 102/67, f], [2, 102/68, f]).
step_up([2, 102/70, f], [2, 102/71, f]).
step_up([2, 102/71, f], [2, 102/72, f]).
step_up([2, 102/72, f], [2, 102/73, f]).
step_up([2, 102/73, f], [2, 102/74, f]).
step_up([2, 102/74, f], [2, 102/75, f]).
step_up([2, 102/75, f], [2, 102/76, f]).
step_up([2, 102/76, f], [2, 102/77, f]).
step_up([2, 102/77, f], [2, 102/78, f]).
step_up([2, 102/80, f], [2, 102/81, f]).
step_up([2, 102/81, f], [2, 102/82, f]).
step_up([2, 102/86, f], [2, 102/87, f]).
step_up([2, 102/87, f], [2, 102/88, f]).
step_up([2, 102/94, f], [2, 102/95, f]).
step_up([2, 102/95, f], [2, 102/96, f]).
step_up([2, 102/96, f], [2, 102/97, f]).
step_up([2, 102/97, f], [2, 102/98, f]).
step_up([2, 102/98, f], [2, 102/99, f]).
step_up([2, 102/99, f], [2, 102/100, f]).
step_up([2, 102/100, f], [2, 102/101, f]).
step_up([2, 102/101, f], [2, 102/102, f]).
step_up([2, 102/102, f], [2, 102/103, f]).
step_up([2, 102/103, f], [2, 102/104, f]).
step_up([2, 102/106, f], [2, 102/107, f]).
step_up([2, 102/107, f], [2, 102/108, f]).
step_up([2, 102/108, f], [2, 102/109, f]).
step_up([2, 102/109, f], [2, 102/110, f]).
step_up([2, 102/114, f], [2, 102/115, f]).
step_up([2, 102/115, f], [2, 102/116, f]).
step_up([2, 102/116, f], [2, 102/117, f]).
step_up([2, 102/117, f], [2, 102/118, f]).
step_up([2, 104/0, f], [2, 104/1, f]).
step_up([2, 104/1, f], [2, 104/2, f]).
step_up([2, 104/4, f], [2, 104/5, f]).
step_up([2, 104/5, f], [2, 104/6, f]).
step_up([2, 104/8, f], [2, 104/9, f]).
step_up([2, 104/9, f], [2, 104/10, f]).
step_up([2, 104/10, f], [2, 104/11, f]).
step_up([2, 104/11, f], [2, 104/12, f]).
step_up([2, 104/18, f], [2, 104/19, f]).
step_up([2, 104/19, f], [2, 104/20, f]).
step_up([2, 104/20, f], [2, 104/21, f]).
step_up([2, 104/21, f], [2, 104/22, f]).
step_up([2, 104/24, f], [2, 104/25, f]).
step_up([2, 104/25, f], [2, 104/26, f]).
step_up([2, 104/32, f], [2, 104/33, f]).
step_up([2, 104/33, f], [2, 104/34, f]).
step_up([2, 104/34, f], [2, 104/35, f]).
step_up([2, 104/35, f], [2, 104/36, f]).
step_up([2, 104/38, f], [2, 104/39, f]).
step_up([2, 104/39, f], [2, 104/40, f]).
step_up([2, 104/44, f], [2, 104/45, f]).
step_up([2, 104/45, f], [2, 104/46, f]).
step_up([2, 104/46, f], [2, 104/47, f]).
step_up([2, 104/47, f], [2, 104/48, f]).
step_up([2, 104/52, f], [2, 104/53, f]).
step_up([2, 104/53, f], [2, 104/54, f]).
step_up([2, 104/58, f], [2, 104/59, f]).
step_up([2, 104/59, f], [2, 104/60, f]).
step_up([2, 104/60, f], [2, 104/61, f]).
step_up([2, 104/61, f], [2, 104/62, f]).
step_up([2, 104/66, f], [2, 104/67, f]).
step_up([2, 104/67, f], [2, 104/68, f]).
step_up([2, 104/68, f], [2, 104/69, f]).
step_up([2, 104/69, f], [2, 104/70, f]).
step_up([2, 104/72, f], [2, 104/73, f]).
step_up([2, 104/73, f], [2, 104/74, f]).
step_up([2, 104/74, f], [2, 104/75, f]).
step_up([2, 104/75, f], [2, 104/76, f]).
step_up([2, 104/76, f], [2, 104/77, f]).
step_up([2, 104/77, f], [2, 104/78, f]).
step_up([2, 104/78, f], [2, 104/79, f]).
step_up([2, 104/79, f], [2, 104/80, f]).
step_up([2, 104/86, f], [2, 104/87, f]).
step_up([2, 104/87, f], [2, 104/88, f]).
step_up([2, 104/88, f], [2, 104/89, f]).
step_up([2, 104/89, f], [2, 104/90, f]).
step_up([2, 104/94, f], [2, 104/95, f]).
step_up([2, 104/95, f], [2, 104/96, f]).
step_up([2, 104/96, f], [2, 104/97, f]).
step_up([2, 104/97, f], [2, 104/98, f]).
step_up([2, 104/100, f], [2, 104/101, f]).
step_up([2, 104/101, f], [2, 104/102, f]).
step_up([2, 104/104, f], [2, 104/105, f]).
step_up([2, 104/105, f], [2, 104/106, f]).
step_up([2, 104/108, f], [2, 104/109, f]).
step_up([2, 104/109, f], [2, 104/110, f]).
step_up([2, 104/112, f], [2, 104/113, f]).
step_up([2, 104/113, f], [2, 104/114, f]).
step_up([2, 104/114, f], [2, 104/115, f]).
step_up([2, 104/115, f], [2, 104/116, f]).
step_up([2, 106/2, f], [2, 106/3, f]).
step_up([2, 106/3, f], [2, 106/4, f]).
step_up([2, 106/6, f], [2, 106/7, f]).
step_up([2, 106/7, f], [2, 106/8, f]).
step_up([2, 106/8, f], [2, 106/9, f]).
step_up([2, 106/9, f], [2, 106/10, f]).
step_up([2, 106/10, f], [2, 106/11, f]).
step_up([2, 106/11, f], [2, 106/12, f]).
step_up([2, 106/20, f], [2, 106/21, f]).
step_up([2, 106/21, f], [2, 106/22, f]).
step_up([2, 106/22, f], [2, 106/23, f]).
step_up([2, 106/23, f], [2, 106/24, f]).
step_up([2, 106/26, f], [2, 106/27, f]).
step_up([2, 106/27, f], [2, 106/28, f]).
step_up([2, 106/32, f], [2, 106/33, f]).
step_up([2, 106/33, f], [2, 106/34, f]).
step_up([2, 106/40, f], [2, 106/41, f]).
step_up([2, 106/41, f], [2, 106/42, f]).
step_up([2, 106/48, f], [2, 106/49, f]).
step_up([2, 106/49, f], [2, 106/50, f]).
step_up([2, 106/52, f], [2, 106/53, f]).
step_up([2, 106/53, f], [2, 106/54, f]).
step_up([2, 106/60, f], [2, 106/61, f]).
step_up([2, 106/61, f], [2, 106/62, f]).
step_up([2, 106/62, f], [2, 106/63, f]).
step_up([2, 106/63, f], [2, 106/64, f]).
step_up([2, 106/68, f], [2, 106/69, f]).
step_up([2, 106/69, f], [2, 106/70, f]).
step_up([2, 106/70, f], [2, 106/71, f]).
step_up([2, 106/71, f], [2, 106/72, f]).
step_up([2, 106/74, f], [2, 106/75, f]).
step_up([2, 106/75, f], [2, 106/76, f]).
step_up([2, 106/76, f], [2, 106/77, f]).
step_up([2, 106/77, f], [2, 106/78, f]).
step_up([2, 106/78, f], [2, 106/79, f]).
step_up([2, 106/79, f], [2, 106/80, f]).
step_up([2, 106/88, f], [2, 106/89, f]).
step_up([2, 106/89, f], [2, 106/90, f]).
step_up([2, 106/92, f], [2, 106/93, f]).
step_up([2, 106/93, f], [2, 106/94, f]).
step_up([2, 106/96, f], [2, 106/97, f]).
step_up([2, 106/97, f], [2, 106/98, f]).
step_up([2, 106/102, f], [2, 106/103, f]).
step_up([2, 106/103, f], [2, 106/104, f]).
step_up([2, 106/106, f], [2, 106/107, f]).
step_up([2, 106/107, f], [2, 106/108, f]).
step_up([2, 106/108, f], [2, 106/109, f]).
step_up([2, 106/109, f], [2, 106/110, f]).
step_up([2, 106/110, f], [2, 106/111, f]).
step_up([2, 106/111, f], [2, 106/112, f]).
step_up([2, 106/112, f], [2, 106/113, f]).
step_up([2, 106/113, f], [2, 106/114, f]).
step_up([2, 108/8, f], [2, 108/9, f]).
step_up([2, 108/9, f], [2, 108/10, f]).
step_up([2, 108/18, f], [2, 108/19, f]).
step_up([2, 108/19, f], [2, 108/20, f]).
step_up([2, 108/20, f], [2, 108/21, f]).
step_up([2, 108/21, f], [2, 108/22, f]).
step_up([2, 108/22, f], [2, 108/23, f]).
step_up([2, 108/23, f], [2, 108/24, f]).
step_up([2, 108/28, f], [2, 108/29, f]).
step_up([2, 108/29, f], [2, 108/30, f]).
step_up([2, 108/34, f], [2, 108/35, f]).
step_up([2, 108/35, f], [2, 108/36, f]).
step_up([2, 108/40, f], [2, 108/41, f]).
step_up([2, 108/41, f], [2, 108/42, f]).
step_up([2, 108/42, f], [2, 108/43, f]).
step_up([2, 108/43, f], [2, 108/44, f]).
step_up([2, 108/48, f], [2, 108/49, f]).
step_up([2, 108/49, f], [2, 108/50, f]).
step_up([2, 108/52, f], [2, 108/53, f]).
step_up([2, 108/53, f], [2, 108/54, f]).
step_up([2, 108/54, f], [2, 108/55, f]).
step_up([2, 108/55, f], [2, 108/56, f]).
step_up([2, 108/58, f], [2, 108/59, f]).
step_up([2, 108/59, f], [2, 108/60, f]).
step_up([2, 108/60, f], [2, 108/61, f]).
step_up([2, 108/61, f], [2, 108/62, f]).
step_up([2, 108/68, f], [2, 108/69, f]).
step_up([2, 108/69, f], [2, 108/70, f]).
step_up([2, 108/70, f], [2, 108/71, f]).
step_up([2, 108/71, f], [2, 108/72, f]).
step_up([2, 108/78, f], [2, 108/79, f]).
step_up([2, 108/79, f], [2, 108/80, f]).
step_up([2, 108/84, f], [2, 108/85, f]).
step_up([2, 108/85, f], [2, 108/86, f]).
step_up([2, 108/88, f], [2, 108/89, f]).
step_up([2, 108/89, f], [2, 108/90, f]).
step_up([2, 108/98, f], [2, 108/99, f]).
step_up([2, 108/99, f], [2, 108/100, f]).
step_up([2, 108/102, f], [2, 108/103, f]).
step_up([2, 108/103, f], [2, 108/104, f]).
step_up([2, 108/108, f], [2, 108/109, f]).
step_up([2, 108/109, f], [2, 108/110, f]).
step_up([2, 108/110, f], [2, 108/111, f]).
step_up([2, 108/111, f], [2, 108/112, f]).
step_up([2, 108/112, f], [2, 108/113, f]).
step_up([2, 108/113, f], [2, 108/114, f]).
step_up([2, 108/114, f], [2, 108/115, f]).
step_up([2, 108/115, f], [2, 108/116, f]).
step_up([2, 110/4, f], [2, 110/5, f]).
step_up([2, 110/5, f], [2, 110/6, f]).
step_up([2, 110/6, f], [2, 110/7, f]).
step_up([2, 110/7, f], [2, 110/8, f]).
step_up([2, 110/8, f], [2, 110/9, f]).
step_up([2, 110/9, f], [2, 110/10, f]).
step_up([2, 110/12, f], [2, 110/13, f]).
step_up([2, 110/13, f], [2, 110/14, f]).
step_up([2, 110/16, f], [2, 110/17, f]).
step_up([2, 110/17, f], [2, 110/18, f]).
step_up([2, 110/18, f], [2, 110/19, f]).
step_up([2, 110/19, f], [2, 110/20, f]).
step_up([2, 110/20, f], [2, 110/21, f]).
step_up([2, 110/21, f], [2, 110/22, f]).
step_up([2, 110/26, f], [2, 110/27, f]).
step_up([2, 110/27, f], [2, 110/28, f]).
step_up([2, 110/32, f], [2, 110/33, f]).
step_up([2, 110/33, f], [2, 110/34, f]).
step_up([2, 110/36, f], [2, 110/37, f]).
step_up([2, 110/37, f], [2, 110/38, f]).
step_up([2, 110/40, f], [2, 110/41, f]).
step_up([2, 110/41, f], [2, 110/42, f]).
step_up([2, 110/42, f], [2, 110/43, f]).
step_up([2, 110/43, f], [2, 110/44, f]).
step_up([2, 110/44, f], [2, 110/45, f]).
step_up([2, 110/45, f], [2, 110/46, f]).
step_up([2, 110/54, f], [2, 110/55, f]).
step_up([2, 110/55, f], [2, 110/56, f]).
step_up([2, 110/56, f], [2, 110/57, f]).
step_up([2, 110/57, f], [2, 110/58, f]).
step_up([2, 110/58, f], [2, 110/59, f]).
step_up([2, 110/59, f], [2, 110/60, f]).
step_up([2, 110/60, f], [2, 110/61, f]).
step_up([2, 110/61, f], [2, 110/62, f]).
step_up([2, 110/62, f], [2, 110/63, f]).
step_up([2, 110/63, f], [2, 110/64, f]).
step_up([2, 110/66, f], [2, 110/67, f]).
step_up([2, 110/67, f], [2, 110/68, f]).
step_up([2, 110/68, f], [2, 110/69, f]).
step_up([2, 110/69, f], [2, 110/70, f]).
step_up([2, 110/74, f], [2, 110/75, f]).
step_up([2, 110/75, f], [2, 110/76, f]).
step_up([2, 110/80, f], [2, 110/81, f]).
step_up([2, 110/81, f], [2, 110/82, f]).
step_up([2, 110/84, f], [2, 110/85, f]).
step_up([2, 110/85, f], [2, 110/86, f]).
step_up([2, 110/90, f], [2, 110/91, f]).
step_up([2, 110/91, f], [2, 110/92, f]).
step_up([2, 110/104, f], [2, 110/105, f]).
step_up([2, 110/105, f], [2, 110/106, f]).
step_up([2, 110/110, f], [2, 110/111, f]).
step_up([2, 110/111, f], [2, 110/112, f]).
step_up([2, 110/112, f], [2, 110/113, f]).
step_up([2, 110/113, f], [2, 110/114, f]).
step_up([2, 110/114, f], [2, 110/115, f]).
step_up([2, 110/115, f], [2, 110/116, f]).
step_up([2, 112/2, f], [2, 112/3, f]).
step_up([2, 112/3, f], [2, 112/4, f]).
step_up([2, 112/4, f], [2, 112/5, f]).
step_up([2, 112/5, f], [2, 112/6, f]).
step_up([2, 112/6, f], [2, 112/7, f]).
step_up([2, 112/7, f], [2, 112/8, f]).
step_up([2, 112/8, f], [2, 112/9, f]).
step_up([2, 112/9, f], [2, 112/10, f]).
step_up([2, 112/10, f], [2, 112/11, f]).
step_up([2, 112/11, f], [2, 112/12, f]).
step_up([2, 112/14, f], [2, 112/15, f]).
step_up([2, 112/15, f], [2, 112/16, f]).
step_up([2, 112/18, f], [2, 112/19, f]).
step_up([2, 112/19, f], [2, 112/20, f]).
step_up([2, 112/24, f], [2, 112/25, f]).
step_up([2, 112/25, f], [2, 112/26, f]).
step_up([2, 112/30, f], [2, 112/31, f]).
step_up([2, 112/31, f], [2, 112/32, f]).
step_up([2, 112/38, f], [2, 112/39, f]).
step_up([2, 112/39, f], [2, 112/40, f]).
step_up([2, 112/42, f], [2, 112/43, f]).
step_up([2, 112/43, f], [2, 112/44, f]).
step_up([2, 112/44, f], [2, 112/45, f]).
step_up([2, 112/45, f], [2, 112/46, f]).
step_up([2, 112/46, f], [2, 112/47, f]).
step_up([2, 112/47, f], [2, 112/48, f]).
step_up([2, 112/50, f], [2, 112/51, f]).
step_up([2, 112/51, f], [2, 112/52, f]).
step_up([2, 112/56, f], [2, 112/57, f]).
step_up([2, 112/57, f], [2, 112/58, f]).
step_up([2, 112/58, f], [2, 112/59, f]).
step_up([2, 112/59, f], [2, 112/60, f]).
step_up([2, 112/60, f], [2, 112/61, f]).
step_up([2, 112/61, f], [2, 112/62, f]).
step_up([2, 112/62, f], [2, 112/63, f]).
step_up([2, 112/63, f], [2, 112/64, f]).
step_up([2, 112/68, f], [2, 112/69, f]).
step_up([2, 112/69, f], [2, 112/70, f]).
step_up([2, 112/72, f], [2, 112/73, f]).
step_up([2, 112/73, f], [2, 112/74, f]).
step_up([2, 112/76, f], [2, 112/77, f]).
step_up([2, 112/77, f], [2, 112/78, f]).
step_up([2, 112/78, f], [2, 112/79, f]).
step_up([2, 112/79, f], [2, 112/80, f]).
step_up([2, 112/80, f], [2, 112/81, f]).
step_up([2, 112/81, f], [2, 112/82, f]).
step_up([2, 112/82, f], [2, 112/83, f]).
step_up([2, 112/83, f], [2, 112/84, f]).
step_up([2, 112/88, f], [2, 112/89, f]).
step_up([2, 112/89, f], [2, 112/90, f]).
step_up([2, 112/90, f], [2, 112/91, f]).
step_up([2, 112/91, f], [2, 112/92, f]).
step_up([2, 112/94, f], [2, 112/95, f]).
step_up([2, 112/95, f], [2, 112/96, f]).
step_up([2, 112/100, f], [2, 112/101, f]).
step_up([2, 112/101, f], [2, 112/102, f]).
step_up([2, 112/108, f], [2, 112/109, f]).
step_up([2, 112/109, f], [2, 112/110, f]).
step_up([2, 112/110, f], [2, 112/111, f]).
step_up([2, 112/111, f], [2, 112/112, f]).
step_up([2, 112/112, f], [2, 112/113, f]).
step_up([2, 112/113, f], [2, 112/114, f]).
step_up([2, 114/4, f], [2, 114/5, f]).
step_up([2, 114/5, f], [2, 114/6, f]).
step_up([2, 114/10, f], [2, 114/11, f]).
step_up([2, 114/11, f], [2, 114/12, f]).
step_up([2, 114/14, f], [2, 114/15, f]).
step_up([2, 114/15, f], [2, 114/16, f]).
step_up([2, 114/22, f], [2, 114/23, f]).
step_up([2, 114/23, f], [2, 114/24, f]).
step_up([2, 114/28, f], [2, 114/29, f]).
step_up([2, 114/29, f], [2, 114/30, f]).
step_up([2, 114/36, f], [2, 114/37, f]).
step_up([2, 114/37, f], [2, 114/38, f]).
step_up([2, 114/40, f], [2, 114/41, f]).
step_up([2, 114/41, f], [2, 114/42, f]).
step_up([2, 114/46, f], [2, 114/47, f]).
step_up([2, 114/47, f], [2, 114/48, f]).
step_up([2, 114/52, f], [2, 114/53, f]).
step_up([2, 114/53, f], [2, 114/54, f]).
step_up([2, 114/60, f], [2, 114/61, f]).
step_up([2, 114/61, f], [2, 114/62, f]).
step_up([2, 114/62, f], [2, 114/63, f]).
step_up([2, 114/63, f], [2, 114/64, f]).
step_up([2, 114/64, f], [2, 114/65, f]).
step_up([2, 114/65, f], [2, 114/66, f]).
step_up([2, 114/72, f], [2, 114/73, f]).
step_up([2, 114/73, f], [2, 114/74, f]).
step_up([2, 114/76, f], [2, 114/77, f]).
step_up([2, 114/77, f], [2, 114/78, f]).
step_up([2, 114/78, f], [2, 114/79, f]).
step_up([2, 114/79, f], [2, 114/80, f]).
step_up([2, 114/82, f], [2, 114/83, f]).
step_up([2, 114/83, f], [2, 114/84, f]).
step_up([2, 114/86, f], [2, 114/87, f]).
step_up([2, 114/87, f], [2, 114/88, f]).
step_up([2, 114/90, f], [2, 114/91, f]).
step_up([2, 114/91, f], [2, 114/92, f]).
step_up([2, 114/94, f], [2, 114/95, f]).
step_up([2, 114/95, f], [2, 114/96, f]).
step_up([2, 114/96, f], [2, 114/97, f]).
step_up([2, 114/97, f], [2, 114/98, f]).
step_up([2, 114/100, f], [2, 114/101, f]).
step_up([2, 114/101, f], [2, 114/102, f]).
step_up([2, 114/104, f], [2, 114/105, f]).
step_up([2, 114/105, f], [2, 114/106, f]).
step_up([2, 114/106, f], [2, 114/107, f]).
step_up([2, 114/107, f], [2, 114/108, f]).
step_up([2, 114/110, f], [2, 114/111, f]).
step_up([2, 114/111, f], [2, 114/112, f]).
step_up([2, 114/116, f], [2, 114/117, f]).
step_up([2, 114/117, f], [2, 114/118, f]).
step_up([2, 116/0, f], [2, 116/1, f]).
step_up([2, 116/1, f], [2, 116/2, f]).
step_up([2, 116/6, f], [2, 116/7, f]).
step_up([2, 116/7, f], [2, 116/8, f]).
step_up([2, 116/8, f], [2, 116/9, f]).
step_up([2, 116/9, f], [2, 116/10, f]).
step_up([2, 116/12, f], [2, 116/13, f]).
step_up([2, 116/13, f], [2, 116/14, f]).
step_up([2, 116/14, f], [2, 116/15, f]).
step_up([2, 116/15, f], [2, 116/16, f]).
step_up([2, 116/20, f], [2, 116/21, f]).
step_up([2, 116/21, f], [2, 116/22, f]).
step_up([2, 116/24, f], [2, 116/25, f]).
step_up([2, 116/25, f], [2, 116/26, f]).
step_up([2, 116/28, f], [2, 116/29, f]).
step_up([2, 116/29, f], [2, 116/30, f]).
step_up([2, 116/34, f], [2, 116/35, f]).
step_up([2, 116/35, f], [2, 116/36, f]).
step_up([2, 116/36, f], [2, 116/37, f]).
step_up([2, 116/37, f], [2, 116/38, f]).
step_up([2, 116/46, f], [2, 116/47, f]).
step_up([2, 116/47, f], [2, 116/48, f]).
step_up([2, 116/50, f], [2, 116/51, f]).
step_up([2, 116/51, f], [2, 116/52, f]).
step_up([2, 116/52, f], [2, 116/53, f]).
step_up([2, 116/53, f], [2, 116/54, f]).
step_up([2, 116/56, f], [2, 116/57, f]).
step_up([2, 116/57, f], [2, 116/58, f]).
step_up([2, 116/58, f], [2, 116/59, f]).
step_up([2, 116/59, f], [2, 116/60, f]).
step_up([2, 116/62, f], [2, 116/63, f]).
step_up([2, 116/63, f], [2, 116/64, f]).
step_up([2, 116/64, f], [2, 116/65, f]).
step_up([2, 116/65, f], [2, 116/66, f]).
step_up([2, 116/66, f], [2, 116/67, f]).
step_up([2, 116/67, f], [2, 116/68, f]).
step_up([2, 116/72, f], [2, 116/73, f]).
step_up([2, 116/73, f], [2, 116/74, f]).
step_up([2, 116/76, f], [2, 116/77, f]).
step_up([2, 116/77, f], [2, 116/78, f]).
step_up([2, 116/82, f], [2, 116/83, f]).
step_up([2, 116/83, f], [2, 116/84, f]).
step_up([2, 116/84, f], [2, 116/85, f]).
step_up([2, 116/85, f], [2, 116/86, f]).
step_up([2, 116/88, f], [2, 116/89, f]).
step_up([2, 116/89, f], [2, 116/90, f]).
step_up([2, 116/92, f], [2, 116/93, f]).
step_up([2, 116/93, f], [2, 116/94, f]).
step_up([2, 116/96, f], [2, 116/97, f]).
step_up([2, 116/97, f], [2, 116/98, f]).
step_up([2, 116/98, f], [2, 116/99, f]).
step_up([2, 116/99, f], [2, 116/100, f]).
step_up([2, 116/102, f], [2, 116/103, f]).
step_up([2, 116/103, f], [2, 116/104, f]).
step_up([2, 116/104, f], [2, 116/105, f]).
step_up([2, 116/105, f], [2, 116/106, f]).
step_up([2, 116/106, f], [2, 116/107, f]).
step_up([2, 116/107, f], [2, 116/108, f]).
step_up([2, 116/108, f], [2, 116/109, f]).
step_up([2, 116/109, f], [2, 116/110, f]).
step_up([2, 116/114, f], [2, 116/115, f]).
step_up([2, 116/115, f], [2, 116/116, f]).
step_up([2, 118/0, e], [2, 118/1, f]).
step_up([2, 118/1, f], [2, 118/2, f]).
step_up([2, 118/4, f], [2, 118/5, f]).
step_up([2, 118/5, f], [2, 118/6, f]).
step_up([2, 118/6, f], [2, 118/7, f]).
step_up([2, 118/7, f], [2, 118/8, f]).
step_up([2, 118/8, f], [2, 118/9, f]).
step_up([2, 118/9, f], [2, 118/10, f]).
step_up([2, 118/10, f], [2, 118/11, f]).
step_up([2, 118/11, f], [2, 118/12, f]).
step_up([2, 118/12, f], [2, 118/13, f]).
step_up([2, 118/13, f], [2, 118/14, f]).
step_up([2, 118/16, f], [2, 118/17, f]).
step_up([2, 118/17, f], [2, 118/18, f]).
step_up([2, 118/18, f], [2, 118/19, f]).
step_up([2, 118/19, f], [2, 118/20, f]).
step_up([2, 118/22, f], [2, 118/23, f]).
step_up([2, 118/23, f], [2, 118/24, f]).
step_up([2, 118/24, f], [2, 118/25, f]).
step_up([2, 118/25, f], [2, 118/26, f]).
step_up([2, 118/26, f], [2, 118/27, f]).
step_up([2, 118/27, f], [2, 118/28, f]).
step_up([2, 118/28, f], [2, 118/29, f]).
step_up([2, 118/29, f], [2, 118/30, f]).
step_up([2, 118/32, f], [2, 118/33, f]).
step_up([2, 118/33, f], [2, 118/34, f]).
step_up([2, 118/36, f], [2, 118/37, f]).
step_up([2, 118/37, f], [2, 118/38, f]).
step_up([2, 118/38, f], [2, 118/39, f]).
step_up([2, 118/39, f], [2, 118/40, f]).
step_up([2, 118/42, f], [2, 118/43, f]).
step_up([2, 118/43, f], [2, 118/44, f]).
step_up([2, 118/46, f], [2, 118/47, f]).
step_up([2, 118/47, f], [2, 118/48, f]).
step_up([2, 118/48, f], [2, 118/49, f]).
step_up([2, 118/49, f], [2, 118/50, f]).
step_up([2, 118/50, f], [2, 118/51, f]).
step_up([2, 118/51, f], [2, 118/52, f]).
step_up([2, 118/54, f], [2, 118/55, f]).
step_up([2, 118/55, f], [2, 118/56, f]).
step_up([2, 118/56, f], [2, 118/57, f]).
step_up([2, 118/57, f], [2, 118/58, f]).
step_up([2, 118/58, f], [2, 118/59, f]).
step_up([2, 118/59, f], [2, 118/60, f]).
step_up([2, 118/60, f], [2, 118/61, f]).
step_up([2, 118/61, f], [2, 118/62, f]).
step_up([2, 118/64, f], [2, 118/65, f]).
step_up([2, 118/65, f], [2, 118/66, f]).
step_up([2, 118/66, f], [2, 118/67, f]).
step_up([2, 118/67, f], [2, 118/68, f]).
step_up([2, 118/68, f], [2, 118/69, f]).
step_up([2, 118/69, f], [2, 118/70, f]).
step_up([2, 118/70, f], [2, 118/71, f]).
step_up([2, 118/71, f], [2, 118/72, f]).
step_up([2, 118/72, f], [2, 118/73, f]).
step_up([2, 118/73, f], [2, 118/74, f]).
step_up([2, 118/76, f], [2, 118/77, f]).
step_up([2, 118/77, f], [2, 118/78, f]).
step_up([2, 118/80, f], [2, 118/81, f]).
step_up([2, 118/81, f], [2, 118/82, f]).
step_up([2, 118/82, f], [2, 118/83, f]).
step_up([2, 118/83, f], [2, 118/84, f]).
step_up([2, 118/84, f], [2, 118/85, f]).
step_up([2, 118/85, f], [2, 118/86, f]).
step_up([2, 118/86, f], [2, 118/87, f]).
step_up([2, 118/87, f], [2, 118/88, f]).
step_up([2, 118/90, f], [2, 118/91, f]).
step_up([2, 118/91, f], [2, 118/92, f]).
step_up([2, 118/92, f], [2, 118/93, f]).
step_up([2, 118/93, f], [2, 118/94, f]).
step_up([2, 118/94, f], [2, 118/95, f]).
step_up([2, 118/95, f], [2, 118/96, f]).
step_up([2, 118/96, f], [2, 118/97, f]).
step_up([2, 118/97, f], [2, 118/98, f]).
step_up([2, 118/98, f], [2, 118/99, f]).
step_up([2, 118/99, f], [2, 118/100, f]).
step_up([2, 118/100, f], [2, 118/101, f]).
step_up([2, 118/101, f], [2, 118/102, f]).
step_up([2, 118/102, f], [2, 118/103, f]).
step_up([2, 118/103, f], [2, 118/104, f]).
step_up([2, 118/104, f], [2, 118/105, f]).
step_up([2, 118/105, f], [2, 118/106, f]).
step_up([2, 118/106, f], [2, 118/107, f]).
step_up([2, 118/107, f], [2, 118/108, f]).
step_up([2, 118/108, f], [2, 118/109, f]).
step_up([2, 118/109, f], [2, 118/110, f]).
step_up([2, 118/110, f], [2, 118/111, f]).
step_up([2, 118/111, f], [2, 118/112, f]).
step_up([2, 118/112, f], [2, 118/113, f]).
step_up([2, 118/113, f], [2, 118/114, f]).
step_up([2, 118/114, f], [2, 118/115, f]).
step_up([2, 118/115, f], [2, 118/116, f]).

