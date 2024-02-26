:-module(actions, [move_down/2
                  ,move_left/2
                  ,move_right/2
                  ,move_up/2
                  ]).

:-use_module(generator_configuration).
:-ensure_loaded(move_generator).
:-move_generator:write_primitives.
:- generator_configuration:primitives_file(P,_M)
  ,reexport(P).


/** <module> Library of actions.

*/

%!      move_down(+State1,-State2) is det.
%
%       Move the robot one step down.
%
%
:-if(action_representation(stack_based)).
move_down([Id,X/Y,T,[]],S2):-
    step_down([Id,X/Y,T,[]],S2).
move_down([Id,X/Y,T,[V|Vs]],S2):-
    V \== u,
    step_down([Id,X/Y,T,[V|Vs]],S2).
:-elif(action_representation(memoryless)).
move_down(S1,S2):-
    step_down(S1,S2).
:-endif.


%!      move_left(+State1,-State2) is det.
%
%       Move the robot one step left.
%
%
:-if(action_representation(stack_based)).
move_left([Id,X/Y,T,[]],S2):-
    step_left([Id,X/Y,T,[]],S2).
move_left([Id,X/Y,T,[V|Vs]],S2):-
    V \== r,
    step_left([Id,X/Y,T,[V|Vs]],S2).
:-elif(action_representation(memoryless)).
move_left(S1,S2):-
    step_left(S1,S2).
:-endif.


%!      move_right(+State1,-State2) is det.
%
%       Move the robot one step right.
%
%
:-if(action_representation(stack_based)).
move_right([Id,X/Y,T,[]],S2):-
    step_right([Id,X/Y,T,[]],S2).
move_right([Id,X/Y,T,[V|Vs]],S2):-
    V \== l,
    step_right([Id,X/Y,T,[V|Vs]],S2).
:-elif(action_representation(memoryless)).
move_right(S1,S2):-
    step_right(S1,S2).
:-endif.


%!      move_up(+State1,-State2) is det.
%
%       Move the robot one step up.
%
%
:-if(action_representation(stack_based)).
move_up([Id,X/Y,T,[]],S2):-
    step_up([Id,X/Y,T,[]],S2).
move_up([Id,X/Y,T,[V|Vs]],S2):-
    V \== d,
    step_up([Id,X/Y,T,[V|Vs]],S2).
:-elif(action_representation(memoryless)).
move_up(S1,S2):-
    step_up(S1,S2).
:-endif.
