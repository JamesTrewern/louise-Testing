:-module(maze_solver, [
    background_knowledge/2
    ,metarules/2
    ,positive_example/2
    ,negative_example/2
    ,move_up/2
    ,move_down/2
    ,move_left/2
    ,move_right/2
    ,step_up/2
    ,step_down/2
    ,step_left/2
    ,step_right/2
    ,maze/2
    ,clear/2
]).

:- use_module(configuration).

:- ['Mazes/load_mazes'].

:- auxiliaries:set_configuration_option(clause_limit, [3]).
:- auxiliaries:set_configuration_option(max_invented, [0]).
% :- auxiliaries:set_configuration_option(fetch_clauses, [[builtins,bk,metarules]]).
:- auxiliaries:set_configuration_option(fetch_clauses, [all]).
:-auxiliaries:set_configuration_option(table_meta_interpreter, [true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter, [true]).

configuration:metarule_constraints(M,fail):- M =.. [m,_,P,P|_Ps].

background_knowledge(ms/2, [
    move_up/2,
    move_down/2,
    move_left/2,
    move_right/2
]).

metarules(ms/2,[chain,identity]).

positive_example(ms/2, ms(s(Maze,[1,0],[]),s(Maze,[5,6],[d,r,d]))):-
    maze(0,Maze).

negative_example(ms/2,_):- false.

clear(Maze,[X,Y]):-
    number(X),
    number(Y),  
    is_list(Maze),
    nth0(Y,Maze,Row),
    nth0(X,Row,Cell),
    Cell =:= 1.

step_up(s(Maze,[X,Y],M),s(Maze,[X,Y1],M)):-
    Y > 0,
    Y1 is Y - 1,
    clear(Maze,[X,Y1]).
    % writeln(Moves).
step_down(s(Maze,[X,Y],M),s(Maze,[X,Y1],M)):-
    Y < 7,
    Y1 is Y + 1,
    clear(Maze,[X,Y1]).
    % writeln(Moves).

step_left(s(Maze,[X,Y],M),s(Maze,[X1,Y],M)):-
    X > 0,
    X1 is X - 1,
    clear(Maze,[X1,Y]).
    % writeln('left').

step_right(s(Maze,[X,Y],M),s(Maze,[X1,Y],M)):-
    X < 7,
    X1 is X + 1,
    clear(Maze,[X1,Y]).
    % writeln('right').

move_up(S1,S3):-
    step_up(S1,S2),
    ((not(step_up(S2,_));step_left(S2,_);step_right(S2,_))->
        (
            s(Maze,P,Moves) = S2,
            S3 = s(Maze,P,[u|Moves])
        );
        (
            move_up(S2,S3)
        )
    ).

move_down(S1,S3):-
    step_down(S1,S2),
    ((not(step_down(S2,_));step_left(S2,_);step_right(S2,_))->
        (
            s(Maze,P,Moves) = S2,
            S3 = s(Maze,P,[d|Moves])
        );
        (
            move_down(S2,S3)
        )
    ).

move_left(S1,S3):-
    step_left(S1,S2),
    ((not(step_left(S2,_));step_up(S2,_);step_down(S2,_))->
        (
            s(Maze,P,Moves) = S2,
            S3 = s(Maze,P,[l|Moves])
        );
        (
            move_left(S2,S3)
        )
    ).

move_right(S1,S3):-
    step_right(S1,S2),
    ((not(step_right(S2,_));step_up(S2,_);step_down(S2,_))->
        (
            s(Maze,P,Moves) = S2,
            S3 = s(Maze,P,[r|Moves])
        );
        (
            move_right(S2,S3)
        )
    ).


test:-
    maze(0,Maze),
    S = s(Maze,[1,0],[]),
    move_down(S,S1),
    move_right(S1,S2),
    move_down(S2,s(_,P,M)),
    writeln(M),
    writeln(P).

% debug(learn),debug(prove_steps),debug(fetch),debug(prove_metasubs).