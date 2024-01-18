:-module(maze_solver, [
    background_knowledge/2
    ,metarules/2
    ,positive_example/2
    ,negative_example/2
    ,move_up/2
    ,move_down/2
    ,move_left/2
    ,move_right/2
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

positive_example(ms/2, ms(s(Maze,[1,0],[]),s(Maze,[1,3],[d,d,d]))):-
    maze(0,Maze).

negative_example(ms/2,_):- false.

clear(Maze,[X,Y]):-
    number(X),
    number(Y),  
    is_list(Maze),
    nth0(Y,Maze,Row),
    nth0(X,Row,Cell),
    Cell =:= 1.

move_up(s(Maze,[X,Y],Moves),s(Maze,[X,Y1],[u|Moves])):-
    Y > 0,
    Y1 is Y - 1,
    clear(Maze,[X,Y1]).
    % writeln(Moves).
move_down(s(Maze,[X,Y],Moves),s(Maze,[X,Y1],[d|Moves])):-
    Y < 7,
    Y1 is Y + 1,
    clear(Maze,[X,Y1]).
    % writeln(Moves).

move_left(s(Maze,[X,Y],Moves),s(Maze,[X1,Y],[l|Moves])):-
    X > 0,
    X1 is X - 1,
    clear(Maze,[X1,Y]).
    % writeln('left').

move_right(s(Maze,[X,Y],Moves),s(Maze,[X1,Y],[r|Moves])):-
    X < 7,
    X1 is X + 1,
    clear(Maze,[X1,Y]).
    % writeln('right').


test:-
    maze(0,Maze),
    S = s(Maze,[1,0],[]),
    move_down(S,S1),
    move_down(S1,S2),
    writeln(S2).

% debug(learn),debug(prove_steps),debug(fetch),debug(prove_metasubs).