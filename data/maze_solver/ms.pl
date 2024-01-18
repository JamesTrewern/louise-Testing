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

:- auxiliaries:set_configuration_option(clause_limit, [4]).
:- auxiliaries:set_configuration_option(max_invented, [2]).
configuration:metarule_constraints(M,fail):- M =.. [m,_,P,P|_Ps].

background_knowledge(ms/2, [
    move_up/2,
    move_down/2,
    move_left/2,
    move_right/2
]).

metarules(ms/2,[chain,identity]).

positive_example(ms/2, ms(s(Maze,[0,1]),s(Maze,[12,11]))):-
    maze(0,Maze).

negative_example(ms/2,_):- false.

clear(Maze,[X,Y]):-
    nth0(Y,Maze,Row),
    nth0(X,Row,Cell),
    Cell =:= 1.

move_up(s(Maze,[X,Y]),s(Maze,[X,Y1])):-
    Y > 0,
    Y1 is Y - 1,
    clear(Maze,[X,Y1]).
move_down(s(Maze,[X,Y]),s(Maze,[X,Y1])):-
    Y < 7,
    Y1 is Y + 1,
    clear(Maze,[X,Y1]).

move_left(s(Maze,[X,Y]),s(Maze,[X1,Y])):-
    X > 0,
    X1 is X - 1,
    clear(Maze,[X1,Y]).

move_right(s(Maze,[X,Y]),s(Maze,[X1,Y])):-
    X < 7,
    X1 is X + 1,
    clear(Maze,[X1,Y]).

test:-
    maze(0,Maze),
    move_up(s(Maze,[1,0]),s(Maze,[X,Y])),
    writeln(X),
    writeln(Y).