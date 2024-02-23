:-module(empty_maze, [background_knowledge/2
                     ,metarules/2
                     ,positive_example/2
                     ,negative_example/2
                     ,step_down/2
                     ,step_left/2
                     ,step_right/2
                     ,step_up/2
                     ,move_down/2
                     ,move_left/2
                     ,move_right/2
                     ,move_up/2
                     ,test_run/3
                     ,testing_instance/3
                     ]).

:-user:use_module(move_generator).
:-user:use_module(map_display).
:- generator_configuration:primitives_file(P,_M)
  ,reexport(P).

/** <module> Learn a maze solving FSA.

__ Maze __

==
?- print_maze(tiles,zero).
f f
f f
true.
==

__ Configuration __

==
?- list_options([clause_limit/1,max_invented/1,fetch_clauses/1,table_meta_interpreter/1,untable_meta_interpreter/1]).
clause_limit(2)
max_invented(0)
fetch_clauses(all)
table_meta_interpreter(true)
untable_meta_interpreter(true)
true.
==


__ Learning Problem __

Encapsulated to show the structure of primitive steps.

==
?- list_encapsulated_problem(solve/2).
Positive examples
-----------------
m(solve,[zero,A/B,C,[]],[zero,D/E,F,G]).

Negative examples
-----------------

Background knowledge (First Order)
----------------------------------
move_down/2:
m(move_down,[A,B/C,D,[]],E):-step_down([A,B/C,D,[]],E).
m(move_down,[A,B/C,D,[E|F]],G):-E\==u,step_down([A,B/C,D,[E|F]],G).
step_down([zero,0/1,f,A],[zero,0/0,f,[d|A]]):-true.
step_down([zero,1/1,f,A],[zero,1/0,f,[d|A]]):-true.

move_left/2:
m(move_left,[A,B/C,D,[]],E):-step_left([A,B/C,D,[]],E).
m(move_left,[A,B/C,D,[E|F]],G):-E\==r,step_left([A,B/C,D,[E|F]],G).
step_left([zero,1/0,f,A],[zero,0/0,f,[l|A]]):-true.
step_left([zero,1/1,f,A],[zero,0/1,f,[l|A]]):-true.

move_right/2:
m(move_right,[A,B/C,D,[]],E):-step_right([A,B/C,D,[]],E).
m(move_right,[A,B/C,D,[E|F]],G):-E\==l,step_right([A,B/C,D,[E|F]],G).
step_right([zero,0/0,f,A],[zero,1/0,f,[r|A]]):-true.
step_right([zero,0/1,f,A],[zero,1/1,f,[r|A]]):-true.

move_up/2:
m(move_up,[A,B/C,D,[]],E):-step_up([A,B/C,D,[]],E).
m(move_up,[A,B/C,D,[E|F]],G):-E\==d,step_up([A,B/C,D,[E|F]],G).
step_up([zero,0/0,f,A],[zero,0/1,f,[u|A]]):-true.
step_up([zero,1/0,f,A],[zero,1/1,f,[u|A]]):-true.


Background knowledge (Second Order)
-----------------------------------
m(identity,P,Q):-m(P,X,Y),m(Q,X,Y)
m(tailrec,P,Q):-m(P,X,Y),m(Q,X,Z),m(P,Z,Y)
true.
==

Note the metasubstitution constraints (not printed with encapsulation):

==
Metasubstitution constraints
----------------------------
:- dynamic configuration:metarule_constraints/2.
:- multifile configuration:metarule_constraints/2.

configuration:metarule_constraints(m(identity, P, P), fail).
configuration:metarule_constraints(m(chain, _, Q, Q), fail).
configuration:metarule_constraints(m(chain, P, P, _), fail).
configuration:metarule_constraints(m(chain, _P, Q, R), fail) :-
    empty_maze:opposite(Q, R).
configuration:metarule_constraints(m(tailrec, P, P), fail).

true.
==

And note that only the first and last constraints apply, since we're
using tailrec.


__ Learnings __

==
?- time( learn(solve/2,_Ps) ), print_clauses(_Ps), length(_Ps,N).
% 86,642 inferences, 0.016 CPU in 0.017 seconds (90% CPU, 5545088 Lips)
solve(A,B):-move_down(A,B).
solve(A,B):-move_left(A,B).
solve(A,B):-move_right(A,B).
solve(A,B):-move_up(A,B).
solve(A,B):-move_down(A,C),solve(C,B).
solve(A,B):-move_left(A,C),solve(C,B).
solve(A,B):-move_right(A,C),solve(C,B).
solve(A,B):-move_up(A,C),solve(C,B).
N = 8.
==


__ Chain Variant __

==
?- list_options([clause_limit/1,max_invented/1,fetch_clauses/1,table_meta_interpreter/1,untable_meta_interpreter/1]).
clause_limit(2)
max_invented(0)
fetch_clauses(all)
table_meta_interpreter(true)
untable_meta_interpreter(true)
true.

?- list_problem_statistics(solve/2).
Positive examples:    1
Negative examples:    0
Background knowledge: 4 [move_down/2,move_left/2,move_right/2,move_up/2]
Metarules:            2 [identity,chain]
true.

?- time( learn(solve/2,_Ps) ), print_clauses(_Ps), length(_Ps,N).% 149,989 inferences, 0.000 CPU in 0.023 seconds (0% CPU, Infinite Lips)
solve(A,B):-move_down(A,B).
solve(A,B):-move_left(A,B).
solve(A,B):-move_right(A,B).
solve(A,B):-move_up(A,B).
solve(A,B):-move_down(A,C),move_left(C,B).
solve(A,B):-move_down(A,C),move_right(C,B).
solve(A,B):-move_down(A,C),solve(C,B).
solve(A,B):-move_left(A,C),move_down(C,B).
solve(A,B):-move_left(A,C),move_up(C,B).
solve(A,B):-move_left(A,C),solve(C,B).
solve(A,B):-move_right(A,C),move_down(C,B).
solve(A,B):-move_right(A,C),move_up(C,B).
solve(A,B):-move_right(A,C),solve(C,B).
solve(A,B):-move_up(A,C),move_left(C,B).
solve(A,B):-move_up(A,C),move_right(C,B).
solve(A,B):-move_up(A,C),solve(C,B).
N = 16.
==


__ Testings __

==

==

*/


% ========================================
% Testing predicates
% ========================================

%!      test_run(+Target,+What,-Steps) is det.
%
%       Test learned plan on each testing instance.
%
%       Target is the predicate indicator, Symbol/Arity, of a learning
%       target.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Steps is an integer, the number of steps taken to solve the
%       maze. This includes steps taken during backtracking.
%
%       Prints out each test maze and traces the path found through that
%       maze by the learned plan(ner).
%
%       Note that this tests learned plans deterministically: if
%       multiple plans can be generated, only one is traced.
%
test_run(T,W,N):-
        generator_configuration:primitives_file(_P,PM)
        ,debug(test_run)
        ,debug(trace_path)
        ,debug(test_run,'Learning program for target: ~w',[T])
        ,debug(test_run,'Training on maze \'~w\':',[zero])
        ,print_maze(W,zero)
        ,time( learn(T,Ps) )
        ,debug_clauses(test_run,'Learned program:',Ps)
        ,S = (assert_program(experiment_file,Ps,Rs)
             %,table(experiment_file:T)
             )
        ,debug(test_run,'Loading test mazes...',[])
        ,time( load_test_file )
        ,G = (findall(Id/Dims
                     ,(PM:maze(Id,Dims,_)
                      ,Id \== zero
                      )
                     ,Ids)
             ,length(Ids,I)
             ,debug(test_run,'Loaded ~w test mazes.',[I])
             ,member(Id/(Wd-Ht),Ids)
             ,debug(test_run,'Testing maze ~w map (~wx~w):',[Id,Wd,Ht])
             ,print_maze(W,Id)
             ,testing_instance(T,Id,E)
             ,trace_path_(W,Id,E,N)
             )
        ,C = (erase_program_clauses(Rs)
             %,untable(experiment_file:T)
             )
        ,setup_call_cleanup(S,G,C).


%!      load_test_file is det.
%
%       Load a primitives file for testing.
%
load_test_file:-
        generator_configuration:test_primitives_file(P,_M)
        ,load_files(P
                  ,[module(primitives)
                   ,redefine_module(true)
                   ]).


%!      trace_path_(+What,+Id,+Instance,-Steps) is det.
%
%       Helper for path tracing with informative messages.
%
trace_path_(W,Id,E,I):-
        %format('Finding a path...~n',[])
        trace_path(W,Id,E,C)
        ,arg(1,C,I)
        ,!.
trace_path_(_W,_Id,_E,_):-
        debug(trace_path,'Failed to find a path!~n~n',[]).



%!      testing_instance(+Target,+Id,-Example) is nondet.
%
%       Generate a testing instance.
%
%       Target is the predicate indicator, lfa/2 or lfg/2, of the
%       learning target for which a testing instance is to be created.
%
%       Id is the id of a maze.
%
%       Example is a testing example of Target, one for each loaded
%       maze/3 term.
%
testing_instance(T/2,Id,E):-
        Id \== zero
        ,generator_configuration:primitives_file(_P,M)
        ,M:maze(Id,Dims,Ms)
        ,once(start_location(Ms,Dims,Xs/Ys))
        ,once(end_location(Ms,Dims,Xe/Ye))
        ,E =.. [T,[Id,Xs/Ys,s,[]],[Id,Xe/Ye,e,_Vs]].


% ========================================
% Training data
% ========================================


:-nodebug(_).
%:-debug(learn).
%:-debug(top_program).
%:-debug(examples).
%:-debug(signature).
%:-debug(metasubstitutions).
%:-debug(prove_steps).
%:-debug(prove_metasubs).
%:-debug(fetch_length).

configuration:metarule_constraints(m(identity,P,P),fail).

configuration:metarule_constraints(m(chain,_,Q,Q),fail).
configuration:metarule_constraints(m(chain,P,P,_),fail).

configuration:metarule_constraints(m(chain,_P,Q,R),fail):-
        opposite(Q,R).

opposite(move_down,move_up).
opposite(move_up,move_down).
opposite(move_left,move_right).
opposite(move_right,move_left).

configuration:metarule_constraints(m(tailrec,P,P),fail).


:-auxiliaries:set_configuration_option(clause_limit,[2]).
:-auxiliaries:set_configuration_option(max_invented,[0]).
:-auxiliaries:set_configuration_option(reduction,[none]).
:-auxiliaries:set_configuration_option(fetch_clauses,[all]).
%:-auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-auxiliaries:set_configuration_option(table_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).

background_knowledge(solve/2, [move_down/2
                              ,move_left/2
                              ,move_right/2
                              ,move_up/2
                              ]).

%metarules(solve/2,[identity,chain]).
metarules(solve/2,[identity,tailrec]).

% Trace with:
% _T = solve/2, _Id = tessera, positive_example(_T,_E), trace_path(tiles,_Id,_E).
positive_example(solve/2, E):-
        Id = zero
        ,E = solve([Id,_X1/_Y1,_T1,[]],[Id,_X2/_Y2,_T2,_Vs]).


negative_example(_,_):- false.


% BK wrapping primitive moves to apply Markovian anti-oscillation
% constraint.

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
