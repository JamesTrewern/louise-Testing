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
                     ,test_run/2
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
m(solve,[zero,A/B,C],[zero,D/E,F]).

Negative examples
-----------------

Background knowledge (First Order)
----------------------------------
move_down/2:
m(move_down,A,B):-step_down(A,B).
step_down([zero,0/1,f],[zero,0/0,f]).
step_down([zero,1/1,f],[zero,1/0,f]).
step_down([0,0/1,f],[0,0/0,f]).
% ... 1025 more clauses.

move_left/2:
m(move_left,A,B):-step_left(A,B).
step_left([zero,1/0,f],[zero,0/0,f]).
step_left([zero,1/1,f],[zero,0/1,f]).
step_left([0,1/6,f],[0,0/6,f]).
% ... 953 more clauses.

move_right/2:
m(move_right,A,B):-step_right(A,B).
step_right([zero,0/0,f],[zero,1/0,f]).
step_right([zero,0/1,f],[zero,1/1,f]).
step_right([0,0/6,f],[0,1/6,f]).
% ... 953 more clauses.

move_up/2:
m(move_up,A,B):-step_up(A,B).
step_up([zero,0/0,f],[zero,0/1,f]).
step_up([zero,1/0,f],[zero,1/1,f]).
step_up([0,0/0,f],[0,0/1,f]).
% ... 1025 more clauses.


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
?- time( learn(solve/2) ).
solve(A,B):-move_down(A,B).
solve(A,B):-move_left(A,B).
solve(A,B):-move_right(A,B).
solve(A,B):-move_up(A,B).
solve(A,B):-move_down(A,C),solve(C,B).
solve(A,B):-move_left(A,C),solve(C,B).
solve(A,B):-move_right(A,C),solve(C,B).
solve(A,B):-move_up(A,C),solve(C,B).
% 85,319 inferences, 0.000 CPU in 0.020 seconds (0% CPU, Infinite Lips)
true.
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

?- time( learn(solve/2) ).
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
% 171,524 inferences, 0.016 CPU in 0.036 seconds (43% CPU, 10977536 Lips)
true.
==


__ Testings __

==

==

*/

% ========================================
% Testing predicates
% ========================================

solve(A,B):-move_down(A,B).
solve(A,B):-move_left(A,B).
solve(A,B):-move_right(A,B).
solve(A,B):-move_up(A,B).
solve(A,B):-move_down(A,C),solve(C,B).
solve(A,B):-move_left(A,C),solve(C,B).
solve(A,B):-move_right(A,C),solve(C,B).
solve(A,B):-move_up(A,C),solve(C,B).

%!      test_run(+Target,+What) is det.
%
%       Test learned plan on each testing instance.
%
%       Target is the predicate indicator, Symbol/Arity, of a learning
%       target.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Prints out each test maze and traces the path found through that
%       maze by the learned plan(ner).
%
%       Note that this tests learned plans deterministically: if
%       multiple plans can be generated, only one is traced.
%
test_run(T,W):-
        generator_configuration:primitives_file(_P,PM)
        ,format('Learning program for target: ~w~n',[T])
        ,time( learn(T,Ps) )
        ,print_clauses('Learned program:',Ps)
        ,S = (assert_program(experiment_file,Ps,Rs)
             %,table(experiment_file:T)
             )
        ,G = (findall(Id
                     ,PM:maze(Id,_,_)
                     ,Ids)
             ,member(Id,Ids)
             ,format('Maze ~w map:~n',[Id])
             ,print_maze(W,Id)
             ,testing_instance(T,Id,E)
             ,trace_path_(W,Id,E)
             )
        ,C = (erase_program_clauses(Rs)
             %,untable(experiment_file:T)
             )
        ,setup_call_cleanup(S,G,C).



%!      trace_path_(+Id,+Instance) is det.
%
%       Helper for path tracing with informative messages.
%
trace_path_(W,Id,E):-
        format('Finding a path...~n',[])
        ,time(trace_path(W,Id,E))
        ,!.
trace_path_(_W,_Id,_E):-
        format('Failed to find a path!~n~n',[]).



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
testing_instance(solve_ll/2,zero,E):-
        generator_configuration:primitives_file(_P,M)
        ,M:maze(zero,Dims,Ms)
        ,map_location(Xs/Ys,T1,Ms,Dims,true)
        ,map_location(Xe/Ye,T2,Ms,Dims,true)
        ,Xs/Ys \= Xe/Ye
        ,E =.. [solve_ll,[zero,Xs/Ys,T1,[]],[zero,Xe/Ye,T2,_Vs]].



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
