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
                     ]).

:-user:use_module(map_display).
:-reexport(actions).
% Load test script.
:-use_module(testing).

:- generator_configuration:action_representation(R)
   ,format('Loaded actions in ~w representation.~n',[R]).

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
        generator_configuration:action_representation(stack_based)
        ,Id = zero
        ,E = solve([Id,_Xs/_Ys,_T1,[]],[Id,_Xe/_Ye,_T2,_Vs]).
positive_example(solve/2, E):-
        generator_configuration:action_representation(memoryless)
        ,Id = zero
        ,E = solve([Id,_Xs/_Ys,_T1],[Id,_Xe/_Ye,_T2]).

negative_example(_,_):- false.
