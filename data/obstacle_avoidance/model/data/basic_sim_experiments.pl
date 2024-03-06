:-module(basic_sim_xp, [background_knowledge/2
                       ,metarules/2
                       ,positive_example/2
                       ,negative_example/2
                       ,document_choices/1
                       ,training_example/1
                       ,testing_example/1
                       ,run_experiments/0
                       ]).

:-user:ensure_loaded(data('obstacle_avoidance/model/load_headless')).
:-user:use_module(model_root(model_configuration)).
:-user:use_module(model_root(specification)).
:-user:use_module(model_root(src/actions)).
:-user:use_module(model_root(output/vector_access)).
:-user:use_module(model_root(src/controller)).
% To call write_primitives/0 at the top-level.
:-user:use_module(model_root(src/printing)).
:-user:use_module(model_root(src/state_vector)).
:-user:use_module(model_root(src/types)).
:-user:use_module(model_root(src/sim_parser)).
:-user:use_module(model_root(src/plan_exec)).

/** <module> Learn simple plans for the Bath robot Toy Problem.

This is an experiment file, to be used with Louise, to learn plans for
the Toy Problem simulation.

__ 1. Configuration, and stack and table limits __

==
?- [load_headless].
Global stack limit 2,147,483,648
Table space 8,589,934,592
true.

?- list_options([clause_limit/1,max_invented/1,reduction/1,fetch_clauses/1,table_meta_interpreter/1,untable_meta_interpreter/1]).
clause_limit(3)
max_invented(0)
reduction(none)
fetch_clauses(all)
table_meta_interpreter(true)
untable_meta_interpreter(true)
true.

?- _Os = [ training_episode(_), list_budget(action,_), map(_,_,_), state_vector_terms(_), stride(float,_) ], forall(member(O,_Os), experiment_file:call_and_write(O)).
training_episode(Episode4)

list_budget(action,200)

map(model_root(data/occ_grid_2_free),250,180)

state_vector_terms([destination,position,scan_area,action])

stride(float,10.0)

true.

?- listing(move_constraint).
model_configuration:move_constraint(in_scan_area/2).
model_configuration:move_constraint(passable/2).
model_configuration:move_constraint(moving_forward/2).

true.
==

__ 2. MIL Problem __

==
?- list_mil_problem(move/2).
Positive examples
-----------------
move([destination(x(70.0),y(80.0),theta(0.0)),position(x(30.0),y(60.0),theta(0.0)),scan_area(x_min(0),x_max(249),y_min(46),y_max(136))],[destination(x(70.0),y(80.0),theta(0.0)),position(x(70.0),y(80.0),theta(0.0)),scan_area(x_min(0),x_max(249),y_min(46),y_max(136))]).

Negative examples
-----------------

Background knowledge (First Order)
----------------------------------
move_north/2:
move_north(S1,S2):-move(move_north,'North',S1,false/increment,S2).

move_north_east/2:
move_north_east(S1,S2):-move(move_north_east,'North East',S1,increment/increment,S2).

move_east/2:
move_east(S1,S2):-move(move_east,'East',S1,increment/false,S2).

move_south_east/2:
move_south_east(S1,S2):-move(move_south_east,'South East',S1,increment/decrement,S2).

move_south/2:
move_south(S1,S2):-move(move_south,'South',S1,false/decrement,S2).

move_south_west/2:
move_south_west(S1,S2):-move(move_south_west,'South West',S1,decrement/decrement,S2).

move_west/2:
move_west(S1,S2):-move(move_west,'West',S1,decrement/false,S2).

move_north_west/2:
move_north_west(S1,S2):-move(move_north_west,'North West',S1,decrement/increment,S2).

Background knowledge(Second Order)
----------------------------------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

Metasubstitution constraints
----------------------------
:- dynamic configuration:metarule_constraints/2.
:- multifile configuration:metarule_constraints/2.

configuration:metarule_constraints(m(identity, P, P), fail).
configuration:metarule_constraints(m(chain, A, A, _), fail).
configuration:metarule_constraints(m(chain, P, P, P), fail).

true.
==

__ 3. Learning __

==
?- _T = move/2, time(learn(_T,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 22,395,712 inferences, 0.469 CPU in 3.867 seconds (12% CPU, 47777519 Lips)
move(A,B):-move_east(A,B).
move(A,B):-move_north(A,B).
move(A,B):-move_north_east(A,B).
move(A,B):-move_north_west(A,B).
move(A,B):-move_south(A,B).
move(A,B):-move_south_east(A,B).
move(A,B):-move_east(A,C),move(C,B).
move(A,B):-move_east(A,C),move_north(C,B).
move(A,B):-move_east(A,C),move_north_east(C,B).
move(A,B):-move_east(A,C),move_south(C,B).
move(A,B):-move_east(A,C),move_south_east(C,B).
move(A,B):-move_north(A,C),move(C,B).
move(A,B):-move_north(A,C),move_east(C,B).
move(A,B):-move_north(A,C),move_north_east(C,B).
move(A,B):-move_north(A,C),move_west(C,B).
move(A,B):-move_north_east(A,C),move(C,B).
move(A,B):-move_north_east(A,C),move_east(C,B).
move(A,B):-move_north_east(A,C),move_north(C,B).
move(A,B):-move_north_east(A,C),move_north_west(C,B).
move(A,B):-move_north_east(A,C),move_south_east(C,B).
move(A,B):-move_north_west(A,C),move(C,B).
move(A,B):-move_north_west(A,C),move_north_east(C,B).
move(A,B):-move_south(A,C),move(C,B).
move(A,B):-move_south(A,C),move_east(C,B).
move(A,B):-move_south_east(A,C),move(C,B).
move(A,B):-move_south_east(A,C),move_east(C,B).
move(A,B):-move_south_east(A,C),move_north_east(C,B).
move(A,B):-move_west(A,C),move_north(C,B).
N = 28.
==

__ 4. Running obstacle avoidance experiment __

==
?- experiment_file:run_experiments.
trn_episode,trn_grid,trn_distance,tst_episode,tst_grid,tst_distance,steps
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_west_north,63.25,56
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_west_south,63.25,106
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_middle_north,63.25,56
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_middle_south,63.25,56
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_east_north,63.25,56
Episode4,occ_grid_2_free,44.72,Episode10,occ_grid_east_south,63.25,56
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_west_north,210.95,161
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_west_south,210.95,191
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_middle_north,210.95,161
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_middle_south,210.95,179
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_east_north,210.95,161
Episode4,occ_grid_2_free,44.72,Episode6,occ_grid_east_south,210.95,211
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1,174.93,294
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1a,174.93,1718
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1b,174.93,294
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1c,174.93,1826
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1d,174.93,20805
Episode4,occ_grid_2_free,44.72,Episode18,occ_grid_1e,174.93,3573
true.
==

*/

% Constraints excluding left-recursive and redundant clauses.
configuration:metarule_constraints(m(identity,P,P),fail).
configuration:metarule_constraints(m(chain,P,P,_),fail).
configuration:metarule_constraints(m(chain,_,P,P),fail).

%configuration:metarule_constraints(m(chain,_P,Q,R),fail):-
%        opposite(Q,R).

opposite(move_north,move_south).
opposite(move_south,move_north).
opposite(move_east,move_west).
opposite(move_wast,move_east).
opposite(move_north_east,move_south_west).
opposite(move_south_west,move_north_east).
opposite(move_north_west,move_south_east).
opposite(move_south_east,move_north_west).


:-auxiliaries:set_configuration_option(clause_limit,[3]).
:-auxiliaries:set_configuration_option(max_invented,[0]).
:-auxiliaries:set_configuration_option(fetch_clauses,[all]).
%:-auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-auxiliaries:set_configuration_option(table_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(reduction,[none]).

background_knowledge(move/2, [move_north/2
                             ,move_north_east/2
                             ,move_east/2
                             ,move_south_east/2
                             ,move_south/2
                             ,move_south_west/2
                             ,move_west/2
                             ,move_north_west/2
                             %,group_contacts/3
                             %,ungroup_contacts/3
                             ]).

metarules(move/2,[chain,identity]).

positive_example(move/2,E):-
        training_example(E).

negative_example(move/2,_):- fail.


%!      training_example(?Episode) is semidet.
%
%       Name of the Episode used for training.
%
%       Found under the path configured in paths_episodes_root/1.
%
training_episode('Episode4').
%training_episode('Episode6').
%training_episode('Episode10').


%!      testing_example(?Episode) is semidet.
%
%       Name of the Episode used for testing.
%
%       Found under the path configured in paths_episodes_root/1.
%
%testing_episode('Episode4'). % destination(x(70.0),y(80.0)
%testing_episode('Episode6'). % destination(x(240.0),y(80.0)
%testing_episode('Episode10'). % destination(x(90.0),y(80.0)
testing_episode('Episode18'). % destination(x(210.0), y(100.0)
%testing_episode('Episode19'). % destination(x(280.0), y(120.0)
%testing_episode('Episode20'). % destination(x(270.0), y(100.0)


%!       training_example(-Example) is det.
%
%        Get an example instance from the training episode.
%
training_example(E):-
        training_episode(Ep)
        ,plan_exec:episode_instance(Ep,E).


%!       testing_example(-Example) is det.
%
%        Get an example instance from the testing episode.
%
testing_example(E):-
        testing_episode(Ep)
        ,plan_exec:episode_instance(Ep,E).


%!      run_experiments is det.
%
%       Run all obstalce avoidance experiments.
%
%       Results are printed at the Prolog console as CSV lines,
%       including a header line.
%
run_experiments:-
        report_header
        ,Es = [exp4_exp10_map_2_west_north
              ,exp4_exp10_map_2_west_south
              ,exp4_exp10_map_2_middle_north
              ,exp4_exp10_map_2_middle_south
              ,exp4_exp10_map_2_east_north
              ,exp4_exp10_map_2_east_south
              ,exp4_exp6_map_2_west_north
              ,exp4_exp6_map_2_west_south
              ,exp4_exp6_map_2_middle_north
              ,exp4_exp6_map_2_middle_south
              ,exp4_exp6_map_2_east_north
              ,exp4_exp6_map_2_east_south
              ,exp4_exp18_map_1
              ,exp4_exp18_map_1a
              ,exp4_exp18_map_1b
              ,exp4_exp18_map_1c
              ,exp4_exp18_map_1d
              ,exp4_exp18_map_1e
              ]
        ,forall(member(E,Es)
               ,(call(E)
                %,nl
                )
               ).

% Experiments on map 2 and variants.
exp4_exp6_map_2_west_north:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_west_north,250,180)
        ,!.
exp4_exp6_map_2_west_south:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_west_south,250,180)
        ,!.
exp4_exp6_map_2_middle_south:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_middle_south,250,180)
        ,!.
exp4_exp6_map_2_middle_north:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_middle_north,250,180)
        ,!.
exp4_exp6_map_2_east_north:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_east_north,250,180)
        ,!.
exp4_exp6_map_2_east_south:-
        obstacle_avoidance('Episode4','Episode6',occ_grid_east_south,250,180)
        ,!.
exp4_exp10_map_2_west_north:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_west_north,250,180)
        ,!.
exp4_exp10_map_2_west_south:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_west_south,250,180)
        ,!.
exp4_exp10_map_2_middle_south:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_middle_south,250,180)
        ,!.
exp4_exp10_map_2_middle_north:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_middle_north,250,180)
        ,!.
exp4_exp10_map_2_east_north:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_east_north,250,180)
        ,!.
exp4_exp10_map_2_east_south:-
        obstacle_avoidance('Episode4','Episode10',occ_grid_east_south,250,180).

% Experiments on Map and variants
exp4_exp18_map_1:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1,400,255).
exp4_exp18_map_1a:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1a,400,255).
exp4_exp18_map_1b:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1b,400,255).
exp4_exp18_map_1c:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1c,400,255).
exp4_exp18_map_1d:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1d,400,255).
exp4_exp18_map_1e:-
        obstacle_avoidance('Episode4','Episode18',occ_grid_1e,400,255).


%!      obstacle_avoidance(+Train,+Test,+TestGrid,+X,+Y) is det.
%
%       Run one step of an obstacle avoidance experiment.
%
obstacle_avoidance(Trn,Tst,G2,X,Y) :-
        T = move/2
        ,M1 = map(model_root(data/occ_grid_2_free),250,180)
        ,M2 = map(model_root(data/G2),X,Y)
        ,atomic_list_concat([exec_plan,Trn,Tst,G2],'_',Py)
        ,plan_exec:experiment_script(T,[Trn,100,M1],[Tst,100,M2,1],Trn,Py,Ns)
        ,report_results(Trn,Tst,occ_grid_2_free,G2,Ns).


%!      report_header is det.
%
%       Print out the header of a report CSV.
%
report_header:-
    format('trn_episode,trn_grid,trn_distance,tst_episode,tst_grid,tst_distance,steps')
    ,nl.


%!      report_results(+Train,+Test,+Grid1,+Grid2,+Counters) is det.
%
%       Print out the results of an experiment as one line of a CSV.
%
report_results(Trn,Tst,G1,G2,[C]):-
        arg(1,C,N)
        ,episode_target_distance(Trn,M_trn)
        ,episode_target_distance(Tst,M_tst)
        ,format('~w,~w,~w,~w,~w,~w,~w~n',[Trn,G1,M_trn,Tst,G2,M_tst,N]).


%!      episode_target_distance(+Episode,-Distance) is det.
%
%       Calculate the Euclidean Distance to a destination in an Episode.
%
episode_target_distance(Ep,M):-
        episode_instance(Ep,move(S1,_S2))
        ,inspect(position,position(Xp,Yp,_),S1)
        ,inspect(destination,destination(Xd,Yd,_),S1)
        ,maplist(term_value,[Xp,Yp,Xd,Yd],[Xp_v,Yp_v,Xd_v,Yd_v])
        ,maplist(truncate,[Xp_v,Yp_v,Xd_v,Yd_v],[Xp_,Yp_,Xd_,Yd_])
        ,magnitude(Xp_/Yp_,Xd_/Yd_,M_)
        ,format(atom(M),'~2f',[M_]).


%!      document_choices(-Clauses) is nondet.
%
%       Run an experiment and document all relevant options.
%
%       N is an integer, the number of clauses learned during the
%       experiment.
%
document_choices(_N):-
        list_options([clause_limit/1
                     ,max_invented/1
                     ,reduction/1
                     ,fetch_clauses/1
                     ,table_meta_interpreter/1
                     ,untable_meta_interpreter/1
                     ])
        ,nl
        ,current_prolog_flag(stack_limit, SL)
        ,format('Global stack limit ~D~n',[SL])
         ,current_prolog_flag(table_space, TS)
        ,format('Table space ~D~n',[TS])
        ,nl
        ,call_and_write(list_budget(action,_))
        ,call_and_write(training_episode(_))
        ,call_and_write(testing_episode(_))
        ,list_mil_problem(move/2)
        ,listing(actions:move_constraints/2)
        ,listing(actions:register_action/4)
        ,listing(plan_exec:episode_instance/2).


%!      call_and_write(?Goal) is nondet.
%
%       Call a goal and print out the called atom.
%
%       Used to document configuration options not in the main
%       configuration file.
%
call_and_write(G):-
        call(G)
        ,writeln(G)
        ,nl.
