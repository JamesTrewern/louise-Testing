:-module(testing,[test_run/2
                 ,test_run/3
                 ,testing_instance/3
                 ]).

:-use_module(src(louise)).
:-use_module(src(auxiliaries)).
:-use_module(move_generator).
:-use_module(map_display).
:-use_module(generator_configuration).

/** <module> Testing predicates for maze solving experiments.

Calling test_run/2 script to test maze-solver without printing mazes:

==
?- test_run(solve/2,N).
Correct to: "testing:test_run(solve/2,N)"? yes
% Learning program for target: solve/2
% Training on maze 'zero':
% 100,736 inferences, 0.016 CPU in 0.014 seconds (110% CPU, 6447104 Lips)
% Learned program:
% solve(A,B):-move_down(A,B)
% solve(A,B):-move_left(A,B)
% solve(A,B):-move_right(A,B)
% solve(A,B):-move_up(A,B)
% solve(A,B):-move_down(A,C),solve(C,B)
% solve(A,B):-move_left(A,C),solve(C,B)
% solve(A,B):-move_right(A,C),solve(C,B)
% solve(A,B):-move_up(A,C),solve(C,B)
% Loading test mazes...
% 3,253,232 inferences, 0.156 CPU in 0.236 seconds (66% CPU, 20820685 Lips)
% Loaded 3 test mazes.
% Testing maze 0 (7x7):
% Finding all paths...
% 10,902 inferences, 0.000 CPU in 0.004 seconds (0% CPU, Infinite Lips)
N = c(113) ;
% Testing maze 1 (19x19):
% Finding all paths...
% 218,907 inferences, 0.047 CPU in 0.046 seconds (102% CPU, 4670016 Lips)
N = c(2273) ;
% Testing maze 2 (119x119):
% Finding all paths...
% 5,543,121 inferences, 17.594 CPU in 30.035 seconds (59% CPU, 315062 Lips)
N = c(57514).
==


*/


%!      test_run(+Target,-Count) is det.
%
%       As test_run/3 but does not print out maze maps.
%
test_run(T,N):-
        generator_configuration:primitives_file(_P,PM)
        ,debug(test_run)
        ,debug(trace_path)
        ,debug(test_run,'Learning program for target: ~w',[T])
        ,debug(test_run,'Training on maze \'~w\':',[zero])
        ,time( learn(T,Ps) )
        ,debug_clauses(test_run,'Learned program:',Ps)
        % Asserted in experiment_file module so map_display.pl preds can find it.
        ,S = (assert_program(experiment_file,Ps,Rs)
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
             ,debug(test_run,'Testing maze ~w (~wx~w):',[Id,Wd,Ht])
             ,testing_instance(T,Id,E)
             ,solve_maze(E,N)
             )
        ,C = (erase_program_clauses(Rs)
             %,untable(experiment_file:T)
             )
        ,setup_call_cleanup(S,G,C).



%!      solve_maze(+Example,-Count) is nondet.
%
%       Solve an Example maze and Count the number of steps taken.
%
%       Similar to trace_path_/4 but directly calls the meta-interpreter
%       execute_plan/5 to generate a path through a maze and count the
%       steps needed to find it, without printing out the result.
%
solve_maze(E,C_):-
        C = c(0)
        ,C_ = c(0)
        ,definition_module(E,M)
        ,debug(trace_path,'Finding a path...',[])
        ,time( execute_plan(M,C,E,[],_Cs) )
        ,preserve_and_reset_counter(C,C_)
        % execute_plan/5 may backtrack unnecessarily.
        ,!.
solve_maze(_E,_C):-
        debug(trace_path,'Failed to find a path!~n~n',[]).


%!      preserve_and_reset_counter(+Counter1,+Counter2) is det.
%
%       Reset a counter to 0 while preserving its value in another.
%
%       This is to make it possible to count steps of plan execution
%       while starting a fresh count for each new plan.
%
preserve_and_reset_counter(C,C_):-
        arg(1,C,J)
        ,nb_setarg(1,C_,J)
        ,nb_setarg(1,C,0).



%!      definition_module(+Instance,-Module) is det.
%
%       Definition Module of a testing Instance.
%
%       Instance is a testing instance for a learned predicate, as
%       returned by testing_instance/3, defined in the current
%       experiment file.
%
%       Module is the definition module of the program defining the
%       predicate of the testing instance. This is used to locate the
%       definition of the predicate so that it ca be tested.
%
%       The experiment file setup seems to change the module where
%       testing_instance/3 is defined everytime the experiment file is
%       loaded.
%
definition_module(E,experiment_file):-
        functor(E,F,A)
        ,current_predicate(experiment_file:F/A)
        ,!.
definition_module(E,M):-
        configuration:experiment_file(_P,M)
        ,functor(E,F,A)
        ,current_predicate(M:F/A).



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
        % Asserted in experiment_file module so map_display.pl preds can find it.
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
        ,generator_configuration:action_representation(stack_based)
        ,generator_configuration:primitives_file(_P,M)
        ,M:maze(Id,Dims,Ms)
        ,once(start_location(Ms,Dims,Xs/Ys))
        ,once(end_location(Ms,Dims,Xe/Ye))
        ,E =.. [T,[Id,Xs/Ys,s,[]],[Id,Xe/Ye,e,_Vs]].
testing_instance(T/2,Id,E):-
        Id \== zero
        ,generator_configuration:action_representation(memoryless)
        ,generator_configuration:primitives_file(_P,M)
        ,M:maze(Id,Dims,Ms)
        ,once(start_location(Ms,Dims,Xs/Ys))
        ,once(end_location(Ms,Dims,Xe/Ye))
        ,E =.. [T,[Id,Xs/Ys,s],[Id,Xe/Ye,e]].
