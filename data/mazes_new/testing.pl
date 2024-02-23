:-module(testing,[test_run/3
                 ,testing_instance/3
                 ]).

:-use_module(src(louise)).
:-use_module(src(auxiliaries)).
:-use_module(move_generator).
:-use_module(map_display).
:-use_module(generator_configuration).

/** <module> Testing predicates for maze solving experiments.

*/

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
