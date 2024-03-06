:-module(plan_exec, [plan_executor/6
                    ,plan_executor/5
                    ,plan_executor/3
                    ,experiment_script/6
                    ,learn_and_write_plan/4
                    ,episode_instance/2
                    ,prove_plans/5
                    ,actions_simulation/2
                    ,write_python_script/4
                    ]).

:-use_module(model_root(src/types)).
:-use_module(model_root(src/sim_parser)).
:-use_module(model_root(output/vector_access)).
:-use_module(model_root(src/actions)).

/** <module> Compose and execute plans in the Toy Problem simulation.

*/


%!      plan_executor(+Module,+Example,+KLimit,+JLImit,-Actions,-Count)
%!      is nondet.
%
%       Execute a plan on an Instance and collect a list of Actions.
%
%       As plan_executor/3 but restricts the number of action terms in
%       each sub-list of Actions to the value of KLmit and the number
%       of lists of Actions returned to JLimit.
%
%       KLimit is a number limiting the number of action terms in each
%       sub-list of Actions.
%
%       JLimit is a number limiting the number of Actions lists
%       returned.
%
%       @tbd This predicate uses limit/2 to limit the number of Actions
%       lists returned, passing JLimit as the first argument to it.
%
plan_executor(M,E,K,J,As,C):-
        limit(J,plan_executor(M,E,K,As,C)).


%!      plan_executor(+Module,+Instance,+Limit,-Actions,-Count)
%!      is nondet.
%
%       Execute a plan on an Instance and collect a list of Actions.
%
%       As plan_executor/3 but restricts the number of action terms in
%       each sub-list of Actions to the value of Limit. Additionally,
%       defines the Module in which Instance is to be proved.
%
%       Module is the name of a module where a learned proram is to be
%       found.
%
%       Limit is a number limiting the number of action terms in each
%       sub-list of Actions.
%
plan_executor(M,E,L,As,C_):-
        C = c(0)
        ,C_ = c(0)
        ,distinct( execute_plan(M,C,E,[],As) )
        ,preserve_and_reset_counter(C,C_)
        ,length(As,N)
        ,N =< L.


%!      preserve_and_reset_counter(+Counter1,+Counter2) is det.
%
%       Reset a counter to 0 while preserving its value in another.
%
%       This is to make it possible to count steps of plan execution
%       while starting a fresh count for each new plan.
%
%       Counter1 and Counter2 are terms c(I) where I is some number,
%       used as a counter, currently of the number of steps, with
%       backtracking, needed to generate a sequence of actions with a
%       call to execute_plan/5.
%
%       This predicate:
%       a) Sets the value of I in Counter2 to the value of I in Counter1
%       so that value can be returned after the end of a call to
%       execute_plan/5.
%       b) Sets the value of I in Counter1 to 0, so that it can
%       correctly count between nondeterministic calls to
%       execute_plan/5.
%
%       Without this predicate and the two counters it manipulates a
%       counter of steps of plan execution will keep being updated every
%       time a new plan is executed so that it counts the number of
%       steps to execute all plans in succession, rather than each one.
%
preserve_and_reset_counter(C,C_):-
        arg(1,C,J)
        ,nb_setarg(1,C_,J)
        ,nb_setarg(1,C,0).


%!      plan_executor(+Instance,-Actions,-Steps) is det.
%
%       Execute a plan on an Instance and collect a list of Actions.
%
%       Instance is a testing instance that should be an atom of a
%       dyadic predicate where the two arguments are state vectors for
%       the input and output states of a composite action. One of the
%       state vector terms should define one or more autonomous agent
%       goals, to be satisfied by proving the instance with a learned
%       program accessible to this predicate.
%
%       Limit is a number limiting the number of action terms in
%       Actions.
%
%       Actions is a list of action terms representing the actions taken
%       by an agent to satisfy the goals in Instance, as a learned
%       program is executed.
%
%       Steps is the number of steps taken to satisfy the goal in the
%       testing Instance by running a learned program, including all the
%       backtracking.
%
%       @tbd This predicate uses distinct/1 to returned only unique
%       actions (i.e. to avoid duplicates in Actions).
%
plan_executor(E,As,C):-
        C = c(0)
        ,definition_module(E,M)
        ,distinct( execute_plan(M,C,E,[],As) ).


%!      definition_module(+Instance,-Module) is det.
%
%       Definition Module of a testing Instance.
%
%       Instance is a testing instance for a learned predicate, as
%       returned by testing_example/1, defined in the current experiment
%       file.
%
%       Module is the definition module of the program defining the
%       predicate of the testing instance. This is used to locate the
%       definition of the predicate so that it ca be tested.
%
%       The experiment file setup seems to change the module where
%       testing_example/1 is defined everytime the experiment file is
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


%!      execute_plan(?Goal,+Steps,+Acc,-Actions) is det.
%
%       Execute a learned plan and collect its sequence of Actions.
%
%       Meta-interpreter for a plan generating sequences of actions to
%       satisfy an agent's goal.
%
%       Goal is the initial goal of the plan to be executed. This will
%       be the same name as a target predicate in the current experiment
%       file.
%
%       Steps is a term c(I) where I is the count of steps taken to
%       satisfy Goal, updated destructively by nb_setarg/3, so that it
%       persists through backtracking. This count includes steps taken
%       during backtracking, i.e. it counts every step taken during the
%       search for the next action, not just the steps of the sequence
%       of Actions returned at the end.
%
%       Acc is an empty list, used as an accumulator of Coordinates.
%
%       Actions is a list of action terms as defined in the
%       specification. Actions is in reverse order, with the first
%       action at the end and the last action first. It should probably
%       be reversed before being processed further.
%
execute_plan(_M,_C,true,Ms,Ms):-
        !.
execute_plan(M,C,(L,Ls),Acc,Ms):-
        execute_plan(M,C,L,Acc,Acc_)
        ,execute_plan(M,C,Ls,Acc_,Ms).
execute_plan(M,C,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,C,B)
        ,debug(execute_plan,'Literal-Body: ~w - ~w',[L,B])
        ,extract_action(L,Acc,Acc_)
        ,execute_plan(M,C,B,Acc_,Ms).


%!      safe_clause(+Module,?Head,+Counter,-Body) is nondet.
%
%       Variant of clause/2 avoiding calling built-ins.
%
%       Module is the name of a module in which to find, or call Head.
%
%       Head is the head of a clause in the program database.
%
%       Counter is the step counter, used to count only calls to move
%       actions, including backtracking, but excluding auxiliaries.
%
%       Body is the body of the clause, or the atom 'true' if Head is
%       the head of a built-in or library predicate, or an action
%       predicate.
%
%       There are two separate motivations for this predicate: a) safely
%       processing built-ins and library predicates, and b) avoiding
%       meta-interpretation of vector access primitives and handing them
%       off to Prolog instead.
%
%       Accessing built-ins by a call to clause/2, as in a
%       meta-interpreter like the one in execute_plan/5 raises
%       permission errors. This variant of clause/2 ensures that
%       built-ins (and predicates defined in libraries) are handed off
%       to Prolog instead. Other predicates, particularly learned plans,
%       are beheaded by clause/2 and their body literals returned.
%
%       Additionally, this predicate hands the body of PDDL-like
%       actions, consisting of vector access primitives, to Prolog, via
%       call/1. It only returns the Body of a clause for further
%       meta-interpretation if Head is the head of a target predicate,
%       such as move/2. Whether an action is a target predicate or a
%       PDDL-like action is determined by a call to action_predicate/1.
%
safe_clause(M,L,_C,true):-
        built_in_or_library_predicate(L)
        ,!
        ,call(M:L).
safe_clause(_M,L,C,true):-
        action_predicate(L)
        ,update_count(C)
        ,call(actions:L)
        % Stops unnecessary backtracking over this action.
        ,!.
safe_clause(M,move(S1,S2),_C,B):-
        clause(M:move(S1,S2),B).


%!      action_predicate(?Head) is semidet.
%
%       True when Head is the head of an action predicate.
%
%       Used to separate PDDL-like actions from vector access primitives
%       in safe_clause/3.
%
action_predicate(move_north(_,_)).
action_predicate(move_north_east(_,_)).
action_predicate(move_east(_,_)).
action_predicate(move_south_east(_,_)).
action_predicate(move_south(_,_)).
action_predicate(move_south_west(_,_)).
action_predicate(move_west(_,_)).
action_predicate(move_north_west(_,_)).
action_predicate(group_contacts(_,_,_)).
action_predicate(ungroup_contacts(_,_,_)).


%!      extract_action(+Literal,+Acc,-New) is det.
%
%       Extract an Action term from the current Literal.
%
%       Literal is a literal of a target predicate, a dyadic predicate
%       where the first argument is a list representing an agent's
%       world-state before an action is taken, and the second argument
%       is a list representing the world-state after that action.
%
%       Acc is the accumulator of actions; it is passed to this
%       predicate by execute_plan/5. Each element of this list is an
%       action term, as defined in the specification.
%
%       Actions is the list of actions in Acc, updated with the
%       action term extracted from the action represented by Literal.
%
extract_action(L,Acc,[A_|Acc]):-
        once(action_predicate(L))
        ,L =.. [Mv,_S1,S2]
        ,action_type_name(Mv,T,N)
        ,action_params(T,S2,Ps)
        ,create_action(T,N,Ps,A)
        ,length(Acc,V)
        ,modify(action,set,actionId(V),A,A_)
        ,debug(extract_action,'New action: ~w',[A_])
        % Stops backtracking and re-adding the previous action to Acc.
        ,!.
extract_action(_L,Acc,Acc).


%!      action_type_name(+Action,-Type,-Name) is det.
%
%       Extract the Type and Name of an Action.
%
action_type_name(Mv,move,N):-
        atom_concat(move_,N,Mv)
        ,debug(action_type_name,'action-type-name: ~w - move - ~w',[Mv,N]).


%!      update_count(+Count) is det.
%
%       Update the step counter for execute_plan/5.
%
update_count(C):-
        arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).



%!      testing_script(+Target,+Train,+Test,+Plan,+Python,-Steps)
%!      is det.
%
%       Train on a Target, Test, and write a Plan and a Python script.
%
%       Target is the learning target.
%
%       Train and Test are lists of the form [Episode,Budget,Map], where
%       Episode is the atomic name of an episode, Budget is a number
%       indicating the list-budget for actions, and Map is a term
%       map(Path,X,Y), where Path is the path of an occlusion grid file,
%       and X and Y the maximum x and y dimensions in that map. The
%       Train and Test lists correspond to those options for the
%       training and testing steps in the experiment, respectively.
%
%       Plan is the name of a Prolog module where the learned plan will
%       be written under P in plans_root(P).
%
%       Python is the path to a file where a Python script will be
%       written under R in python_root(R).
%
%       Steps is a list of terms C(I), counting the steps for each
%       path written as a sequence of actions in the python script.
%
%       On successive backtracking multiple files are written with the
%       same name and a postfix number counting from 0.
%
experiment_script(T,[Trn,N,map(Gr1,X1,Y1)],[Tst,M,map(Gr2,X2,Y2),J],Pl,Py,Ns):-
        model_configuration:list_budget(action,B0)
        ,model_configuration:map(P0,X0,Y0)
        ,C = ep(0)
        ,training_step(T
                      ,Pl
                      ,C
                      ,Trn
                      ,N
                      ,map(Gr1,X1,Y1)
                      ,B0
                      ,map(P0,X0,Y0)
                      ,Ps
                      )
        ,testing_step(Py
                     ,C
                     ,Ps
                     ,Tst
                     ,M
                     ,J
                     ,map(Gr2,X2,Y2)
                     ,B0
                     ,map(P0,X0,Y0)
                     ,Ns
                     ).


%!      training_step(+T,+Pl,+C,+Ep_Trn,+Bt,+Map1,+Bc,+Map2,-Prog)
%!      is det.
%
%       Training step of experiment_script/5.
%
%       Arguments:
%
%       T: Symbol/Arity of learning target.
%       Pl: Atomic name of the Prolog plan file.
%       C: c(I) compound term used as a file counter.
%       Ep_Trn: atomic name of the training episode.
%       Bt: action list-budget for training.
%       Map1: map/3 term for occlusion grid used in training.
%       Bc: action list-budget originally in config.
%       Map2: map/3 term for occlusion grid originally in config.
%       Prog: List of clauses learned from training episode.
%
training_step(T,Pl,C,Ep_Trn,B_trn
             ,map(G_trn,X_trn,Y_trn),B_conf,map(G_conf,X_conf,Y_conf),Ps):-
        script_paths(Pl,nil,C,Pl_f,_)
        ,S = setup_cleanup(B_trn,map(G_trn,X_trn,Y_trn))
        ,G = (actions:load_map
             ,learn_and_write_plan(T,Pl_f,Ep_Trn,Ps)
             %,get_single_char(_)
              )
        ,Cl = setup_cleanup(B_conf,map(G_conf,X_conf,Y_conf))
        ,setup_call_cleanup(S,G,Cl).


%!      testing_step(+Py,+C,+Ps,+Ep_tst,+B_tst,+Lim,+Map1,+B_conf
%!      ,+Map2,-Ns) is det.
%
%       Training step of experiment_script/5.
%
%       Arguments:
%
%       Py: Atomic name of the Python plan file.
%       C: c(I) compound term used as a file counter.
%       Prog: List of clauses learned from training episode.
%       Ep_Tst: atomic name of the testing episode.
%       Bt: action list-budget for training.
%       Map1: map/3 term for occlusion grid used in training.
%       Bc: action list-budget originally in config.
%       Map2: map/3 term for occlusion grid originally in config.
%       Ns: number of steps to find a path.
%
testing_step(Py,C,Ps,Ep_tst,B_tst,Lim
            ,map(G_tst,X_tst,Y_tst),B_conf,map(G_conf,X_conf,Y_conf),Ns):-
        S2 = setup_cleanup(B_tst,map(G_tst,X_tst,Y_tst))
        ,G2 = (actions:load_map
              ,episode_instance(Ep_tst,I_tst)
              ,prove_plans(I_tst,Ps,Lim,As,Ns)
              ,forall(member(As_i,As)
                     ,(actions_simulation(actions(As_i),Ss)
                      ,script_paths(nil,Py,C,_,Py_f)
                      ,write_python_script(C,Py_f,Ep_tst,Ss)
                      )
                     )
              )
        ,Cl_2 = setup_cleanup(B_conf,map(G_conf,X_conf,Y_conf))
        ,setup_call_cleanup(S2,G2,Cl_2).


%!      setup_cleanup(+Budget,+Map) is det.
%
%       Setup and cleanup action list-Budget and Map.
%
%       Budget is an integer, the action list-budget.
%
%       Map is a term map(Path,X,Y), pointing to the occlusion grid.
%
%       This predicate first retracts the current list_budget(action,_)
%       and map/3 term from model_configuration, then asserts new terms
%       with the values passed in.
%
%       Called both before and after running a training and test
%       script.
%
setup_cleanup(B,map(G,X,Y)):-
        retract(model_configuration:list_budget(action,_))
        ,retract(model_configuration:map(_,_,_))
        ,asserta(model_configuration:list_budget(action,B))
        ,asserta(model_configuration:map(G,X,Y)).


%!      script_paths(+Plan,+Python,+Counter,-File1,-File2) is det.
%
%       Compose paths to Plan and Python scripts.
%
%       Counter is a counter for the index affix to the names of Plan
%       and Python files.
%
%       File1 is the path to the Plan file and File2 is the path to the
%       Python file.
%
script_paths(Pl,Py,C,data(Pl_f),data(Py_f)):-
        model_configuration:paths_plans_root(Pl_R)
        ,model_configuration:paths_python_root(Py_R)
        ,arg(1,C,I)
        ,format(atom(Pl_f),'~w/~w_~w.pl',[Pl_R,Pl,I])
        ,format(atom(Py_f),'~w/~|~`0t~d~4+_~w.py',[Py_R,I,Py]).


%!      learn_and_write_plan(+Target,+File,+Episode,-Program) is det.
%
%       Learn a plan from an Episode and write it out as a Python file.
%
%       Target is the predicate indicator, Symbol/Arity, of a learning
%       predicate.
%
%       File is path to a prolog file in which to write the program
%       learned from Target, which should be a plan... ner.
%
%       Episode is the atomic name of an episode, that will become the
%       name of the plan...ner module in File.
%
%       Program is the list of clauses in the learned program, flattened
%       in case of sub-lists (i.e. sub-programs learned with e.g.
%       learn_greedy/1).
%
learn_and_write_plan(T,F,Ep,Ps_f):-
        experiment_data(T,_Pos,Neg,BK,MS)
        ,episode_instance(Ep,Ex)
        ,findall(Ps_i
                ,(timed_learn(false,[Ex],Neg,BK,MS,Ps_i)
                 ,debug_clauses(learn_and_write_plan,Ps_i)
                 )
                ,Ps)
        ,S = (expand_file_search_path(F,E)
             ,open(E,write,Strm,[alias(plan_file)
                                ,close_on_abort(true)
                               ])
            )
        ,G = (portray_clause(Strm,:-module(Ep,[]))
             ,nl(Strm)
             ,debug(learn_and_write_plan,'Learning plans and writing to file:',[])
             ,forall(member(Ci,Ps)
                    ,(portray_clause(Strm,Ci,[spacing(standard)
                                             ]
                                    )
                     )
                    )
             )
        ,C = close(Strm)
        ,setup_call_cleanup(S,G,C)
        ,flatten(Ps,Ps_f).


%!      timed_learn(+Time,+Pos,+Neg,+BK,+MS,-Program) is nondet.
%
%       Make a learning query, wrapped in time/1 or not.
%
%       Time is a boolean, true or false, that determines whether the
%       learning query is timed, by wrapping it to a time/1 call.
%
timed_learn(true,Pos,Neg,BK,MS,Ps_i):-
        time(learning_query(Pos,Neg,BK,MS,Ps_i)).
timed_learn(false,Pos,Neg,BK,MS,Ps_i):-
        learning_query(Pos,Neg,BK,MS,Ps_i).



%!      prove_plans(+Instance,+Program,+Results,-Actions,-Steps) is det.
%
%       Executed a learned plan and return all ground Actions.
%
%       Instance is a testing instance.
%
%       Program is a list of clauses, of a learned program.
%
%       Results is a number passed to plan_executor/6 to limit the
%       number of paths generated.
%
%       Actions is a list of lists where each sub-list is a list
%       of action terms, as defined in the specification, generated by
%       calling plan_executor/5 on the testing Instance, with the given
%       Program asserted to the dynamic database. The limit on the
%       number of actions in each sub-list of Action is determined by
%       querying the model configuration option list_budget(action,B).
%
%       Steps is a list of counters C(I), counting the number of steps
%       to find each sequence of actions in Actions.
%
prove_plans(E,Ps,Lim,As,Ns):-
        model_configuration:list_budget(action,B)
        ,Ct = c(0)
        ,S = (table(plan:move/2)
             ,auxiliaries:assert_program(plan,Ps,Rs)
             )
        ,G = (findall(As_i-Sn
                     ,(plan_executor(plan,E,B,Lim,As_i,Sn)
                      ,arg(1,Ct,J)
                      ,length(As_i,L)
                      ,debug(prove_plans,'Exec\'d plan ~w. Actions: ~w Steps: ~w',[J,L,Sn])
                      ,succ(J,K)
                      ,nb_setarg(1,Ct,K)
                      )
                     ,As_)
             ,sort(As_,As_s)
             ,maplist(length,[As_,As_s],[N,M])
             ,debug(prove_plans,'Generated ~w plans; ~w unique ones.~n',[N,M])
             %,get_single_char(_)
             ,pairs_keys_values(As_s,As,Ns)
             )
        ,C = (erase_program_clauses(Rs)
             ,untable(plan:move/2)
             )
        ,setup_call_cleanup(S,G,C).



%!      actions_simulation(+Actions,-Python) is det.
%
%       Generate Python action code from a list of learned Actions.
%
actions_simulation(actions(As),Ss):-
% Actions are pushed to the plan so are in reverse order.
        reverse(As,As_)
        ,findall(A_
               ,(member(A,As_)
                ,inspect(action,actionType,T,A)
                ,term_value(T,T_)
                ,inspect(action,actionParams,P,A)
                ,term_value(P,Ps)
                ,maplist(term_to_atom,[X,Y],Ps)
                ,maplist(term_value,[X,Y],[X_,Y_])
                ,format(atom(A_),'ss.new_action(\'~w\',[~w,~w])',[T_,X_,Y_])
                )
               ,Ss).


%!      write_python_script(+Counter,+TestRoot,+Actions)
%!      is det.
%
%       Write a python script to play back a sequence of Actions.
%
%       Counter is a term c(I) where I is an integer, counting the
%       number of python scripts written so-far.
%
%       TestRoot is the root directory where test episodes will be
%       written. Test episodes are the episode directories created by
%       the SurveySimulation python class in 'test' mode. TestRoot is
%       passed as the save_loc parameter of an object of that class
%       where the given Actions are executed, and logged. Currently,
%       TestRoot is always the name of the test episode in
%       experiment_script/5.
%
%       Actions is a list of actions to be written in the python script
%       and played back later.
%
%       Note that when one of the python scripts generated is executed a
%       new directory is created under TestRoot, named Episode<n> where
%       n is the ascending index of episode directories created so-far.
%       That means there's a dependency between scripts - they have to
%       be run in order.
%
write_python_script(C,F,Ep,As):-
        length(As,N)
        ,model_configuration:paths_save_loc(P)
        ,next_episode(C,Ep_)
        ,debug(write_python_script,'Writing ~w actions for simulation ~w to ~w',[N,Ep,F])
        ,S = (expand_file_search_path(F,E)
             ,open(E,write,Strm,[alias(output_file)
                                ,close_on_abort(true)
                               ])
            )
        ,G = (writeln(Strm,'import survey_simulation')
             ,format(Strm,'ss = survey_simulation.SurveySimulation(mode=\'test\', save_loc=\'~w/~w\')~n',[P,Ep])
             ,forall(member(A,As)
                   ,writeln(Strm,A)
                   )
             ,writeln(Strm,'ss.save_episode()')
             ,format(Strm,'ss_pb = survey_simulation.SurveySimulation(\'playback\',save_loc=\'~w/~w/~w\')~n',[P,Ep,Ep_])
             )
        ,Cl = close(Strm)
        ,setup_call_cleanup(S,G,Cl).



%!      next_episode(+Counter,-Episode) is det.
%
%       Get the next episode name for the playback directory.
%
%       Counter is a term c(I) where I is an integer, counting the
%       number of python scripts written so-far.
%
%       Episode is the name of the next directory that will be created
%       in the playback directory when a python script is executed that
%       starts the simulation in playback mode.
%
%       The value of I in Counter is incremented destructively after
%       Episode is created.
%
next_episode(C,Ep):-
        arg(1,C,I)
        ,atom_concat('Episode',I,Ep)
        ,arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).



%!      episode_instance(+Episode,-Instance) is det.
%
%       An Instance of a move action in an Episode.
%
%       Use as a generator for training and testing examples.
%
%       Episode is the name of a directory storing the data for
%       survey-simulation episode, created by interaction with the
%       simulation in 'manual' mode. The Episode<n>_META and
%       Episode<n>_ACTIONS files in that directory will be parsed to
%       initialise the model, and collect training examples,
%       respectively, to compose into Instance.
%
%       Instance is an atom move(Ss,Gs) where Ss is a state vector
%       representing the starting state of the simulation before a move
%       and Gs is a state vector representing the state of the
%       simulation after the move, to be used as a goal for planning,
%       or to test a learned plan.
%
:-if((model_configuration:state_vector_terms(Ss)
     ,memberchk(plan,Ss)
     )
    ).
episode_instance(E,move(Ss,Gs)):-
        episode_paths(E,F1,F2)
        ,parse_episode(F1,Ms)
        ,parse_episode(F2,As)
        ,episode_initialisation(Ms,S1)
        ,episode_actions(As,As_)
	,last(As_,P)
        % Goal plan set to var because unknown
        ,modify(plan,_Plan,S1,S2)
        ,modify(position,P,S2,S3)
        ,inspect(position,position(X,Y,Theta),S3)
        ,modify(destination,destination(X,Y,Theta),S3,Gs)
        ,modify(destination,destination(X,Y,Theta),S1,Ss).
:-else.
episode_instance(E,move(Ss,Gs)):-
        episode_paths(E,F1,F2)
        ,parse_episode(F1,Ms)
        ,parse_episode(F2,As)
        ,episode_initialisation(Ms,S1)
        ,episode_actions(As,As_)
	,last(As_,P)
        ,modify(position,P,S1,S2)
        ,inspect(position,position(X,Y,Theta),S2)
        ,modify(destination,destination(X,Y,Theta),S2,Gs)
        ,modify(destination,destination(X,Y,Theta),S1,Ss).
:-endif.


%!      episode_paths(+Episode,-Meta,-Actions) is det.
%
%       Compose paths for META and ACTIONS file for an Episode.
%
%       Episode should be the atomic name of a survey-simulation
%       episode. It seems only names in the form Episode<n> where <n> is
%       some integer, are accepted.
%
%       Meta and Actions are the paths to the Episode<n>_META and
%       Episode<n>_ACTIONS files holding metadata and actions, in the
%       Episode<n> directory, under the path defined in episodes_root/1.
%
episode_paths(E,data(M),data(A)):-
        model_configuration:paths_episodes_root(Pe)
        ,format(atom(M),'~w/~w/~w_META',[Pe,E,E])
        ,format(atom(A),'~w/~w/~w_ACTIONS',[Pe,E,E]).
