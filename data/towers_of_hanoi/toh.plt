:- use_module(lib(mathemancy/mathemancy)).
:- use_module(scripts(unit_tests/test_harness)).
:-use_module(src(auxiliaries)).

:- set_prolog_flag(plunit_output, always).

:-set_test_options([
                load(normal),
                run(make)
                % format(log)
                %    ,show_blocked(true)
                ]).

set_config:-
        % auxiliaries:set_configuration_option(learning_predicate, [learn/1])
        auxiliaries:set_configuration_option(clause_limit, [4])
        ,auxiliaries:set_configuration_option(max_invented, [0])
        % ,auxiliaries:set_configuration_option(fetch_clauses, [all])
        ,auxiliaries:set_configuration_option(table_meta_interpreter, [true])
        ,auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
        % Goes faster than default '$'. Why?
        ,auxiliaries:set_configuration_option(invented_symbol_prefix,['inv_']).
reset_config:-
        reset_defaults.

% Table target and invented predicates to avoid infinite
% left-recursion when testing.
table_learned:-
        table(program:toh/2).
        %,table(program:'$1'/2).
untable_learned:-
        untable(program:toh/2).
        %,untable(program:'$1'/2).
 
example(N, s(T,[],[]), s([],[],T)):-
    series(1, N, T).

:-begin_tests(toh, [setup((set_config
                           ,writeln('Current setup:')
                           ,auxiliaries:list_mil_problem(toh/2)
                           ,debug(program)
                           ,debug(problem)
                           )
                          )
                    ,cleanup((reset_config
                             ,nodebug(program)
                             ,nodebug(problem)
                             )
                            )
                    ]
             ).

test(toh_3, [setup((set_config
                       ,train_and_save([toh(s([1,2,3],[],[]),s([],[],[1,2,3]))]
                                      ,[]
                                      ,[
                                             move_12/2
                                            ,move_13/2
                                            ,move_21/2
                                            ,move_23/2
                                            ,move_31/2
                                            ,move_32/2
                                        ]
                                      ,[chain, identity]
                                      ,program
                                      ,short
                                      ,Rs)
                       ,table_learned
                       )
                      )
                ,cleanup((reset_config
                         ,untable_learned
                         ,cleanup_after_training(Rs)
                         )
                        )
                ]):-
        example(3, Ex3_1,Ex3_2)
        ,assertion( program:toh(Ex3_1,Ex3_2) )
        ,example(10, Ex5_1,Ex5_2)
        ,assertion( program:toh(Ex5_1,Ex5_2) ).

:- end_tests(toh).

