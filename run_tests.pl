:- [load_headless].
:- use_module(data/scripts/learning_curve/learning_curve).

:- set_prolog_flag(stack_limit, 2_147_483_648).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).

:-set_prolog_flag(table_space, 12_000_000_184).
:-current_prolog_flag(table_space, V)
 ,format('Table space ~D~n',[V]).


:-auxiliaries:set_configuration_option(clause_limit,[1]).
:-auxiliaries:set_configuration_option(max_invented,[0]).
:-auxiliaries:set_configuration_option(reduction,[plotkins]).
:-auxiliaries:set_configuration_option(resolutions,[0]).
:-auxiliaries:set_configuration_option(fetch_clauses,[all]).
%:-auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-auxiliaries:set_configuration_option(table_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).



update_experiment_file(FilePath, ModuleName):-
    retract(experiment_file(_,_)),
    assert(experiment_file(FilePath,ModuleName)),
    load_experiment_file:load_experiment_file(FilePath).

test_graphs(K, TUP):-
    NoiseTypes = [no_noise,ambiguities,false_positives,false_negatives],
    maplist(test_graph(K,TUP), NoiseTypes).
    
test_graph(K, TUP, Noise):-
    atomic_list_concat([graph, '_' ,Noise], LP),
    atomic_list_concat(['data/coloured_graph/', LP], FilePath),
    update_experiment_file(FilePath,LP),
    T = LP/2,
    test(T,K, TUP).

test(T,K, (Stride, LB, UB)):-
    M = acc,
    format('Testing: ~w\n',[T]),
    float_interval(LB,UB,Stride,Ss),
    learning_curve(T,M,K,Ss,_,_).

test_robots(K,TUP):-
    update_experiment_file('data/robots/robots', robots),
    test(move/2,K,TUP).

run_all_tests:-
    K = 10,
    Stride = 1,
    LB = 1,
    UB = 9, 
    test_graphs(K,(Stride,LB,UB)),
    test_robots(K,(Stride,LB,UB)).

