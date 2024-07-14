
% Remember to set:
% experiment_file('data/robots/robots.pl',robots).

:- use_module(data/scripts/learning_curve/learning_curve).

:- set_prolog_flag(stack_limit, 2_147_483_648).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).

:-set_prolog_flag(table_space, 17_179_869_184).
:-current_prolog_flag(table_space, V)
 ,format('Table space ~D~n',[V]).

% Uncomment to echo logging to console
%:-debug(learning_curve).

% Uncomment to allow tracking progresss while logging to file.
% :-debug(progress).

% Log learned hypotheses
%:-debug(learning_curve_full).

% :-debug(learning_curve_setup).

:-auxiliaries:set_configuration_option(clause_limit,[1]).
:-auxiliaries:set_configuration_option(max_invented,[0]).
:-auxiliaries:set_configuration_option(reduction,[plotkins]).
:-auxiliaries:set_configuration_option(resolutions,[0]).
:-auxiliaries:set_configuration_option(fetch_clauses,[all]).
%:-auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-auxiliaries:set_configuration_option(table_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).


% Uncomment line 57 in data/coloured_graph/graph_generator_configuration
% In configuration uncomment experiment_file('data/coloured_graph/coloured_graph.pl',coloured_graph). on line 399

run_acc:-
        T = graph_false_positives/2
        ,M = acc
        ,K = 10
        ,float_interval(1,9,1,Ss)
        ,learning_curve(T,M,K,Ss,Ms,SDs)
        ,writeln(Ms)
        ,writeln(SDs).

run_time:-
        T = graph_false_positives/2
        ,M = time
        ,K = 10
        ,float_interval(1,9,1,Ss)
        ,learning_curve(T,M,K,Ss,Ms,SDs)
        ,writeln(Ms)
        ,writeln(SDs).
