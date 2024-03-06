% Start in dark mode
%:-use_module(library(theme/dark)).
% Start in dork mode
%:-use_module((themes/dork)).

:- set_prolog_flag(encoding, utf8).

:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(model_root, Dir)).

user:file_search_path(model_src, model_root(src)).
user:file_search_path(model_lib, model_root(lib)).
user:file_search_path(model_data, model_root(data)).
user:file_search_path(model_output, model_root(output)).
user:file_search_path(model_scripts, model_data(scripts)).

:-doc_browser.

:-use_module(model_root(model_configuration)).
:-use_module(model_src(state_vector)).
:-use_module(model_src(state_operations)).
:-use_module(model_src(printing)).
:-use_module(model_data(actions)).
:-use_module(model_src(sim_parser)).

edit_files:-
% Uncomment, or add, the paths of files you want to open in the IDE when
% the project loads, below.
	edit(model_root(load_project))
	,edit(model_root(model_configuration))
	,edit(model_data(specification))
	,edit(model_src(types))
	%,edit(model_src(state_vector))
	%,edit(model_src(state_operations))
	%,edit(model_src(printing))
	,edit(model_data(actions))
	,edit(model_src(sim_parser))
	.
:-edit_files.

:- write_primitives,
     primitives_file(F),
     user:use_module(F).

%:-set_test_options([load(normal),run(make)]).
%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
:- set_prolog_flag(stack_limit, 2_147_483_648).
%:- set_prolog_flag(stack_limit, 4_294_967_296).
%:-set_prolog_flag(stack_limit, 8_589_934_592).
%:-set_prolog_flag(stack_limit, 17_179_869_184).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).
