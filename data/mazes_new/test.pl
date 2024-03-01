:-module(test,[]).

load_file_from_row(row(_,Name,_,_,_)):-
    atom_concat('test_mazes/', Name, Path),
    [Path].

:-  csv_read_file('./test_mazes/data.csv', [_|Rows]),
    maplist(load_file_from_row,Rows).

:- table(solve/2).

move_left(S1,S2):-
    step_left(S1,S2).
move_down(S1,S2):-
    step_down(S1,S2).
move_right(S1,S2):-
    step_right(S1,S2).
move_up(S1,S2):-
    step_up(S1,S2).

solve(A,B):-move_down(A,B).
solve(A,B):-move_left(A,B).
solve(A,B):-move_right(A,B).
solve(A,B):-move_up(A,B).
solve(A,B):-move_down(A,C),solve(C,B).
solve(A,B):-move_left(A,C),solve(C,B).
solve(A,B):-move_right(A,C),solve(C,B).
solve(A,B):-move_up(A,C),solve(C,B).



map_row_to_steps(Pred, row(ID,A,B,C,D),row(ID,A,B,C,D,Steps)):-
    test_id(ID,Pred,Steps).

run_tests:-
    csv_read_file('./test_mazes/data.csv', [row(A,B,C,D,E)|Rows1]),
    maplist(map_row_to_steps(solve),Rows1,Rows2),
    csv_write_file('./test_mazes/results.csv', [row(A,B,C,D,rl,louise)|Rows2]).

test_id(Id,Pred,N):-
    % findall(Id/Dims,(PM:maze(Id,Dims,_),Id \== zero),Ids)
    % ,length(Ids,I)
    % ,debug(test_run,'Loaded ~w test mazes.',[I])
    % ,member(Id/(Wd-Ht),Ids)
    % ,debug(test_run,'Testing maze ~w (~wx~w):',[Id,Wd,Ht])
    testing_instance(Pred,Id,E),
    write(E),
    solve_maze(E,N).

%!      solve_maze(+Example,-Count) is nondet.
%
%       Solve an Example maze and Count the number of steps taken.
%
%       Similar to trace_path_/4 but directly calls the meta-interpreter
%       execute_plan/5 to generate a path through a maze and count the
%       steps needed to find it, without printing out the result.
%
solve_maze(E,N):-
        C = c(0)
        ,C_ = c(0)
        ,debug(trace_path,'Finding a path...',[])
        ,time(execute_plan(test,C,E,[],_Cs) )
        ,preserve_and_reset_counter(C,C_)
        ,arg(1,C_,N)
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


execute_plan(_M,_C,true,Ms,Ms):-
        !.
execute_plan(M,C,(L,Ls),Acc,Ms):-
        execute_plan(M,C,L,Acc,Acc_)
        ,execute_plan(M,C,Ls,Acc_,Ms).
execute_plan(M,C,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,B)
        ,debug(execute_plan,'Literal-Body: ~w - ~w',[L,B])
        ,record_last_action(L,Acc,C,Acc_)
        ,execute_plan(M,C,B,Acc_,Ms).

% safe_clause/2 now only meta-interprets top-level move actions, i.e.
% move_up/2, move_down/2, move_right/2 and move_left/2, and the target
% predicate, solve/2. It hands off everything else to the Prolog engine.
% That's so we can count only the calls to move actions. Note
% that this _includes_ backtracking over the move actions.
safe_clause(M,L,true):-
        built_in_or_library_predicate(L)
        ,!
        ,call(M:L).
safe_clause(M,L,true):-
        action_predicate(L)
        ,call(M:L)
        % Stops unnecessary backtracking over this action.
        ,!.
safe_clause(M,solve(S1,S2),B):-
        clause(M:solve(S1,S2),B).


built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).


% Used to filter out non-move actions.
action_predicate(move_up(_,_)).
action_predicate(move_down(_,_)).
action_predicate(move_left(_,_)).
action_predicate(move_right(_,_)).

% update_count/1 is moved here to ensure we're only counting the
% instances of move actions that are meta-interpreted.
record_last_action(L,Acc,C,L):-
    once(action_predicate(L))
    ,\+ opposite_moves(L,Acc)
    ,update_count(C)
    ,!.
record_last_action(L,Acc,_C,Acc):-
% TODO: should we count this move too? It's rejected.
    \+ opposite_moves(L,Acc)
%    ,update_count(C)
    .


opposite_moves(move_up(_,_),move_down(_,_)).
opposite_moves(move_down(_,_),move_up(_,_)).
opposite_moves(move_left(_,_),move_right(_,_)).
opposite_moves(move_right(_,_),move_left(_,_)).


update_count(C):-
        arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).
