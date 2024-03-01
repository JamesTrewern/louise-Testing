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
    csv_write_file('./test_mazes/results.csv', [row(A,B,C,D,E,rl,louise)|Rows2]).

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

%:-table(execute_plan/5).

execute_plan(_M,_C,true,Ms,Ms):-
        !.
execute_plan(M,C,(L,Ls),Acc,Ms):-
        execute_plan(M,C,L,Acc,Acc_)
        ,execute_plan(M,C,Ls,Acc_,Ms).
execute_plan(M,C,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,B)
        ,extract_coords(L,Acc,Acc_)
        ,avoid_oscillation(Acc_)
        ,update_count(C)
        ,execute_plan(M,C,B,Acc_,Ms).

safe_clause(M,L,true):-
        built_in_or_library_predicate(L)
        ,!
        ,call(M:L).
safe_clause(M,L,B):-
        clause(M:L,B).
update_count(C):-
        arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).
built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).
avoid_oscillation([]):-
        !.
avoid_oscillation([_]):-
        !.
avoid_oscillation([_:D1,_:D2|_]):-
        debug(avoid_oscillation,'Checking oscillation between ~w, ~w',[D1,D2])
        ,\+ opposite_direction(D1,D2).
opposite_direction(d,u).
opposite_direction(u,d).
opposite_direction(l,r).
opposite_direction(r,l).

extract_coords(L,Acc,Acc_2):-
        L =.. [Mv,S1,S2]
        ,move_direction(Mv,D)
        ,S1 = [Id,X1/Y1|_]
        ,S2 = [Id,X2/Y2|_]
        ,include_coords(X1/Y1:D,Acc,Acc_1)
        ,include_coords(X2/Y2:D,Acc_1,Acc_2)
        ,!.
extract_coords(_L,Acc,Acc).

move_direction(move_down, d).
move_direction(move_up, u).
move_direction(move_left, l).
move_direction(move_right, r).

include_coords(X/Y:D,Acc,[X/Y:D|Acc]):-
% Start and end are already ground so this excludes them
%        \+ ground(X/Y:D)
        ground(X/Y:D)
        ,\+ memberchk(X/Y,Acc)
        ,!.
include_coords(_X/_Y:_D,Acc,Acc).
