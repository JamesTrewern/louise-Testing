:-module(map_display, [print_maze/2
                      ,trace_path/2
                      ,trace_path/3
                      ,trace_path/4
                      ]).

:-use_module(generator_configuration).

/** <module> Pretty-print mazes in glorious ASCII.

__ Map Printing __

For debugging purposes, print_maze/2 can be used to print a map at the
Prolog console. The first argument of print_maze/3 determines what is
printed: 'tiles', 'coordinates', or 'both'. Those do what they sound
like.

Example of using print_maze/2:

==
?- print_maze(tiles,short_2).
s f w
w f w
w e w
true.

?- print_maze(coordinates,short_2).
0/2 1/2 2/2
0/1 1/1 2/1
0/0 1/0 2/0
true.

?- print_maze(both,short_2).
0/2.s 1/2.f 2/2.w
0/1.w 1/1.f 2/1.w
0/0.w 1/0.e 2/0.w
true.
==


__ Path printing __

Pretty-print a path through a map:
==
?- _T = lfa/2, _Id = 2, lfa:testing_instance(_T,_Id,_E), trace_path(tiles,_Id,_E).
► ► ▼ ■ □ □ □
■ ■ ▼ ■ □ ■ □
□ ■ ▼ ■ □ ■ □
□ ■ ▼ ■ ■ ■ □
□ ■ ▼ ■ ► ► ▼
□ ■ ▼ ■ ▲ ■ ▼
□ □ ► ► ▲ ■ █
true.
==

Print all paths; useful for testing:
==
?- findall(Id,move_generator:maze(Id,_,_),_Ids), member(Id,_Ids), Id \= tessera, writeln('Id':Id), trace_path(Id), nl, fail ; true.
==

__ Themes __

TODO.


*/


%!      print_maze(+How,+Map) is det.
%
%       Print a Map of a maze to the top-level.
%
%       Useful for debugging.
%
%       How is one of:
%       * tiles: prints the map as a list of terrain type symbols.
%       * coordinates: prints the coordinate of each Map location.
%       * both: prints both tiles and coordinates.
%
print_maze(W,Id):-
        generator_configuration:primitives_file(_P,M)
        ,M:maze(Id,Max_x-Max_y,Ms)
        ,maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(move_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        ,print_tile(W,Xt/Yt,T,[])
                        )
                       )
                ,nl
                )
               ).

%!      dec(+X,-Y) is det.
%
%       Decrement an integer X.
%
%       Helper arithmetic predicate to be passed to maplist/2 to
%       decrement coordinates.
%
dec(X,X_):- X_ is X - 1.



%!      trace_path(+Id,+Instance) is det.
%
%       Pretty-print a path through a maze.
%
%       As path(tiles,Id,Instance).
%
trace_path(Id,E):-
        trace_path(tiles,Id,E).



%!      trace_path(+What,+Id,+Instance) is det.
%
%       Pretty-print a path through a maze.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Id is the id of a maze map.
%
%       Instance is a testing instance through which a path is to be
%       traced.
%
%       The path taken must be defined in the experiment file
%       module so that it is accessible to this module.
%
trace_path(W,Id,E):-
%        debug(trace_path)
        definition_module(E,M)
        ,debug(trace_path,'Finding all paths...',[])
        ,G = findall(Cs
                ,(execute_plan(M,E,[],Cs)
                 )
                ,CS)
        ,time(G)
        ,length(CS,N)
        ,debug(trace_path,'Found ~w paths.',[N])
        ,nth1(I,CS,Ps)
        ,debug(trace_path,'Tracing path ~w...',[I])
        ,time( print_path(W,Id,Ps) ).



%!      trace_path(+What,+Id,+Instance,-Steps) is det.
%
%       Pretty-print a path through a maze.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Id is the id of a maze map.
%
%       Instance is a testing instance through which a path is to be
%       traced.
%
%       Steps is an integer, the number of steps taken to solve the
%       maze. This includes steps taken during backtracking.
%
%       The path taken must be defined in the experiment file
%       module so that it is accessible to this module.
%
trace_path(W,Id,E,C):-
%        debug(trace_path)
        C = c(0)
        ,definition_module(E,M)
        ,debug(trace_path,'Finding all paths...',[])
        ,G = findall(Cs
                ,(execute_plan(M,C,E,[],Cs)
                 )
                ,CS)
        ,time(G)
        ,length(CS,N)
        ,debug(trace_path,'Found ~w paths.',[N])
        ,nth1(I,CS,Ps)
        ,debug(trace_path,'Tracing path ~w...',[I])
        ,time( print_path(W,Id,Ps) ).



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


%!      execute_plan(?Goal,+Steps,+Acc,-Coordinats) is det.
%
%       Follow a path through a map and collect its Coordinates.
%
%       Meta-interpreter for a plan generating paths through a maze.
%
%       Goal is the initial goal of the plan to be executed. This is the
%       same name as a target predicate in an experiment learning a
%       maze-solver.
%
%       Steps is a term c(I) where I is the count of steps taken through
%       the maze while trying to solve it, updated destructively by
%       nb_setarg/3, so that it persists through backtracking. This
%       count includes steps taken during backtracking, i.e. it counts
%       every step taken during the search for a path through a maze,
%       not just the steps of the path returned at the end.
%
%       Acc is an empty list, used as an accumulator of Coordinates.
%
%       Coordinates is a list of pairs X/Y, the coordinates of each
%       floor traversed by a path generated by the learned plan.
%
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
        ,update_count(C)
        ,execute_plan(M,C,B,Acc_,Ms).


%!      update_count(+Count) is det.
%
%       Update the step counter for execute_plan/5.
%
update_count(C):-
        arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).


%!      execute_plan(?Goal,+Acc,-Coordinats) is det.
%
%       Follow a path through a map and collect its Coordinates.
%
%       Meta-interpreter for a plan generating paths through a maze.
%
%       Goal is the initial goal of the plan to be executed. This is the
%       same name as a target predicate in an experiment learning a
%       maze-solver.
%
%       Acc is an empty list, used as an accumulator of Coordinates.
%
%       Coordinates is a list of pairs X/Y, the coordinates of each
%       floor traversed by a path generated by the learned plan.
%
execute_plan(_M,true,Ms,Ms):-
        !.
execute_plan(M,(L,Ls),Acc,Ms):-
        execute_plan(M,L,Acc,Acc_)
        ,execute_plan(M,Ls,Acc_,Ms).
execute_plan(M,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,B)
        ,extract_coords(L,Acc,Acc_)
        ,execute_plan(M,B,Acc_,Ms).


%!      safe_clause(?Head,-Body) is nondet.
%
%       Variant of clause/2 avoiding calling built-ins.
%
%       Head is the head of a clause in the program database.
%
%       Body is the body of the clause, or the atom 'true' if Head is
%       the head of a built-in or library predicate.
%
%       Accessing built-ins by a call to clause/2, as in a
%       meta-interpreter like the one in execute_plan/3 raises
%       permission errors. This variant of clause/2 ensures that
%       built-ins (and predicates defined in libraries) are handed off
%       to Prolog instead. Other predicates, particularly learned plans,
%       are beheaded by clause/2 and their body literals returned.
%
safe_clause(M,L,true):-
        built_in_or_library_predicate(L)
        ,!
        ,call(M:L).
safe_clause(M,L,B):-
        clause(M:L,B).


%!      extract_coords(+Literal,+Acc,-Coords) is det.
%
%       Extract a coordinate pair from a move Literal.
%
%       Literal is a literal of a move predicate, a dyadic predicate
%       where the first argument is a list representing the world-state
%       before the move, and the second argument is a list representing
%       the world-state after the move.
%
%       Acc is the accumulator of coordinates passed-in by
%       execute_plan/3. Each element of this list is a triple X/Y:D
%       where X/Y are the actual coordinates and D is a symbol denoting
%       the direction of the move (according to the move action): 'u'
%       for up, 'd' for down, 'l' for left and 'r' for right.
%
%       Coords is the list of coordinates in Acc, updated with the
%       coordinates of the move represented by Literal.
%
extract_coords(L,Acc,Acc_2):-
        L =.. [Mv,S1,S2]
        ,move_direction(Mv,D)
        ,S1 = [Id,X1/Y1|_]
        ,S2 = [Id,X2/Y2|_]
        ,include_coords(X1/Y1:D,Acc,Acc_1)
        ,include_coords(X2/Y2:D,Acc_1,Acc_2)
        ,!.
extract_coords(_L,Acc,Acc).


%!      move_direction(?Move,?Direction) is semidet.
%
%       Mapping between Move predicates and Direction symbols.
%
%       Used by extract_coords/3 to construct a coordinate term.
%
%       @tbd These should not be hard-coded. We might want to change
%       them alter on.
%
move_direction(move_down, d).
move_direction(move_up, u).
move_direction(move_left, l).
move_direction(move_right, r).


%!      include_coords(+Coordinates,+Acc,-New) is det.
%
%       Decide whether to add a pair of Coordinates to an accumulator.
%
%       Coordinates is a triple X/Y:D, where X/Y a pair of coordinates
%       and D a direction of movement, as created by extract_coords/3.
%
%       Acc is the accumulator of coordinates:move triples accumulated
%       during execution of a plan, by execute_plan/4.
%
%       New is the accumulator in Acc updated with the given
%       Coordinates, if Coordinaes is ground and it is not already in
%       Acc.
%
%       The motivation for this predicate is to avoid adding
%       already-added coordinates to the accumulator, as a small
%       optimisation of path tracing.
%
include_coords(X/Y:D,Acc,[X/Y:D|Acc]):-
% Start and end are already ground so this excludes them
%        \+ ground(X/Y:D)
        ground(X/Y:D)
        ,\+ memberchk(X/Y,Acc)
        ,!.
include_coords(_X/_Y:_D,Acc,Acc).



%!      print_path(+Id,+Coords) is det.
%
%       Pretty-print a path through a maze.
%
%       Variant of print_maze/2, printing move symbols along with map
%       tiles.
%
%       @tbd This is very copy-pasta of print_maze/2. Couldn't we
%       abstract the functionality a bit so we don't have two different
%       predicates to print a maze with and without moves?
%
print_path(W,Id,CS):-
        generator_configuration:primitives_file(_P,M)
        ,M:maze(Id,Max_x-Max_y,Ms)
        ,maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(move_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        ,print_tile(W,Xt/Yt,T,CS)
                        )
                       )
                ,nl
                )
               ).



%!      print_tile(+What,+Coordinates,+Tile,+Directions) is det.
%
%       Print a map tile which may be a terrain or move symbol.
%
%       What is one of [tiles, coordinates, both], denoting what is to
%       be printed.
%
%       Coordinates is a pair X/Y, the coordinates of the map tile
%       to print.
%
%       Tile is the symbol of the tile at the given coordinates.
%
%       Directions is a list of triples X/Y:D, as generated by
%       extract_coords/3, where X/Y are the coordinates of the current
%       tile, iff the tile was traversed by a path, and D is the
%       direction in which the tile was traversed.
%
%       This predicate prints tile symbols with a call to ansi_format/3.
%       The attributes and format string used in the call are determined
%       by tile/5 (attributes) and mapping/2 (format).
%
print_tile(W,X/Y,_T,CS):-
        theme(Th)
        ,memberchk(X/Y:D,CS)
        ,!
        ,mapping(W,F)
        ,tile(D,Th,As,S)
        ,ansi_format(As,F,[X/Y,S]).
print_tile(W,X/Y,T,_CS):-
        theme(Th)
        ,mapping(W,F)
        ,tile(T,Th,As,S)
        ,ansi_format(As,F,[X/Y,S]).


%!      mapping(?What,?Format) is semidet.
%
%       Mapping between tile printing style and formatting.
%
%       How is one of: [tiles, coordinates, both], which prints what it
%       says.
%
%       Format is a format atom to be used when printing the thing said
%       in What.
%
mapping(tiles,'~i~w ').
mapping(coordinates,'~w~i ').
mapping(both,'~w.~w ').


%!      tile(?Token,?Theme,?Style,?Symbol) is semidet.
%
%       Mapping between tiles and their printing elements.
%
%       Token is the symbol of a tile, or a primitive move.
%
%       Theme is the atomic identifier of a printing theme, as defined
%       in theme/1.
%
%       Style is a list of attributes passed to the second
%       argument of ansi_format/3, that determines how Tile will be
%       printed by print_tile/4.
%
%       Symbol is the symbol to be printed. This allows arbitrary
%       mapping between the representation of map tiles and move
%       symbols, and printing characters.
%
tile(s,text,[fg(cyan)],s).
tile(f,text,[fg(green)],f).
tile(w,text,[fg(red)],w).
tile(e,text,[bold,fg(green)],e).
tile(@,text,[fg(yellow)],@).
tile(u,text,[fg(yellow)],˄).
tile(d,text,[fg(yellow)],˅).
tile(r,text,[fg(yellow)],˃).
tile(l,text,[fg(yellow)],˂).
% ASCII box drawings
tile(s,boxes,[fg(cyan)],█).
tile(f,boxes,[fg(green)],□).
tile(w,boxes,[fg(red)],■).
tile(e,boxes,[bold,fg(green)],█).
tile(@,boxes,[bold,fg(yellow)],@).
tile(u,boxes,[bold,fg(yellow)],▲).
tile(d,boxes,[bold,fg(yellow)],▼).
tile(r,boxes,[bold,fg(yellow)],►).
tile(l,boxes,[bold,fg(yellow)],◄).
