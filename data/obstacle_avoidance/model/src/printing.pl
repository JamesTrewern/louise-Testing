:-module(printing,[write_primitives/0
                  ,write_primitives/1
                  ,write_primitives/2
                  ,write_primitives/3
                  ]).

:-use_module(model_src(state_operations)).
:-use_module(model_root(specification)).

/** <module> Predicates to print vector access primitives to a file.
*/


%!      write_primitives is det.
%
%       Generate and print a file of state vector access primitives.
%
%       As write_primitives/1 but takes the names of terms to be
%       generated and the path of the output file from the
%       configuration.
%
write_primitives:-
        vector_specification(Ss)
        ,write_primitives(Ss).



%!      write_primitives(+Spec) is det.
%
%       Generate and print a file of state vector access primitives.
%
%       As write_primitives/2 but takes the path for the output file
%       from the configuration.
%
%       Spec is the state-vector specification. This is passed to
%       term_access/3. See that predicate for explanation.
%
write_primitives(Vs):-
        configuration:primitives_file(F)
        ,write_primitives(F,Vs).



%!      write_primitives(+Filename,+Spec) is det.
%
%       Generate and print a file of state vector access primitives.
%
%       Filename is an atom or compound term, a representation of a file
%       name as understood by SWI-Prolog's open/4.
%
%       Spec is the state-vector specification. This is passed to
%       term_access/3. See that predicate for explanation.
%
write_primitives(F,Vs):-
        access_level(Vs,Ts,Ls)
        ,term_access(Ts,inspect,Ts_I)
        ,argument_access(Ts,inspect,Args_I)
        ,list_access(Ls,inspect,Ls_I)
        ,list_arguments_access(Ls,inspect,Largs_I)
        ,term_access(Ts,modify,Ts_M)
        ,argument_access(Ts,modify,Args_M)
        ,list_arguments_access(Ls,modify,Largs_M)
        ,Stp = (expand_file_search_path(F,E),
                open(E,write,Strm,[alias(output_file)
                                  ,close_on_abort(true)
                                  ])
               )
        ,G = (write_module_header(F,Strm)
             ,write_module_imports(Strm)
             ,write_access_preds(Strm,Ts_I)
             ,nl(Strm)
             ,write_access_preds(Strm,Args_I)
             ,nl(Strm)
             ,write_access_preds(Strm,Ls_I)
             ,nl(Strm)
             ,write_access_preds(Strm,Largs_I)
             ,nl(Strm)
             ,write_access_preds(Strm,Ts_M)
             ,nl(Strm)
             ,write_access_preds(Strm,Args_M)
             ,nl(Strm)
             ,write_access_preds(Strm,Largs_M)
             )
        ,C = close(Strm)
        ,setup_call_cleanup(Stp,G,C).



%!      access_level(+Terms,-Term_Level,-List_level) is det.
%
%       Split a list of state-vector Terms by access level.
%
%       "Access level" refers to the level of nesting of a term within
%       another term, which is also the level at which we must descend
%       in order to access a term, and its arguments.
%
%       There are two "access levels" known: one top-level for terms
%       that are found in the state-vector, the "term-level"; and one
%       inner-level for terms that are (only?) found as elements of
%       lists that are arguments of term-level terms.
%
%       The motivation for this predicate is to split the list of
%       state_vector_term/2 atoms, in Terms, into two: one for the
%       Term_level terms, and one for the List_level, so that they can
%       each be handled according to their correct access level.
%
access_level(Vs,Ts,Ls):-
        access_level(Vs,[],Ts,[],Ls).

access_level([],Ts_Acc,Ts,Ls_Acc,Ls):-
        maplist(reverse,[Ts_Acc,Ls_Acc],[Ts,Ls])
        ,!.
access_level([state_vector_term(N,Ss)|Vs],Ts_Acc,Ts,Ls_Acc,Ls):-
        specification:list_type(N)
        ,!
        ,access_level(Vs,Ts_Acc,Ts,[state_vector_term(N,Ss)|Ls_Acc],Ls).
access_level([state_vector_term(N,Ss)|Vs],Ts_Acc,Ts,Ls_Acc,Ls):-
        access_level(Vs,[state_vector_term(N,Ss)|Ts_Acc],Ts,Ls_Acc,Ls).



%!      write_primitives(+Filename,+Spec,+How) is det.
%
%       Generate and print a file of state vector access primitives.
%
%       As write_primitives/2 but specifies the kind of access, in
%       How.
%
%       Filename is an atom or compound term, a representation of a file
%       name as understood by SWI-Prolog's open/4.
%
%       Spec and How are the state-vector specification and type of
%       access predicate to print, respectively. Those are passed to
%       term_access/3 and argument_access/3. See those predicates for
%       descriptions.
%
%       @tbd Possibly no longer needed? Just for debugging purposes?
%       Doesn't handle list-level access predicates.
%
write_primitives(F,V,H):-
        term_access(V,H,As)
        ,argument_access(V,H,Args)
        ,Stp = (expand_file_search_path(F,E),
                open(E,write,Strm,[alias(output_file)
                                  ,close_on_abort(true)
                                  ])
               )
        ,G = (write_module_header(Strm)
             ,write_access_preds(Strm,As)
             ,nl(Strm)
             ,write_access_preds(Strm,Args)
             )
        ,C = close(Strm)
        ,setup_call_cleanup(Stp,G,C).


%!      write_module_header(+File,+Stream) is det.
%
%       Write the module header for the state-vector access module.
%
%       The name of the Prolog module is taken from the Filename for the
%       output file associated with the output Stream.
%
write_module_header(F,Strm):-
        expand_file_search_path(F,E)
        ,file_base_name(E,N)
        ,file_name_extension(B,_Ext,N)
        ,D = (:-module(B,[inspect/3,inspect/4,modify/4,modify/5]))
        ,portray_clause(Strm,D)
        ,nl(Strm).


%!      write_module_imports(+Stream) is det.
%
%       Write a use_module/2 directive to a state-vector access module.
%
%       Used to import predicates used in modify/5 terms.
%
write_module_imports(Strm):-
        writeln(Strm,':-use_module(model_src(types)).\n').


%!      write_access_preds(+Stream,+Clauses) is det.
%
%       Write Clauses of state vector access predicates to Stream.
%
write_access_preds(_Strm,[]):-
        !.
write_access_preds(Strm,[C|Cs]):-
        what_access_clause(C,[H,F|Args])
        ,write_access_comment(Strm,[H,F|Args])
        ,write_access_clauses([C|Cs],Strm,_).

%!      write_access_clauses(+Clauses,+Stream,?Term) is det.
%
%       Write Clauses of access predicates for each Term to a Stream.
%
write_access_clauses([],_,_):-
        !.
write_access_clauses([C|Cs],Strm,F):-
        what_access_clause(C,[_H,F|_Args])
        ,!
        ,write_access_clause(Strm,C)
        ,write_access_clauses(Cs,Strm,F).
write_access_clauses([C|Cs],Strm,_):-
        what_access_clause(C,[H,F|Args])
        ,nl(Strm)
        ,write_access_comment(Strm,[H,F|Args])
        ,write_access_clause(Strm,C)
        ,write_access_clauses(Cs,Strm,F).


%!      what_access_clause(+Clause,-Arguments) is det.
%
%       Determine the kind and Arguments of an access predicate Clause.
%
what_access_clause(C,[How,F|As]):-
        C =.. [:-,H,_B]
        ,!
        ,H =.. [How,F|As].
what_access_clause(C,[H,F|As]):-
        C =.. [H,F|As].


%!      write_access_clause(+Stream,+Clause) is det.
%
%       Write one Clause of an access predicate to a Stream.
%
write_access_clause(Strm,C):-
        portray_clause(Strm,C,[spacing(standard)
                              ]).

%!      write_access_comment(+Stream,+Head) is det.
%
%       Write a comment describing an access clause written to Stream.
%
write_access_comment(Strm,Hs):-
        access_kind(Hs,term_inspector(F))
        ,!
        ,format(Strm,'% inspect/3 clause for ~w term.~n',[F]).
write_access_comment(Strm,Hs):-
        access_kind(Hs,list_inspector(F,_El))
        ,!
        ,format(Strm,'% inspect/4 clauses for ~w terms in a list.~n',[F]).
write_access_comment(Strm,Hs):-
        access_kind(Hs,element_inspector(F,_El))
        ,!
        ,format(Strm,'% inspect/4 clauses for arguments of ~w terms.~n',[F]).
write_access_comment(Strm,Hs):-
        access_kind(Hs,argument_inspector(F,_Arg))
        ,!
        ,format(Strm,'% inspect/4 clauses for arguments of ~w term.~n',[F]).
write_access_comment(Strm,Hs):-
        access_kind(Hs,term_modifier(F))
        ,!
        ,format(Strm,'% modify/4 clause for ~w term.~n',[F]).
write_access_comment(Strm,Hs):-
        access_kind(Hs,argument_modifier(F,_Arg))
        ,!
        ,format(Strm,'% modify/5 clauses for arguments of ~w term.~n',[F]).


%!      access_kind(+Head,-Level) is semidet.
%
%       Determine the kind of an access predicate from its Head.
%
access_kind([inspect,F,_T,_Vs],term_inspector(F)).
access_kind([modify,F,_T,_Vs1,_Vs2],term_modifier(F)).
access_kind([inspect,F,El,_Arg,_Vs],list_inspector(F,El)):-
        !
        ,specification:list_type(F)
        ,compound(El).
access_kind([inspect,F,El,_Arg,_Vs],element_inspector(F,El)):-
        !
        ,specification:list_type(F)
        ,atomic(El).
access_kind([inspect,F,_Op,Arg,_Vs],argument_inspector(F,Arg)).
access_kind([modify,F,_Op,Arg,_Vs1,_Vs2],argument_modifier(F,Arg)).

