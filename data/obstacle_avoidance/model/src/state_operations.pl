:-module(state_operations, [term_access/3
                           ,argument_access/3
                           ,list_access/3
                           ,list_arguments_access/3
                           ]).

:-use_module(model_src(types)).
:-use_module(model_root(specification)).

/** <module> Generate predicates operating on state vectors.

*/


%!      term_access(+Spec,+How,-Access) is det.
%
%       Generate predicates to inspect or modify state vector terms.
%
%       Spec is a state-vector specification where each element is a
%       state_vector_term/2 atom, as described in state_vector.pl. Each
%       state_vector_term/2 atom is of the form: state_vector_term(F,S)
%       where F is the functor name of an atom in a state vector and S
%       is a list describing the arguments of that atom, as N:T pairs,
%       with N the name and T the type of each argument.
%
%       How is one of "inspect" and "modify", determining the kind of
%       state vector access predicate whose clauses are to be
%       constructed. If How is "inspect", that means clauses of an
%       inspection predicate should be created, which only retrieve the
%       value of an atom in a state vector, but do not modify it. If How
%       is "modify" generated clauses are of a predicate that modifies
%       the values of state vector atoms.
%
%       Access is a list of clauses of the state access predicate
%       specificed in How. Thus, each element of Access is a clause of a
%       predicate inspect/3 or modify/3, which each do what it sounds
%       like.
%
%       Each clause of an inspect/3 predicate is of the form:
%       inspect(Term,Value,Vector) where Vector is a state-vector, Term
%       is the functor name of a term in the state Vector and Value is
%       bound to the named term in the state vector.
%
%       Example use (with manual spacing for readability):
%       ==
%       ?- _Ss = [state_vector_term(pos,[x:coord,y:coord,theta:ang])
%       ,state_vector_term(heading,[x:coord,y:coord,theta:ang])]
%       ,_H = inspect
%       ,term_access(_Ss, _H, _Ts)
%       ,forall(member(T,_Ts),writeln(T)).
%
%       inspect(pos,pos(X,Y,Theta),[pos(X,Y,Theta),_])
%       inspect(heading,heading(X,Y,Theta),[_,heading(X,Y,Theta)])
%       true.
%       ==
%
%       Note how in the resulting inspect/3 clauses each element of the
%       state-vector that is not bound to the second argument of the
%       atom is replaced with an "_" underscore. These underscores are
%       in fact '$VAR'('_') terms, that are printed as underscores- but
%       we want to use those inspect/2 terms to print them out in a
%       file, and this does the job.
%
%       @tbd: Describe clauses of modify/3
%
term_access(Ss,inspect,Ts):-
        !
        ,vars_underscores(Ss,Us)
        ,term_access(Ss,inspect,1,Us,[],Ts).
term_access(Ss,modify,Ts):-
        state_vars(Ss,Vs)
        ,term_access(Ss,modify,1,Vs,[],Ts).



%!      term_access(+Spec,+How,+N,+Vars,+Acc,-Access) is det.
%
%       Business end of term_access/3.
%
%       Spec, How and Access are as in term_access/3.
%
%       N is the index into the state-vector. Each i'th index is to be
%       replaced with an access term for the i'th term in the vector.
%       Remaining terms are left as variables named after each term.
%
%       Vars is a list of length equal to Spec, minus two. Each element
%       of that list is a Skolem constant '$VAR'('_'), which will be
%       printed out as an underscore, "_", thus ignoring the term in
%       that position in the state-vector.
%
%       Combined, N and Vars, are used to replace each term in the
%       state-vector with an access term while leaving all the other
%       terms as underscores, to avoid warnings of singletons and also
%       to reduce visual clutter when reading the generated access
%       predicates' clauses.
%
%       Acc is the accumulator of state-vector access terms.
%
term_access([],_H,_N,_Vs,Acc,Ts):-
        reverse(Acc,Ts)
        ,!.
term_access([S|Ss],H,N,Vs,Acc,Bind):-
        H == inspect
        ,!
        ,access_term(S,F-A)
        ,nth1(N,Vs_,A,Vs)
        ,T =.. [H,F,A,Vs_]
        ,succ(N,M)
        ,term_access(Ss,H,M,Vs,[T|Acc],Bind).
term_access([S|Ss],H,N,Vs,Acc,Bind):-
        H == modify
        ,access_term(S,F-A)
        ,atom_skolem_constant(F,F_)
        % Create a don't-care copy of F.
        ,atom_to_uppercase(F,FF)
        ,atom_concat('_',FF,Fu)
        ,atom_skolem_constant(Fu,A_)
        ,selectchk(F_,Vs,A_,In)
        ,selectchk(F_,Vs,A,Out)
        ,T =.. [H,F,A,In,Out]
        ,succ(N,M)
        ,term_access(Ss,H,M,Vs,[T|Acc],Bind).



%!      access_term(+State,-Access) is det.
%
%       Construct an Access term for a State vector term.
%
%       State is a state_vector_term/2 atom that defines the structure
%       of a term in a state vector.
%
%       Access is a term N-F, where each N is the name of a term in the
%       state vector and each F is a term that unifies with that
%       term and with every argument turned into a variable named
%       according to the name of the corresponding term defined in the
%       second argument of State.
%
%       The motivation for this is to create a term that can be used to
%       access an atom in a state vector like indexing into the state
%       vector, but by unification rather than positional indexing.
%
%       Example use (with manual spacing):
%       ==
%       ?- _S = state_vector_term(position,[x:coordinate,y:coordinate,theta:angle])
%       ,state_operations:access_term(_S, T).
%
%       T = position-position(X, Y, Theta).
%       ==
%
access_term(state_vector_term(N,Ss),N-T):-
        structure_vars(Ss,Vs)
        ,T =.. [N|Vs].


%!      structure_vars(+Structure,-Variables) is det.
%
%       Convert a state vector term Structure to a list of Variables.
%
%       Structure is a representation of the structure of an atom in a
%       state vector given as a list of key-value pairs of the form
%       N:T, where each N is the name of a term in the atom and each T
%       is the type of that term.
%
%       Variables is a list of variables with length equal to Structure
%       and where each element is the upcased Name in the N:T pair in
%       the same position in Structure.
%
%       The poitn of this predicate is to turn a list of structure N:T
%       terms into variables that can be used to easily access each term
%       represented by that pair.
%
%       Example of use:
%       ==
%       ?- structure_vars([x:coordinate,y:coordinate,theta:angle],Vs).
%       Vs = [X, Y, Theta].
%       ==
%
structure_vars(Ss,Vs):-
        structure_vars(Ss,[],Vs).

%!      structure_vars(+Structure,+Acc,-Vars) is det.
%
%       Business end of structure_vars/2.
%
structure_vars([],Acc,Vs):-
        reverse(Acc,Vs)
        ,!.
structure_vars([N:_T|Ss],Acc,Bind):-
        atom_to_uppercase(N,N_)
        ,structure_vars(Ss,['$VAR'(N_)|Acc],Bind).


%!      atom_to_uppercase(+Atom,-Uppercase) is det.
%
%       Turn the first character of an atom to uppercase.
%
atom_to_uppercase(A,U):-
        atom_chars(A,[C|Cs])
        ,upcase_atom(C,C_)
        ,atom_chars(U,[C_|Cs]).



%!      argument_access(Spec,How,-Access) is det.
%
%       Generate predicates to access state vector terms' arguments.
%
%       This predicate is similar to term_access/3 but instead generates
%       clauses of predicates inspect/4 and modify/4, that access the
%       arguments of state vector terms.
%
%       Spec is a state-vector specification, as in term_access/3.
%
%       How is one of "inspect" or "modify", with meaning as described
%       in term_access/3.
%
%       Access is a list of inspect/4 or modify/5 clauses.
%
%       inspect/4 clauses are unit clauses of the form inspect(F,A,V,Ss)
%       where: F is the functor name of a term in the state-vector Ss
%       constructed according to Spec, A is the name of an argument in
%       that term, V is the value of that argument in the state-vector
%       and Ss is that state-vector.
%
%       modify/5 terms are definite clauses of the form modify(F,M,A,In,Out):-
%       Body, where: F is the functor name of a term in the state-vector
%       In, constructed according to Spec, M is a modifier for the
%       argument A of that vector, and Out is the vector In with the
%       argument A modified by M.
%
%       Example of use (with manual spacing for readability):
%       ==
%       ?- _Ss = [state_vector_term(pos,[x:coord,y:coord,theta:ang])
%       ,state_vector_term(heading,[x:coord,y:coord,theta:ang])]
%       ,_H = inspect
%       ,argument_access(_Ss, _H, _Ts)
%       ,forall(member(T,_Ts),writeln(T)).
%
%       inspect(pos,x,X,[pos(X,_,_),_])
%       inspect(pos,y,Y,[pos(_,Y,_),_])
%       inspect(pos,theta,Theta,[pos(_,_,Theta),_])
%       inspect(heading,x,X,[_,heading(X,_,_)])
%       inspect(heading,y,Y,[_,heading(_,Y,_)])
%       inspect(heading,theta,Theta,[_,heading(_,_,Theta)])
%       true.
%
%       ?- _Ss = [state_vector_term(pos,[x:coordinate,y:coordinate,theta:angle])
%       ,state_vector_term(heading,[x:coordinate,y:coordinate,theta:angle])]
%       ,_H = modify
%       ,argument_access(_Ss,_H,_Ms)
%       ,forall(member(M,_Ms),writeln(M)).
%
%       modify(pos,increment,x,[pos(X,Y,Theta),Heading],[pos(X_,Y,Theta),Heading]):-
%              increment_float(X,X_)
%       modify(pos,decrement,x,[pos(X,Y,Theta),Heading],[pos(X_,Y,Theta),Heading]):-
%              decrement_float(X,X_)
%       modify(pos,increment,y,[pos(X,Y,Theta),Heading],[pos(X,Y_,Theta),Heading]):-
%              increment_float(Y,Y_)
%       modify(pos,decrement,y,[pos(X,Y,Theta),Heading],[pos(X,Y_,Theta),Heading]):-
%              decrement_float(Y,Y_)
%       modify(pos,increment,theta,[pos(X,Y,Theta),Heading],[pos(X,Y,Theta_),Heading]):-
%              increment_float(Theta,Theta_)
%       modify(pos,decrement,theta,[pos(X,Y,Theta),Heading],[pos(X,Y,Theta_),Heading]):-
%              decrement_float(Theta,Theta_)
%       modify(heading,increment,x,[Pos,heading(X,Y,Theta)],[Pos,heading(X_,Y,Theta)]):-
%              increment_float(X,X_)
%       modify(heading,decrement,x,[Pos,heading(X,Y,Theta)],[Pos,heading(X_,Y,Theta)]):-
%              decrement_float(X,X_)
%       modify(heading,increment,y,[Pos,heading(X,Y,Theta)],[Pos,heading(X,Y_,Theta)]):-
%              increment_float(Y,Y_)
%       modify(heading,decrement,y,[Pos,heading(X,Y,Theta)],[Pos,heading(X,Y_,Theta)]):-
%              decrement_float(Y,Y_)
%       modify(heading,increment,theta,[Pos,heading(X,Y,Theta)],[Pos,heading(X,Y,Theta_)]):-
%              increment_float(Theta,Theta_)
%       modify(heading,decrement,theta,[Pos,heading(X,Y,Theta)],[Pos,heading(X,Y,Theta_)]):-
%              decrement_float(Theta,Theta_)
%       true.
%       ==
%
%       @tbd: the above examples won't work anymore because predicates
%       called by this one now check that the input term names are
%       fluents, for one thing. Make sure the input/output examples
%       above work with new code.
%
argument_access(Ss,inspect,As):-
        !
        ,vars_underscores(Ss,Us)
        ,argument_access(Ss,inspect,1,Us,[],As).
argument_access(Ss,modify,As):-
        state_vars(Ss,Vs)
        ,argument_access(Ss,modify,1,Vs,[],As).


%!      vars_underscores(+List,-Underscores) is det.
%
%       Create a list of Underscores corresponding to elements in List.
%
%       Example use:
%       ==
%       ?- state_operations:vars_underscores([X,Y,Z],Us).
%       Us = [_, _].
%       ==
%
%       @tbd This only makes sense in the context of the predicates that
%       call it, specifically the beginning of iteration from the index
%       2. Its reuse made me abstract it away to its own predicate but
%       perhaps it's better to not use it and instead copy-paste the
%       findall/3 loop and length/2 call, the better to clarify how the
%       underscores work with respect to what they replace?
%
vars_underscores(Vs,Us):-
        length(Vs,N)
        ,findall('$VAR'('_')
                ,between(2,N,_)
                ,Us).


%!      state_vars(+State,-Variables) is det.
%
%       Create a list of Variables named after a list of State terms.
%
%       Example use:
%       ==
%       ?- _Ss = [state_vector_term(pos,[x:coordinate,y:coordinate,theta:angle])
%       ,state_vector_term(heading,[x:coordinate,y:coordinate,theta:angle])]
%       ,state_operations:state_spec_vars(_Ss,Vs).
%
%       Vs = [Pos, Heading].
%       ==
%
%       Note that Pos and Heading are really the terms '$VAR'('Pos') and
%       '$VAR'('Heading'), because we want to print them out so that
%       they can be read back as variables.
%
state_vars(Ss,Vs):-
        findall(S
               ,(member(state_vector_term(T,_Spec),Ss)
                ,atom_skolem_constant(T,S)
                )
               ,Vs).


%!      atom_skolem_constant(+Atom,-Constant) is det.
%
%       Convert an Atom to a synonymous skolem Constant.
%
%       Atom is an atom, the name of a term or argument in a
%       state-vector.
%
%       Constant is a term '$VAR'(Atom), used to print variables with
%       the same name as Atom, but with an uppercase first letter, so
%       that they can be read back as variables.
%
%       Example use (with manual spacing):
%       ==
%       ?- _A = abc, state_operations:atom_skolem_constant(_A,_C)
%       ,writeln(_C)
%       ,write_canonical(_C).
%
%       Abc
%       '$VAR'('Abc')
%       true.
%       ==
%
atom_skolem_constant(A,'$VAR'(U)):-
        atom_to_uppercase(A,U).



%!      argument_access(+Spec,+How,+N,+Acc,-Args) is det.
%
%       Business end of argument_access/3.
%
%       Acc is the accumulator of argument access clauses.
%
argument_access([],_H,_N,_Us,Acc,As):-
        reverse(Acc,As)
        ,!.
argument_access([S|Ss],H,N,Us,Acc,Bind):-
        H == inspect
        ,!
        ,argument_access_terms(S,F-As)
        ,argument_access_acc(As,F,H,N,Us,Acc,Acc_)
        ,succ(N,N_)
        ,argument_access(Ss,H,N_,Us,Acc_,Bind).
argument_access([S|Ss],H,N,Vs,Acc,Bind):-
        H == modify
        ,state_modify_term_args(S,F-As)
        ,argument_access_acc(As,F,H,N,Vs,Acc,Acc_)
        ,succ(N,N_)
        ,argument_access(Ss,H,N_,Vs,Acc_,Bind).


%!      argument_access_terms(+State,-Access) is det.
%
%       Construct Access terms for state vector terms' arguments.
%
%       State is a state_vector_term/2 atom that defines the structure
%       of a term in a state vector. A state_vector/2 atom is of the
%       form: state_vector_term(T,Ss) where Ss is a list of key-value
%       pairs N:T, where each N is the name of a term in a state vector
%       and each T is the type of that term.
%
%       Access is a list of lists where each sublist is of the form
%       [N,V,A]. In each of those sublists, N is the key of a key-value
%       pair N:T in the second argument of state_vector_term/2; V is a
%       variable with the same name as N (but starting with an uppercase
%       letter) and A is the state-vector term represented by State,
%       with each argument apart from V replaced by underscores, '_'
%       (or, rather, the skolem constant '$VAR'('_') since again we just
%       want to print those terms.
%
%       Example  use (with spacing added manually for readability):
%       ==
%       ?- _S = state_vector_term(pos,[x:coord,y:coord,theta:ang])
%       ,state_operations:argument_access_terms(_S,As).
%
%       As = [
%        [x, X, pos(X, _, _)]
%       ,[y, Y, pos(_, Y, _)]
%       ,[theta, Theta, pos(_, _, Theta)]
%       ].
%       ==
%
%       The motivation for this predicate is to create access terms for
%       specific arguments of state-vector terms.
%
argument_access_terms(state_vector_term(N,Ss),N-As):-
        access_term(state_vector_term(N,Ss),N-T)
        ,term_access_args(T,Ss,As).

% TODO: document.
term_access_args(T,Ss,Args):-
        T =.. [F|As]
        ,vars_underscores(As,Us)
        ,term_access_args(As,F,1,Us,Ss,[],Args).

term_access_args([],_F,_N,_Us,_Ss,Acc,As):-
        reverse(Acc,As)
        ,!.
term_access_args([A|As],F,N,Us,[Nm:_T|Ss],Acc,Bind):-
        nth1(N,Us_,A,Us)
        ,T =.. [F|Us_]
        ,succ(N,N_)
        ,term_access_args(As,F,N_,Us,Ss,[[Nm,A,T]|Acc],Bind).


%!      state_modify_term_args(+Spec,-Modify) is det.
%
%       Construct Modify terms for state vector terms' arguments.
%
%       Spec is a state_vector_term/2 atom that defines the structure of
%       a term in a state vector. A state_vector/2 atom is of the form:
%       state_vector_term(T,Ss) where Ss is a list of key-value pairs
%       N:T, where each N is the name of a term in a state vector and
%       each T is the type of that term.
%
%       Modify is a pair F-Ms, where F is the functor of the
%       state-vector term specifided in Spec, and Ms is a list of lists
%       where each sublist is a modifier specification for that term.
%
%       Each element of Ms is a list of the form [N:T,A,T,A_,T_], where
%       N:P is a pair name:type, of a named term in a state-vector, A is
%       the name of an argument in that term, T is the named term, A_ is
%       the same as A but with an underscore appended to indicated it is
%       modified and T_ is the same thing, but for T.
%
%       Example of use (manual spacing blah blah):
%       ==
%       ?- _S = state_vector_term(position,[x:coordinate,y:coordinate,theta:angle])
%       ,state_operations:state_modify_term_args(_S,F-_As)
%       ,forall(member(A,_As),writeln(A)).
%
%       [x:coordinate,X,position(X,Y,Theta),X_,position(X_,Y,Theta)]
%       [y:coordinate,Y,position(X,Y,Theta),Y_,position(X,Y_,Theta)]
%       [theta:angle,Theta,position(X,Y,Theta),Theta_,position(X,Y,Theta_)]
%       F = position.
%       ==
%
state_modify_term_args(state_vector_term(N,Ss),F-Args):-
        access_term(state_vector_term(N,Ss),N-T)
        ,args_modified(Ss,Ms)
        ,T =.. [F|As]
        ,term_modifier_args(As,As,F,T,Ss,Ms,[],Args).


%!      args_modified(+Structure,-Modifiers) is det.
%
%       Construct a list of Modified terms for a vector term Structure.
%
%       As arg_modified/2 but takes in a list of Structures and returns
%       a list of Modifiers.
%
%       Example:
%       ==
%       ?- _Ss = [x:coord,y:coord,theta:ang], state_operations:args_modified(_Ss,Ms).
%       Ms = [(X->X_), (Y->Y_), (Theta->Theta_)].
%       ==
%
args_modified(Ss,Ms):-
        findall(M
               ,(member(A,Ss)
                ,arg_modified(A,M)
                )
               ,Ms).


%!      arg_modified(+Argument,-Modifier) is det.
%
%       Construct a Modified term for an Argument.
%
%       Argument is a key-value pair N:T, found in a state_vector_term/2
%       specification, where N is the atomic name of an argument in a
%       state-vector term and T its type.
%
%       Modified is a term V->V_ where V is a Skolem constant with the
%       same name as N, capitalised, and V_ is the same as V but with an
%       appended underscore to indicate it is a modified version (so
%       it's V and V-prime; but we can't easily add quotation marks in
%       Prolog).
%
%       Example:
%       ==
%       ?- _A = x:coord, state_operations:arg_modified(_A,M).
%       M = (X->X_).
%       ==
%
%
arg_modified(N:_T,C->C_):-
        atom_skolem_constant(N,C)
        ,atom_concat(N,'_',N_)
        ,atom_skolem_constant(N_,C_).


%!      term_modifier_args(+As,+As,+F,+T,+Ss,+Mods,+Acc,-Out) is det.
%
%       Replace each argument in a state-vector term with a modifier.
%
%       As is a list of arguments of a state-vector term. It is given
%       twice. The first instance is de-constructed from the head during
%       recursion to ensure each argument is processed. The other is
%       modified by replacing each of its elements with a modifier
%       version.
%
%       F is the functor name of the term for which a modifier clause is
%       to be created.
%
%       T is state-vector access term one of whose arguments is to be
%       modified. The modifier predicates' clauses that we want to
%       construct have two copies of that term in their head literals:
%       one is T, which is unmodified, the other is a version with one
%       argument changed by a "primed" (underscored_) version,
%       representing the modified argument.
%
%       Mods is the list of A->A_ argument modifiers returned by
%       args_modified/2.
%
%       Acc is the accumulator of modifiers.
%
%       Out is Acc updated with all modifiers for each argument in
%       As.
%
term_modifier_args([],_As,_F,_T,[],[],Acc,Args):-
        reverse(Acc,Args)
        ,!.
term_modifier_args([A|As],As_,F,T,[N:P|Ss],[A->A_|Ms],Acc,Bind):-
        selectchk(A,As_,A_,As_2)
        ,T_ =.. [F|As_2]
        ,term_modifier_args(As,As_,F,T,Ss,Ms,[[N:P,A,T,A_,T_]|Acc],Bind).



%!      argument_access_acc(+As,+Func,+How,+N,+Ts,+Acc,-Out) is det.
%
%       Accumulate predicates accessing arguments of state-vector terms.
%
%       As and Func are as in the pair F-Args, returned by
%       argument_access_terms/2.
%
%       How is one of "inspect" or "modify", the type of access atom to
%       create.
%
%       N is the index into the list of "_"'s representing the
%       state-vector. Each argument-access term is to be added at that
%       N, then the N updated.
%
%       Ts is a list representing the current state vector. Its elements
%       depend on the value of H.
%
%       If H is "inspect", Ts is the list of underscroes, "_".
%       Underscores are replaced with argument-access terms to make
%       state-vectors with only one argument of only one term "visible".
%
%       If H is "modify", Ts is a list of named variables representing
%       the terms in the state-vector, each of which is to be modified
%       by modify/5 terms.
%
%       Acc is the accumulator of state-vector argument access terms.
%
%       Out is the finished list of argument access terms, eventually to
%       be written to a file.
%
%       Example of use (manual spacing):
%       ==
%       ?- _Index = 2
%       ,_How = inspect
%       ,_Ts = ['$VAR'('_'),'$VAR'('_')]
%       ,_S = state_vector_term(pos,[x:coord,y:coord,theta:ang])
%       ,argument_access_terms(_S,_F-_As)
%       ,argument_access_acc(_As,_F,_How,_Index,_Ts,[],_Acc)
%       ,maplist(writeln,_Acc).
%
%       inspect(pos,theta,Theta,[_,pos(_,_,Theta),_])
%       inspect(pos,y,Y,[_,pos(_,Y,_),_])
%       inspect(pos,x,X,[_,pos(X,_,_),_])
%       true.
%       ==
%
%       @tbd Document accumulation of modify predicates.
%
argument_access_acc([],_F,_H,_N,_Us,Acc,Acc):-
        !.
argument_access_acc([[Nm,V,A]|As],F,H,N,Us,Acc,Acc_):-
        H == inspect
        ,!
        ,nth1(N,Us_,A,Us)
        ,T =.. [H,F,Nm,V,Us_]
        ,argument_access_acc(As,F,H,N,Us,[T|Acc],Acc_).
argument_access_acc([Ai|As],F,H,N,Vs,Acc,Bind):-
        H == modify
        ,argument_modifiers_acc(Ai,F,H,Vs,Acc,Acc_)
        ,argument_access_acc(As,F,H,N,Vs,Acc_,Bind).


%!      argument_modifiers_acc(+Mod,+F,+How,+Vars,+Acc,-Out) is det.
%
%       Accumulate predicates modifying arguments of state-vector terms.
%
%       Mod is a list of the form: [N:P,A,T,A_,T_] where N:P is a pair
%       name:type, of a named term in a state-vector. A is the name of
%       an argument in that term. T is the named term. A_ is same as A
%       but with an underscore appended to indicate it is modified. T_
%       is the same thing but for T.
%
%       F is the functor name of the term for which a modifier clause is
%       to be created.
%
%       How is always "modify". So what do we need it as a variable?
%
%       Vars is the list of named variables representing the terms in
%       the state-vector. Each of them is to be replaced by
%       access-and-modiy terms in the clauses of the modifier predicates
%       to be returned by this predicate.
%
%       Acc is the accumulator of modifier clauses, inherited from
%       state_vector_access_acc/6.
%
%       Out is the accumulator in Acc, updated with new modifier
%       clauses.
%
%       @tbd This predicate goes one level deeper in recursion over
%       the accumulator of access terms, in Acc, than the parent
%       predicate, state_vector_access_acc/6 when How is "inspect". That
%       is because there may be multiple modifiers for each argument in
%       a state-vector term. For example, an integer argument may need
%       an increment and a decrement modifier, and so on.
%
argument_modifiers_acc(_Mod,F,_H,_Vs,Acc,Acc):-
        \+ specification:fluent(F)
        ,!.
argument_modifiers_acc([Nm:P,A,T,A_,T_],F,H,Vs,Acc,Acc_):-
        findall(M
               ,modifier_symbol(P,M)
               ,Ms)
        ,arg_type_modifiers_acc(Ms,[Nm,A,T,A_,T_],F,H,Vs,Acc,Acc_).


%!      modifier_symbol(+Type,-Symbol) is det.
%
%       A symbol and arity for modifiers of a Type.
%
%       Needed to abstract over modifiers of different arities,
%       particularly dyadic and triadic.
%
modifier_symbol(T,M):-
        type_modifier(T,M).
modifier_symbol(T,M):-
        type_modifier(T,_P,M).


%!      arg_type_modifiers_acc(+Ms,+Arg,+F,+H,+Vs,+Acc,-Out) is det.
%
%       Business end of argument_modifiers_acc/7.
%
%       Handles the recursion over the terms to be added to Acc, to
%       produce Out.
%
%       Ms is a list of atoms representing modifiers for the
%       state-vector term argument in Arg. Each element of Ms is of the
%       form N:M/A, where N is an atom naming the operation performed
%       by the modifier, and M/A are the symbol and arity of a predicate
%       implementing the modifier.
%
%       Note that operation names, the N in N:M/A, are not unique and
%       are not necessarilyt he same as the symbols of modifier
%       predicates. For example, a modifier named "increment" may apply
%       both to a modifier defined as increment_int/2 and one defined as
%       increment_float/2. The combination of named terms, arguments and
%       modifier names however should be unique. Probably.
%
%       All other arguments are as in argument_modifiers_acc/7.
%
%       Example query:
%       ==
%       ?- arg_type_modifiers_acc(
%              [increment:increment_float/2,decrement:decrement_float/2]
%              ,[x, X, position(X, Y, Theta), X_, position(X_, Y, Theta)]
%              ,position
%              ,modify
%              ,[Position]
%              ,[]
%              ,_Acc)
%       ,maplist(portray_clause,_Acc).
%
%       modify(position, decrement, x, [position(A, B, C)], [position(D, B, C)]) :-
%           decrement_float(A, D).
%       modify(position, increment, x, [position(A, B, C)], [position(D, B, C)]) :-
%           increment_float(A, D).
%       true.
%       ==
%
%       @tbd: There is now quite a bit of code duplication in this
%       predicate. Can it be reduced? A little? Please?
%
arg_type_modifiers_acc([],_Arg,_F,_H,_Vs,Acc,Acc):-
        !.
arg_type_modifiers_acc([N:S/2|Ms],[Nm,A,T,A_,T_],F,H,[],Acc,Acc_):-
% Add inspectors for list-type terms' arguments.
        !
        ,Hd =.. [H,F,N,Nm,T,T_]
        ,Bd =.. [S,A,A_]
        ,arg_type_modifiers_acc(Ms,[Nm,A,T,A_,T_],F,H,[],[(Hd:-Bd)|Acc],Acc_).
arg_type_modifiers_acc([N:S/3|Ms],[Nm,A,T,A_,T_],F,H,[],Acc,Acc_):-
% Add modifiers for list-type terms' arguments.
        !
        ,atom_skolem_constant('Params',P)
        ,Arg =.. [Nm,P]
        ,Hd =.. [H,F,N,Arg,T,T_]
        ,Bd =.. [S,P,A,A_]
        ,arg_type_modifiers_acc(Ms,[Nm,A,T,A_,T_],F,H,[],[(Hd:-Bd)|Acc],Acc_).
arg_type_modifiers_acc([N:S/2|Ms],[Nm,A,T,A_,T_],F,H,Vs,Acc,Acc_):-
% Add inspectors for ordinary terms' arguments.
        !
        ,atom_skolem_constant(F,F_)
        ,selectchk(F_,Vs,T,In)
        ,selectchk(F_,Vs,T_,Out)
        ,Hd =.. [H,F,N,Nm,In,Out]
        ,Bd =.. [S,A,A_]
        ,arg_type_modifiers_acc(Ms,[Nm,A,T,A_,T_],F,H,Vs,[(Hd:-Bd)|Acc],Acc_).
arg_type_modifiers_acc([N:S/3|Ms],[Nm,A,T,A_,T_],F,H,Vs,Acc,Acc_):-
% Add modifiers for ordinary terms' arguments.
        atom_skolem_constant(F,F_)
        ,selectchk(F_,Vs,T,In)
        ,selectchk(F_,Vs,T_,Out)
        ,atom_skolem_constant('Params',P)
        ,Arg =.. [Nm,P]
        ,Hd =.. [H,F,N,Arg,In,Out]
        ,Bd =.. [S,P,A,A_]
        ,arg_type_modifiers_acc(Ms,[Nm,A,T,A_,T_],F,H,Vs,[(Hd:-Bd)|Acc],Acc_).



%!      list_access(+Spec,+How,-Access) is det.
%
%       Generate predicates to access elements of list-type terms.
%
%       Spec is a state-vector specification, as returned by
%       vector_specification/[1,2]. The elements of Spec should be
%       state-vector terms that are found as elements of list-type
%       arguments of other terms.
%
%       How is one of "inspect" or "modify" and determines the kind of
%       access implemented by the generated predicates. See
%       term_access/3 for more details.
%
%       Access is a list of clauses of the access predicate of the kind
%       specified in How.
%
%       Example use (manual spacing):
%       ==
%       ?- _H =inspect
%       ,findall(N,specification:list_type(N),_Ns)
%       ,vector_specification(_Ns,_Ss)
%       ,list_access(_Ss,_H,_As)
%       ,maplist(portray_clause,_As).
%
%       inspect(group,GroupId,group(GroupId,Contacts,Count)
%              ,[group(GroupId,Contacts,Count)|_]).
%       inspect(group,GroupId,group(GroupId,Contacts,Count)
%              ,[_,group(GroupId,Contacts,Count)|_]).
%       inspect(group,GroupId,group(GroupId,Contacts,Count)
%              ,[_,_,group(GroupId,Contacts,Count)|_]).
%       inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)
%              ,[contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
%       inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)
%              ,[_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
%       inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)
%              ,[_,_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
%       true.
%       ==
%
%       Above, list_type/1 is a specification predicate that determines
%       which terms are found as elements of lists in other terms'
%       arguments. These are used to collect only the clauses of
%       state_vector_terms/1 in the specification for those terms, that
%       can be in lists.
%
%       In the clauses output by list_access/3 note that the last
%       argument is a list, but this list is not a state-vector. Rather,
%       it is an argument of list type, of another state-vector term.
%       The generated clauses of inspect/4 in the example are used to
%       access terms in this list by their Id's (which should always be
%       the first argument in the term to be accessed). The puprose of
%       this is to be able to access the elements of state-vector term
%       arguments that are lists in constant time, without having to
%       scan the list every time.
%
%       Note that the number of inspect clauses to create for each kind
%       of term defined in list_type/1 is taken from the configuration
%       option list_budget/2. Above, there are three list-access terms
%       for 'group' and three for 'contact', implying the presence, in
%       the configuration of the following clauses of list_budget/2:
%       ==
%       list_budget(group,3).
%       list_budget(contact,3).
%       ==
%
%       The number of clauses of inspect predicates for list-terms is a
%       constant, so this predicate generates what is effectively a
%       static data structure, which can't be easily updated, unlike a
%       linked list, even if it is used to access a linked list. This is
%       all done in order to avoid hitting the dynamic database and
%       avoid a) slightly more expensive access operations and b) the
%       pain of working with the dynamic database.
%
%       Note also that the clauses in the example above are printed
%       using portray_clause/2. That's because, unlike
%       term_access/3 clauses, the don't-care elements of the
%       lists generated by this predicate are not bound to '$VAR'('_')
%       but to free variables- because that's the simplest way to do it.
%       Generated access predicate clauses are written to the output
%       file by portray_clause/3, so the unbound variables in lists will
%       end up printed as underscores, '_', anyway, but this is a
%       difference that should be kept in mind when working on the level
%       of this predicate, rather than printing predicates.
%
%       Finally, note that the list-terms in the example above are
%       accessed by matching their Ids (ContactID or GroupID, in the
%       example). This _assumes_ very strongly that the order in which
%       elements are added to the lists of groups and contacts matches
%       the Ids of those elements. If that assumption is broken, all
%       will be lost.
%
%       Well, ish. We can always change the code to access terms by
%       their index in a list. For now though accessing terms via their
%       Ids makes it clearer to see what we're accessing.
%
list_access(Ss,H,Ts):-
        list_access(Ss,H,[],Ts).

%!      list_access(+State,+How,+Acc,-Terms) is det.
%
%       Business end of list_access/3.
%
list_access([],_H,Acc,Ts):-
        reverse(Acc,Ts)
        ,!.
list_access([S|Ss],H,Acc,Bind):-
        list_access_acc(S,H,Acc,Acc_)
        ,list_access(Ss,H,Acc_,Bind).


%!      list_access_acc(+Term,+How,+Acc,-New) is det.
%
%       Accumulate list-access predicates' clauses for a Term.
%
%       This predicate handles the generation of access predicate
%       clauses for the given state-vector list-Term (i.e. a term that
%       is only found in lists that are arguments of other terms).
%
%       Term is a list-term, a state-vector term that found (or only
%       found) as the element of a list in an other term.
%
%       How is one of 'inspect' or 'modify' and determines the kind of
%       access term to generate.
%
%       Acc is the accumulator of list-access terms, which does not yet
%       include clauses ofr the current Term.
%
%       New is the updated Acc-umulator, including access clauses for
%       this Term.
%
list_access_acc(S,H,Acc,Acc_):-
        access_term(S,Nm-T)
        ,configuration:list_budget(Nm,B)
        ,T =.. [Nm,Id|_]
        ,list_access_acc(0,B,H,Nm-T,Id,Acc,Acc_).

%!      list_access_acc(+N,+B,+How,+Term,+Id,+Acc,-New) is det.
%
%       Business end of list_access_acc/4.
%
%       N and B are integers. B is the list-budget for the current Term,
%       while N is the counter of clauses generated for this Term, and
%       also the index into the list of Term's that the current clause
%       of an access predicate will, well, access.
%
%       How and Term are as in list_access_acc/4.
%
%       Acc is the accumulated list of clauses of access predicates' for
%       all list-terms, which must include all clauses for the given
%       Term.
%
%       New is the completed accumulator with access clauses for the
%       given Term.
%
list_access_acc(N,N,_H,_Nm_T,_Id,Ts,Ts):-
        !.
list_access_acc(N,K,H,Nm-T,Id,Acc,Bind):-
        nth0(N,Vs_,T)
        ,T_ =.. [H,Nm,Id,T,Vs_]
        ,succ(N,M)
        ,list_access_acc(M,K,H,Nm-T,Id,[T_|Acc],Bind).


%!      state_vectort_list_access_args(+Spec,+How,-Access) is det.
%
%       Create access predicates for arguments of list-type terms.
%
%       Spec is a state-vector specification list. Terms in Spec should
%       have names in list_type/2 clauses in the specification.
%
%       How is one of 'inspect' or 'modify', which determines the access
%       predicate whose clauses will be generated.
%
%       Access is a list of clauses of the access predicates given in
%       How.
%
list_arguments_access(Ss,H,As):-
        list_arguments_access(Ss,H,[],As).

%!      list_arguments_access(+Spec,+How,+Acc,-Access) is det.
%
%       Business end of list_arguments_access/3.
%
list_arguments_access([],_H,Acc,As):-
        reverse(Acc,As)
        ,!.
list_arguments_access([S|Ss],H,Acc,Bind):-
        H == inspect
        ,!
        ,argument_access_terms(S,F-As)
        ,list_argument_access_acc(As,F,H,Acc,Acc_)
        ,list_arguments_access(Ss,H,Acc_,Bind).
list_arguments_access([S|Ss],H,Acc,Bind):-
        H == modify
        ,state_modify_term_args(S,F-As)
        ,list_argument_access_acc(As,F,H,Acc,Acc_)
        ,list_arguments_access(Ss,H,Acc_,Bind).


%!      list_argument_access_acc(+Args,+Name,+How,+Acc,-New) is det.
%
%       Accumulate clauses of access predicates for list-term arguments.
%
%       Args is a list of lists where each sub-list is of the form
%       [N,V,A], where N is the name of an argument of a list-type term,
%       V is its value and A is the entire term. This list is
%       constructed by argument_access_terms/2.
%
%       Name is the name of the term for which to generate access
%       clauses. Name also comes from the output of
%       argument_access_terms/2.
%
%       How is one of 'inspect' or 'modify', which determines the access
%       predicate whose clauses will be generated.
%
%       Access is a list of clauses of the access predicates given in
%       How.
%
list_argument_access_acc([],_F,_H,Acc,Acc):-
        !.
list_argument_access_acc([[Arg,V,T]|As],F,H,Acc,Bind):-
        H == inspect
        ,!
        ,T_ =.. [H,F,Arg,V,T]
        ,list_argument_access_acc(As,F,H,[T_|Acc],Bind).
list_argument_access_acc([Arg|As],F,H,Acc,Bind):-
        H == modify
        ,argument_modifiers_acc(Arg,F,H,[],Acc,Acc_)
        ,list_argument_access_acc(As,F,H,Acc_,Bind).
