:-module(state_vector,[vector_initialisation/2
                      ,vector_initialisation/1
                      ,vector_specification/2
                      ,vector_specification/1
                      ,initialised_state_vector/1
                      ,initialised_state_vector/2
                      ,initialised_term/2
                      ,initialised_args/2
                      ,initialised_args/3
                      ,argument_of_type/2
                      ,initialised_type/2
                      ,argument_of_value/2
                      ]).

:-use_module(model_root(model_configuration)).
:-use_module(model_root(specification)).
:-use_module(model_src(types)).

/** <module> State vector inspection and manipulation predicates.

*/

%!      vector_initialisation(-Terms) is det.
%
%       Collect state vector initialisation Terms in the specification.
%
%       As vector_initialisation/2 but the names of terms to be
%       initialised are taken from the configuration option
%       state_vector_terms/1.
%
vector_initialisation(Is):-
        configuration:state_vector_terms(Ns)
        ,vector_initialisation(Ns,Is).


%!      vector_initialisation(+Names,-Terms) is det.
%
%       Collect state vector initialisation Terms in the specification.
%
%       Names is a list of atoms, each the name of a term in an atom of
%       state_vector_term_init/2 in the specification, and a
%       corresponding atom of state_vector_term/2.
%
%       Terms is a list of state_vector_term_init/2 terms.
%
%       Example query:
%       ==
%       ?- vector_initialisation(_Is), maplist(writeln,_Is).
%       state_vector_term_init(map_area,[x_min: -20,x_max:220,y_min: -20,y_max:120])
%       state_vector_term_init(scan_area,[x_min:0,x_max:200,y_min:0,y_max:100])
%       state_vector_term_init(position,[x:0.0,y:0.0,theta:0.0])
%       state_vector_term_init(scan_params,
%              [scan_width:20.0
%              ,leadinleadout:5.0
%              ,min_scan_length:10.0
%              ,nadir_width:3.0
%              ,boat_speed:20.0
%              ,time_limit:500.0
%              ,min_scan_angle_diff:30])
%       state_vector_term_init(observations,[contacts:[],count:0])
%       state_vector_term_init(grouped_observations,[groups:[],count:0])
%       true.
%       ==
%
vector_initialisation(Ns,Ts):-
        findall(state_vector_term_init(N,Ss)
               ,(member(N,Ns)
                ,specification:state_vector_term_init(N,Ss))
               ,Ts).



%!      vector_specification(-Terms) is det.
%
%       Collect all configured state-vector Terms in a secification.
%
%       As vector_specification/2 but the names of state-vector Terms to
%       be returned are taken from the configuration option
%       state_vector_terms/1.
%
vector_specification(Ts):-
        configuration:state_vector_terms(Ns)
        ,vector_specification(Ns,Ts).



%!      vector_specification(+Names,-Terms) is det.
%
%       Collect all named state-vector Terms in a specification.
%
%       Names is a list of atomic names of state vector terms.
%
%       Terms is a list of stae_vector_term/2 atoms where the first
%       argument is one of the Names given.
%
%       Example use:
%       ==
%       ?- vector_specification([map_area,scan_area,position,observations],Vs).
%       Vs = [state_vector_term(map_area
%              ,[x_min:length, x_max:length, y_min:length, y_max:length])
%       ,state_vector_term(scan_area
%              ,[x_min:length, x_max:length, y_min:length, y_max:length])
%       ,state_vector_term(position
%              ,[x:coordinate, y:coordinate, theta:angle])
%       ,state_vector_term(observations, [contacts:list(contact)])].
%       ==
%
vector_specification(Ns,Ts):-
        findall(state_vector_term(N,Ss)
               ,(member(N,Ns)
                ,specification:state_vector_term(N,Ss))
               ,Ts).



%!      initialised_state_vector(-Vector) is det.
%
%       Initialise a state Vector according to the current specification
%
%       As initialised_state_vector/2, but the Specification is taken
%       from the current contents of specification.pl.
%
initialised_state_vector(Vs):-
        vector_specification(Ss)
        ,initialised_state_vector(Ss,Vs).



%!      initialised_state_vector(+Specification,-Vector) is det.
%
%       Initialise a state Vector according to a Specification.
%
%       Specification is a list whose elements can be aither: a) Prolog
%       atoms each of which should be the name of a state-vector term,
%       found in the first argument of a state_vector_term/2 clause, or
%       b), state_vector_term/2 atoms (in the FOL sense now).
%
%       Vector is an initialised state-vector: a list where each element
%       is a term with the name and structure of one of the terms
%       defined in Specification, and with values initialised according
%       to type_init/2.
%
%       The form of arguments in state-vector terms depends on the value
%       of term_arguments/1.
%
%       Example query (manual spacing):
%       ==
%       % With term_arguments(terms):
%
%       ?- _Spec = [map_area,scan_area,position,observations,grouped_observations]
%       ,initialised_state_vector(_Spec,_Vs)
%       ,forall(member(V,_Vs),writeln(V))
%       ,term_arguments(Form).
%
%       map_area(x_min(0),x_max(0),y_min(0),y_max(0))
%       scan_area(x_min(0),x_max(0),y_min(0),y_max(0))
%       position(x(0.0),y(0.0),theta(0.0))
%       observations(contacts([]))
%       grouped_observations(groups([]))
%       Form = terms.
%
%       % With term_arguments(constants):
%
%       ?- _Spec = [map_area,scan_area,position,observations,grouped_observations]
%       ,initialised_state_vector(_Spec,_Vs)
%       ,forall(member(V,_Vs),writeln(V))
%       ,term_arguments(Form).
%
%       map_area(0,0,0,0)
%       scan_area(0,0,0,0)
%       position(0.0,0.0,0.0)
%       observations([])
%       grouped_observations([])
%       Form = constants.
%
%       % With state_vector_term/2 atoms (collected with vector_specification/2):
%
%       ?- vector_specification([map_area,position,observations],_Spec)
%       ,forall(member(S,_Spec),writeln(S))
%       ,initialised_state_vector(_Spec,_Vs)
%       ,forall(member(V,_Vs),writeln(V)).
%
%       state_vector_term(map_area,[x_min:length,x_max:length,y_min:length,y_max:length])
%       state_vector_term(position,[x:coordinate,y:coordinate,theta:angle])
%       state_vector_term(observations,[contacts:list(contact)])
%
%       map_area(x_min(0),x_max(0),y_min(0),y_max(0))
%       position(x(0.0),y(0.0),theta(0.0))
%       observations(contacts([]))
%       true.
%       ==
%
initialised_state_vector(Ts,Vs):-
        initialised_state_vector(Ts,[],Vs).

%!      initialised_state_vector(+Spec,+Acc,-Vector) is det.
%
%       Business end of initialised_state_vector/2.
%
%       The accumulator accumulates initialised state-vector terms.
%
initialised_state_vector([],Acc,Is):-
        reverse(Acc,Is)
        ,!.
initialised_state_vector([state_vector_term(N,_Ss)|Ts],Acc,Bind):-
        list_type(N)
        ,!
        ,initialised_state_vector(Ts,Acc,Bind).
initialised_state_vector([state_vector_term_init(N,_Ss)|Ts],Acc,Bind):-
        list_type(N)
        ,!
        ,initialised_state_vector(Ts,Acc,Bind).
initialised_state_vector([T|Ts],Acc,Bind):-
        initialised_term(T,TI)
        ,initialised_state_vector(Ts,[TI|Acc],Bind).



%!      initialised_term(+Term,-Initialised) is det.
%
%       Initialise one state-vector Term.
%
%       Term is one of the following three: a) the atomic name of a
%       state-vector term, found in the first argument of a
%       state_vector_term/2 clause, or, b) a state_vector_term/2 atom
%       or, c) a state_vector_term_init/2 atom.
%
%       Initialised is a compould term Term(Terms...) where each term in
%       Terms is as defined in the structure of Term in the
%       corresponding state_vector_term/2 clause and initialised
%       according to the form of Term.
%
%       If Term is an atom of state_vector_term/2 or the atomic name of
%       a state-vector term, then each argument of Term is initialised
%       by the initial value of the corresponding type, as defined in
%       type_init/3 in types.pl.
%
%       If Term is an atom of state_vector_term_init/2 then each
%       argument of Term is initialised to the value specified in that
%       atom, for that argument.
%
%       The form of arguments in state-vector terms depends on the value
%       of term_arguments/1.
%
%       Example query for atomic or state_vector_term/2 Term:
%       ==
%       % With term_arguments(constants):
%
%       ?- initialised_term(map_area,I)
%       ,term_arguments(Form).
%
%       I = map_area(0, 0, 0, 0),
%       Form = constants.
%
%       % With term_arguments(terms):
%
%       ?- initialised_term(map_area,I)
%       ,term_arguments(Form).
%
%       I = map_area(x_min(0), x_max(0), y_min(0), y_max(0)),
%       Form = terms.
%
%       % With a state_vector_term/1 atom as first argument:
%
%       ?- state_vector:initialised_term(state_vector_term(map_area
%              ,[x_min:length,x_max:length,y_min:length,y_max:length]),I).
%
%       I = map_area(x_min(0), x_max(0), y_min(0), y_max(0)).
%       ==
%
%       Example query for state_vector_term_init/2 Term:
%       ==
%       % With term_arguments(constants):
%
%       ?- vector_initialisation(_Is)
%       ,initialised_state_vector(_Is,_Vs)
%       ,maplist(writeln,_Vs), term_arguments(Form).
%
%       map_area(-20,220,-20,120)
%       scan_area(0,200,0,100)
%       position(0.0,0.0,0.0)
%       scan_params(500.0,30)
%       observations([])
%       grouped_observations([])
%       Form = constants.
%
%       % With term_arguments(terms):
%
%       ?- vector_initialisation(_Is)
%       ,initialised_state_vector(_Is,_Vs)
%       ,maplist(writeln,_Vs)
%       ,term_arguments(Form).
%
%       map_area(x_min(-20),x_max(220),y_min(-20),y_max(120))
%       scan_area(x_min(0),x_max(200),y_min(0),y_max(100))
%       position(x(0.0),y(0.0),theta(0.0))
%       scan_params(time_limit(500.0),min_scan_angle_diff(30))
%       observations(contacts([]))
%       grouped_observations(groups([]))
%       Form = terms.
%       ==
%
initialised_term(state_vector_term_init(T,Ts),I):-
        %!
        must_be(ground,[T,Ts])
        %,specification:state_vector_term(T,Ss)
        ,state_vector_term(T,Ss)
        ,findall(Arg
                ,(nth1(I,Ts,N:V)
                 ,nth1(I,Ss,N:Tp)
                 ,argument_of_value(N:Tp->V,Arg)
                 )
                ,Is)
        ,I =.. [T|Is].
initialised_term(state_vector_term(T,Ts),I):-
        %!
        initialised_args(Ts,Is)
        ,I =.. [T|Is].
initialised_term(T,I):-
        atom(T)
%        ,specification:state_vector_term(T,Ts)
        ,state_vector_term(T,Ts)
        ,initialised_args(Ts,Is)
        ,I =.. [T|Is].



%!      initialised_args(+Args,-Initialised) is det.
%
%       Initialise each argument of a state-vector term.
%
%       Args is a list of key-value pairs Name:Type, where Name is the
%       name of an argument of a term in a state-vector and Type is the
%       type of that argument, as found in the second argument of a
%       state_vector_term/2 clause.
%
%       Initialised is a list of arguments created according to the
%       specification in Args. The form of the elements of Initialied
%       depends on the value of term_arguments/1.
%
%       If term_arguments(terms) is true, each element of Initialised
%       is a compound Name(Init) where Name is the key in a Name:Type
%       pair as described above, and Init is the initialisation value of
%       the corresponding Type.
%
%       If term_arguments(constants) is true, then each element of
%       Initialised is a constant, the initialisation value of the
%       Type specified for that argument as above.
%
%       Example query:
%       ==
%       % With term_arguments(terms):
%
%       ?- initialised_args([x_min:length,x_max:length,y_min:length,y_max:length],I)
%       ,term_arguments(Form).
%
%       I = [x_min(0), x_max(0), y_min(0), y_max(0)],
%       Form = terms.
%
%       % With term_arguments(constants):
%
%       ?- initialised_args([x_min:length,x_max:length,y_min:length,y_max:length],I)
%       ,term_arguments(Form).
%
%       I = [0, 0, 0, 0],
%       Form = constants.
%       ==
%
initialised_args(Ts,Is):-
        initialised_args(Ts,[],Is).

%!      initialised_args(+Terms,+Acc,-Initialised) is det.
%
%       Business end of initialised_args/3.
%
%       Acc is the accumulator of initialised terms.
%
initialised_args([],Acc,Is):-
        reverse(Acc,Is)
        %,!
        .
initialised_args([N:T|Ts],Acc,Bind):-
        argument_of_type(N:T,I)
        ,initialised_args(Ts,[I|Acc],Bind).


%!      argument_of_type(+Argument,-Value) is det.
%
%       Initialise an Argument to a Value of the correct type.
%
%       Argument is a term N:T where N is the name of an argument of a
%       state-vector term and T is the type of that argument.
%
%       Value is an initial value of the given Argument. This is either
%       a constant of the type specified in T, if
%       term_arguments(constants) is true, or a compound term N(T) if
%       term_arguments(terms) is true.
%
%       This predicate raises an error if term_arguments/1 is something
%       else than 'terms' or 'constants'.
%
%       Examples of use:
%       ==
%       % With term_arguments(constants):
%
%       ?- state_vector:argument_of_type(x:integer,V), term_arguments(Form).
%       V = 0,
%       Form = constants.
%
%       % With term_arguments(terms):
%
%       ?- state_vector:argument_of_type(x:integer,V), term_arguments(Form).
%       V = x(0),
%       Form = terms.
%       ==
%
argument_of_type(_N:T,V):-
        term_arguments(constants)
        %,!
        ,initialised_type(T,V).
argument_of_type(N:T,I):-
        term_arguments(terms)
        %,!
        ,initialised_type(T,V)
        ,I =.. [N,V].
argument_of_type_(_,_):-
        term_arguments(T)
        ,must_be(oneof([terms,constants]),T).


%!      initialised_type(+Type,-Value) is nondet.
%
%       Initialise a Value of the given Type.
%
%       Type is the name of a type known to the system, as listed in
%       type/2.
%
%       Value is the initial value of Type, as given in type_init/2.
%       Note that not all types known to the system have an
%       initialisation value in a type_init/2 clause. If Type doesn't,
%       then the initialisation value is taken from the first subtype of
%       type_init/2 that does have an initialisation value.
%
%       @tbd: This predicate is _not_ deterministic. It will backtrack
%       over multiple initialisation values if multiple are somehow
%       defined, e.g. if there's some mistake in the type hierarchy that
%       means a type is a sub-type of multiple super-types at once. This
%       should be considered an unwanted complication and please deal
%       with this later right, OK.
%
%       Example usage:
%       ==
%       ?- initialised_type(distance,Init).
%       Init = 0.
%
%       ?- initialised_type(coordinate,Init).
%       Init = 0.0.
%       ==
%
initialised_type(T,V):-
        type_init(T,V).
        %,!.
initialised_type(Sub,V):-
        subtype(Sub,Sup)
        ,initialised_type(Sup,V).


%!      argument_of_value(+Argument,-Value) is det.
%
%       Check the Value of an Argument is the right type.
%
%       Argument is a compound N:T->V where N is the name of an argument
%       in a state-vector term, T is its type according to the
%       specification and V is its initial value according to the
%       specification. In particular, T is the type given for the named
%       argument in a state_vector_term/2 atom, while V is the value for
%       that argument given in a state_vector_term_init/2 atom.
%
%       Value is the value of the named Argument, constructed according
%       to the configuration option term_arguments/1. If that option is
%       set to 'constant', Value is a constant. If that option is set to
%       'terms', Value is a compound N(V).
%
argument_of_value(N:T->V,V):-
        term_arguments(constants)
        ,!
        ,(   compound_type(V,T)
         ->  true
         ;   format(atom(M),'Argument: ~w',[N:T->V])
            ,throw(error(type_error(T,V),context(argument_of_value/2,M)))
        ).
argument_of_value(N:T->V,A):-
        term_arguments(terms)
        ,(   compound_type(V,T)
         ->  A =.. [N,V]
         ;   format(atom(M),'Argument: ~w',[N:T->V])
            ,throw(error(type_error(T,V),context(argument_of_value/2,M)))
         ).
