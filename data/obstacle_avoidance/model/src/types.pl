:-module(types, [type/2
                ,type/1
                ,type_init/2
                ,type_modifier/2
                ,type_modifier/3
                ,subtype/2
                ,compound_type/2
                ,increment_int/2
                ,increment_float/2
                ,increment/3
                ,decrement_int/2
                ,decrement_float/2
                ,decrement/3
                ,arithmetic_operation/4
                ,numeric_inequality/3
                ,term_value/2
                ,new_value/3
                ,prepend/3
                ,remove/3
                ,set_val/3
                ]).

:-use_module(model_root(specification)).

/** <module> Rudimentary type system for state-vector terms.

*/


%!      type(+Type,+Term) is det.
%
%       True when Term is of the given Type.
%
%       Type is an atom, the name of the Type. This should match one of
%       the atomic type names defined in type/1.
%
%       Alternatively, Type can be the compound list(T) in which case T
%       must be a type/1 type name.
%
%       Term is a term of the given type. This might be a single,
%       atomic, constant, or a complex term of the form Functor(Term),
%       where the type of Term is defined in the body of the type/2
%       clause in terms of other type/2 clauses.
%
type(float,X):-
        %!
        float(X).
type(integer,X):-
        %!
        integer(X).
type(list(T),Ls):-
        %!
        list(Ls,T).
type(string,S):-
        %!
        atom(S).
type(T,V):-
        subtype(T,S)
        ,type(S,V).



%!      list(+Elements,+Type) is det.
%
%       True when all Elements are of the given Type.
%
%       Used to check the types of elements in typed lists of terms.
%
%       Elements may be the empty list. Type must be a type known to the
%       system as recorded in type/1.
%
list([],T):-
        type(T).
       %,!.
list([],T):-
        list_type(T).
       %,!.
list([L|Ls],T):-
        compound_type(L,T)
        ,list(Ls,T).



%!      type(?Name) is semidet.
%
%       A Named type.
%
%       Name is an atom, the name of a type (other than the SWI-Prolog
%       defined "types", such as "number", "atomic", "integer" etc).
%
%       Alternatively, Name may be the compound list(T) where T must be
%       a type name.
%
%       Used as a record and documentation of the types known to the
%       system.
%
type(angle).
type(coordinate).
type(distance).
type(float).
type(id).
type(integer).
type(length).
type(list(T)):-
        type(T).
type(name).
type(speed).
type(string).
type(time).



%!      subtype(?Subtype,?Supertype) is semidet.
%
%       True when Subtype is a sub-type of Supertype.
%
%       Used to associate types known in the system in a type hierarchy.
%       Sigh. Mainly for the purposes of initialisation.
%
subtype(angle,float).
subtype(coordinate,float).
subtype(distance,length).
subtype(id,integer).
subtype(length,integer).
subtype(name,string).
subtype(speed,float).
subtype(time,float).



%!      type_init(?Type,?Value) is semidet.
%
%       An initialisation Value for the given Type.
%
type_init(integer,0).
type_init(float,0.0).
type_init(list(_),[]).



%!      type_modifier(?Type,?Modifier) is semidet.
%
%       A Type and a Modifier predicate name.
%
%       Modifier predicates are binary predicates that take as input ish
%       a term of the given Type and return ish the same term modified
%       according to its type.
%
%       Parameters for predicates declared as modifiers by
%       type_modifier/2 are taken from the specification of a problem
%       domain.
%
type_modifier(integer,increment:increment_int/2).
type_modifier(integer,decrement:decrement_int/2).
type_modifier(float,increment:increment_float/2).
type_modifier(float,decrement:decrement_float/2).
type_modifier(T,M):-
        subtype(T,S)
        ,type_modifier(S,M).


%!      type_modifier(?Type,?Parameters,?Modifier) is semidet.
%
%       A Type and a Modifier predicate name.
%
%       Modifier predicates with Parameters are three-arity predicates
%       that take as input ish a term of the given Type and return ish
%       the same term modified according to its type by the given
%       Paramters.
%
type_modifier(list(_),params(_L),add:prepend/3).
type_modifier(list(_),params(_L),delete:remove/3).
type_modifier(integer,params(_X),set:set_val/3).
type_modifier(float,params(_X),set:set_val/3).
type_modifier(string,params(_S),set:set_val/3).
type_modifier(T,Ps,M):-
        subtype(T,S)
        ,type_modifier(S,Ps,M).

% Alternative notation; not used:
%type_modifier(list(_),params(el:term,list:list(term),new:list(term)),add:prepend/3).
/*
type_modifier(list(_),params(El),add:prepend(El,_L1,_L2)).
type_modifier(integer,params(X),set:set_val(X,_Y,_Z)).
type_modifier(float,params(X),set:set_val(X,_Y,_Z)).
*/


%!      compound_type(+Value,+Type) is det.
%
%       Check the Type of a Value.
%
%       Helper to handle both possible forms of Value: a constant, or a
%       compound.
%
%       If Value is a constant then type(Type,Value) must be true.
%
%       If Value is a compound F(V) then type(Type,V) must be true.
%
%       Otherwise this predicate fials silently. As I just did.
%
compound_type(V,T):-
        atomic(V)
        %,!
        ,type(T,V).
compound_type(V,T):-
        is_list(V)
        %,!
        ,type(T,V).
compound_type(V,T):-
        compound(V)
        ,\+ is_list(V)
        %,!
        ,V =.. [_F,V_]
        ,type(T,V_).


%!      increment_int(+X,-Y) is det.
%
%       Increment a term of integer type.
%
increment_int(X,Y):-
        increment(integer,X,Y).


%!      increment_float(+X,-Y) is det.
%
%       Increment a term of float type.
%
increment_float(X,Y):-
        increment(float,X,Y).


%!      increment(+Type,+X,-Y) is det.
%
%       Increment a term of the given Type.
%
%       @tbd This should check the type of X before incrementing.
%
increment(T,X,Y):-
        stride(T,S)
        ,arithmetic_operation(X,+,S,Y).



%!      decrement_int(+X,-Y) is det.
%
%       Decrement a term of integer type.
%
decrement_int(X,Y):-
        decrement(integer,X,Y).


%!      decrement_float(+X,-Y) is det.
%
%       decrement a term of float type.
%
decrement_float(X,Y):-
        decrement(float,X,Y).


%!      decrement(+Type,+X,-Y) is det.
%
%       Decrement a term of the given Type.
%
%       @tbd This should check the type of X before Decrementing.
%
decrement(T,X,Y):-
        stride(T,S)
        ,arithmetic_operation(X,-,S,Y).


%!      arithmetic_operation(+X,+Op,+Y,-Z) is det.
%
%       An arithmetic operation such that Op(X,Y,Z).
%
%       X and Y are either constants or compound terms, depending on the
%       value of term_arguments/1.
%
%       Op is the functor of an arithmetic function known to SWI-Prolog:
%       +, -, /, *, etc.
%
%       Z is the result of applying the arithmetic function in Op to X
%       and Y.
%
arithmetic_operation(X,O,Y,Z):-
        term_value(X,X_)
        ,Op =.. [O,X_,Y]
        ,Z_ is Op
        ,new_value(X,Z_,Z).



%!      numeric_inequality(+X,+Inequality,+Y) is det.
%
%       True when X Inequality Y is true.
%
%       X and Y are two numbers: integers or floats. There is no type
%       checking in this predicate so X and Y can be an integer and a
%       float.
%
%       Inequality is one of the Prolog comparison predicates that apply
%       between numbers and arithmetic expressions: >/2, </2, =</2,
%       >=/2, =\=/2, and =:=/2.
%
numeric_inequality(X,C,Y):-
        term_value(X,X_)
        ,term_value(Y,Y_)
        ,C_ =.. [C,X_,Y_]
        ,call(C_).



%!      term_value(+Term,-Value) is det.
%
%       Retrieve the Value of a Term.
%
%       Term is either a constant or a compound Prolog term, according
%       to the value of term_arguments/1.
%
%       Value is the value of the Term: either the Term's argument, if
%       term_argument(terms) is true; or the (constant) Term itself, if
%       ter_argument(constants) is true.
%
term_value(T,T):-
        term_arguments(constants).
        %,!.
term_value(T,T_):-
        term_arguments(terms)
        ,T =.. [_,T_].


%!      new_value(+Term,+Value,-New) is det.
%
%       Modify the Value of a Term.
%
%       Term is either a constant or a compound Prolog term, according
%       to the value of term_arguments/1.
%
%       Value is always a constant, the new value of Term.
%
%       If term_arguments(constants) is true, then Term is a constant
%       and New is bound to Value.
%
%       If term_arguments(terms) is true,then Term is a compound Prolog
%       T(V), where T the functor and V the current value, and New is
%       the compound term T(Value).
%
%       This is used to simplify arithmetic when the form of terms'
%       arguments can be a constant or a compound term
%
new_value(_,V,V):-
        term_arguments(constants).
        %,!.
new_value(T,V,T_):-
        term_arguments(terms)
        ,functor(T,F,_A)
        ,T_ =.. [F,V].


%!      prepend(+Element,+List,-New) is det.
%
%       Prepend an Element to a List.
%
prepend(X,[Xs|T],[X|[Xs|T]]).%:-
% Current argument is a list
        %!.
prepend(X,T,T_):-
% Current argument is a list-term
% e.g. T = contacts([... list of contacts ...]).
        T =.. [F,Xs]
        ,T_ =.. [F,[X|Xs]].


%!      remove(+Element,+List,-New) is det.
%
%       Remove an Element from a List.
%
remove(X,Xs,Ys):-
        is_list(Xs)
        ,selectchk(X,Xs,Ys).
remove(X,T,T_):-
        compound(T)
        ,T =.. [F,Xs]
        ,selectchk(X,Xs,Ys)
        ,T_ =.. [F,Ys].


%!      set_val(+Given,+Old,-New) is semidet.
%
%       Directly set a value to a Given value.
%
set_val(X,_Y,X):-
        ground(X)
        ,term_arguments(constant).
        %,!.
set_val(X,Y,X_):-
        maplist(ground,[X,Y])
        ,term_arguments(terms)
        ,Y =.. [F,_V]
        ,X_ =.. [F,X].
