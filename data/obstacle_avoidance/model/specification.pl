:-module(specification, [state_vector_term/2
                        ,state_vector_term_init/2
                        ,fluent/1
                        ,list_type/1
                        ]).

/** <module> State-vector specification.

Steps to add a new term, T, with structure S, to a state-vector:

a) Create a clause state_vector_term(T,S).
b) If T is a fluent, add a clause fluent(T).
c) It T can be contained in a list, then:
c1) add a clause list_type(T).
c2) Add a clause list_budget(T,<some budget>) in configuration.pl

To have T ncluded in initialised state-vectors and access predicates,
remember to include T in the list of state-vector terms in the
configuration options state_vector_terms/1 in configuration.pl.


*/


%!      state_vector_term(?Name,?Structure) is semidet.
%
%       The Name and Structure of a state-vector term.
%
%       Name is the atomic nameof a state-vector term, used to
%       initialise and access that term. A state-vector should have a
%       term with a functor of the given Name, and the given Structure,
%       as explained below.
%
%       Structure is a list of key-value pairs Term:Type, where each
%       Term is the name of an argument-term of the named state-vector
%       term (i.e. the term whose name is given in Name), and Type is
%       its type, which must be one of the types known to the system, as
%       defined in type/1.
%
%       Name and Structure are used to initialise the state-vector as a
%       list of terms with the given arguments.
%
%
state_vector_term(destination,[x:coordinate,y:coordinate,theta:angle]).
state_vector_term(position,[x:coordinate,y:coordinate,theta:angle]).
state_vector_term(plan,[actions:list(action),count:integer]).
state_vector_term(map_area,[x_min:length,x_max:length,y_min:length,y_max:length]).
state_vector_term(scan_area,[x_min:length,x_max:length,y_min:length,y_max:length]).
state_vector_term(scan_params,[scan_width:float
                              ,leadinleadout:float
                              ,min_scan_length:float
                              ,nadir_width:float
                              ,boat_speed:speed
                              ,time_limit:time
                              ,min_scan_angle_diff:length
                              ]).
state_vector_term(observations,[contacts:list(contact),count:integer]).
state_vector_term(grouped_observations,[groups:list(group),count:integer]).
% List-level terms: only found in lists in other terms.
state_vector_term(group,[groupId:id,contacts:list(id),count:integer]).
state_vector_term(contact,[contactId:id
                          ,actionId:id
                          ,time:time
                          ,x:coordinate
                          ,y:coordinate
                          ,range:distance
                          ,scan_angle:angle
                          ]).
state_vector_term(action,[actionId:id
                         ,actionType:name
                         ,actionName:name
                         ,actionParams:list(string)
                         ]).
/* Example of adding a new term.
state_vector_term(obstacles,[objects:list(obstacle),count:integer]).
state_vector_term(obstacle,[obstacleId:id
                           ,x:coordinate
                           ,y:coordinate
                           ,heading:angle
                           ,speed:speed
                           ,direction:angle
                           ,distance:length]).
*/

%!      state_vector_term_init(?Name,?Values) is semidet.
%
%       The Name and initial Values of a state-vector term.
%
%       Name is the atomic nameof a state-vector term, used to
%       initialise and access that term. A state-vector should have a
%       term with a functor of the given Name, and a sructure as
%       specified in the second argument of a state_vector_term/2 atom
%       with the same Name.
%
%       Values is a list of key-value pairs Arg:Val, where each Arg is
%       an argument of the named term, and each Val is the value of that
%       argument. The value of each argument must be of the type
%       specified in the corresponding state_vector_term/2 atom, i.e.
%       the one with the same name as this state_vector_term_init/2
%       atom, otherwise hilarity will ensue.
%
state_vector_term_init(destination,[x:0.0,y:0.0,theta:0.0]).
state_vector_term_init(position,[x:100.0,y:100.0,theta:0.0]).
state_vector_term_init(plan,[actions:[],count:0]).
state_vector_term_init(scan_area,[x_min:0,x_max:200,y_min:0,y_max:100]).
state_vector_term_init(map_area,[x_min:(-20),x_max:220,y_min:(-20),y_max:120]).
state_vector_term_init(scan_params,[scan_width:20.0
                                   ,leadinleadout:5.0
                                   ,min_scan_length:10.0
                                   ,nadir_width:3.0
                                   ,boat_speed:20.0
                                   ,time_limit:500.0
                                   ,min_scan_angle_diff:30
                                   ]).
state_vector_term_init(observations,[contacts:[],count:0]).
state_vector_term_init(grouped_observations,[groups:[],count:0]).


%!      fluent(?Name) is det.
%
%       A state-vector term that can be modified.
%
%       Used to select the state-vector terms for which modify/5 clauses
%       will be constructed.
%
%       @tbd: "Fluent" is taken from the Event Calculus where it means a
%       predicate whose terms may change over time.
%
fluent(destination).
fluent(plan).
fluent(action).
fluent(position).
fluent(observations).
fluent(grouped_observations).
fluent(group).
fluent(contact).
/*
fluent(obstacles).
fluent(objects).
fluent(obstacle).
*/


%!      list_type(?Term) is semidet.
%
%       Name of a Term that only found as an element of a list.
%
%       Term is the name of a state-vector term that can only be found
%       inside a list that is an argument of another state-vector term.
%
list_type(action).
list_type(group).
list_type(contact).
%list_type(obstacle).
