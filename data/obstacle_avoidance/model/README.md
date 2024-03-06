Autonomous Agent Architecture
=============================

The code in this project defines a world-model for an autonomous agent operating
in the Basic Simulator. The agent's model is implemented as a set of Prolog
predicates to be used as background knowledge for learning by MIL, or other ILP
approaches.

The background knowledge is based on a set of "access primitives" that are
automatically generated from a specification describing the elements of a "state
vector", a representation of the state of the world in the simulation.

Access primitives, together with some auxiliary predicates of generic nature,
are used to compose PDDL-like actions. These are the ones directly used as
background knowledge to learn plans for the agent.

This file describes the structure of the Autonomous Agent Architecture project,
and the composition of the background knowledge, including the generation of
access primitives. More information will be added as the project proceeds.
Feedback is encouraged, from all project participants.

Project Structure
-----------------

```
.
|   configuration.pl - Project-wide configuration options.
|   load_project.pl  - Load project files into SWI-Prolog memory.
|   README.md        - This file.
|
+---data
|       actions.pl       - PDDL-like actions for use as background knowledge 
|       specification.pl - Description of state-vector terms, arguments and their types 
|
+---lib
+---output
|       vector_access.pl - Generated file of vector access primitives
|
+---scripts
\---src
        printing.pl         - Predicates to print vector acccess primitives to a file.
        state_operations.pl - Generate predicates operating on state vectors.
        state_vector.pl     - State-vector initialisation and manipulation predicates.
        types.pl            - Rudimentary type system for state-vector terms' arguments.
```

Running the project code
------------------------

The project code runs under SWI-Prolog. It was developed with SWI-Prolog version
9.1.15, 64 bits.

Make sure that you have an up-to-date version of the code, and SWI-Prolog before
starting to work on the project.

### Start the Project

In the project's root directory (the top directory when you clone the git
repository) you will find a file named `load_project.pl`. Clicking on this or
consulting it from the Prolog command-line will do the following things:

1. Load necessary project files to memory.
2. Open the SWI-Prolog IDE with some project files in the editor.
3. Start the SWI-Prolog documentation server, displaying this readme file in the
default browser.

### Configure the project

In the configuration file, `configuration.pl` you can find options that control
the behaviour of the project. 

For example, you can set the "budget" for lists of terms that are defined as
`list_type/1` in the specification, such as `group` and `contact`, by changing
the `list_budget/2` option, etc.

We refer to `configuration.pl` and its contents as "the configuration".

### Edit the specification

The specification file in `data/specification.pl` describes the structure of the
"state vector", a representation of our autonomous agent's environment as a
Prolog list of terms representing objects or concepts of interest in the
environment.

We refer to `specification.pl` and its contents as "the specification".

#### State vector terms

Each term to be included in the state vector is defined by a clause of the
predicate `state_vector_term/2` in `specification.pl`. Each clause of
`state_vector_term/2` lists the name of a term in the state vector, and the
arguments of that term, each with its type.

Types of arguments in the state vector must conform to the rudimentary type
system defined in `src/types.pl`. This is used to impose some sanity checks on
values of state vector term arguments, when those are modified.

#### Fluents

Some of the terms in the state vector, and their arguments, can be modified.
Such terms are called fluents, in various AI fields' jargons. In the project,
a term that is a fluent can be marked as such by declaring its name in a
`fluent/1` clause. You will find such declarations already in the specification
file.

#### List type terms

Some terms specified in `state_vector_term/2` clauses can only be found as
elements in lists that are arguments of other terms. We call the former
"list-type" terms.

A term can be declared as a list-type term by declaring its name in a clause of
the predicate `list_type/1` in the specification.

### Initialise a state vector to default valuse

Given a set of `state_vector_term/2` clauses in the specification, a state
vector can be initialised with each argument set to its default value according
to its type, as follows:

```
% List the specification of state vector terms:

?- vector_specification(_Ss), maplist(writeln,_Ss).
state_vector_term(map_area,[x_min:length,x_max:length,y_min:length,y_max:length])
state_vector_term(scan_area,[x_min:length,x_max:length,y_min:length,y_max:length])
state_vector_term(position,[x:coordinate,y:coordinate,theta:angle])
state_vector_term(scan_params,[scan_width:float,leadinleadout:float,min_scan_length:float,nadir_width:float,boat_speed:speed,time_limit:time,min_scan_angle_diff:length])
state_vector_term(observations,[contacts:list(contact),count:integer])
state_vector_term(grouped_observations,[groups:list(group),count:integer])
state_vector_term(group,[groupId:id,contacts:list(id),count:integer])
state_vector_term(contact,[contactId:id,actionId:id,time:time,x:coordinate,y:coordinate,range:distance,scan_angle:angle])
true.

% Initialise a state vector to default values by type:

?- initialised_state_vector(_Vs), maplist(writeln,_Vs).
map_area(x_min(0),x_max(0),y_min(0),y_max(0))
scan_area(x_min(0),x_max(0),y_min(0),y_max(0))
position(x(0.0),y(0.0),theta(0.0))
scan_params(scan_width(0.0),leadinleadout(0.0),min_scan_length(0.0),nadir_width(0.0),boat_speed(0.0),time_limit(0.0),min_scan_angle_diff(0))
observations(contacts([]),count(0))
grouped_observations(groups([]),count(0))
true.
```
#### Initialise a state vector to arbitrary values

Alternatively (and more usefully) a state vector can be initialised to arbitrary
values, most likely ones taken from the Basic simulator.

Initial values for state vector terms' arguments are given in clauses of the
predicate `state_vector_term_init/2`. One clause of this predicate must be given
for each clause of `state_vector_term/2`, with the same term name and with
initialisation values matching the types of that term's arguments.

For example, suppose the following clause of `state_vector_term/2` is used to
declare a term called "position" and with arguments "x" and "y", of type
"coordinate", and "theta" of type "angle":

```
state_vector_term(position,[x:coordinate,y:coordinate,theta:angle]).
```

In that case, a corresponding `state_vector_term_init/2` must be included in the
specification with "position" as the first argument and a list of argument-names
and values of the right types:

```
state_vector_term_init(position,[x:0.0,y:0.0,theta:0.0]).
```

Note that currently there is no attempt to check either of the conditions above
(naming and argument typing). If you give the wrong term names or argument
values you will probably get some inscrutable errors.

Once initial values are given, a state vector can be initialised to the declared
values as follows:

```

% List state_vector_term_init/2 clauses in the specification:

?- vector_initialisation(_Is), maplist(writeln,_Is).
state_vector_term_init(map_area,[x_min: -20,x_max:220,y_min: -20,y_max:120])
state_vector_term_init(scan_area,[x_min:0,x_max:200,y_min:0,y_max:100])
state_vector_term_init(position,[x:0.0,y:0.0,theta:0.0])
state_vector_term_init(scan_params,[scan_width:20.0,leadinleadout:5.0,min_scan_length:10.0,nadir_width:3.0,boat_speed:20.0,time_limit:500.0,min_scan_angle_diff:30])
state_vector_term_init(observations,[contacts:[],count:0])
state_vector_term_init(grouped_observations,[groups:[],count:0])
true.


% Initialise a state vector to initialisation terms in the specification:

?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs), maplist(writeln,_Vs).
map_area(x_min(-20),x_max(220),y_min(-20),y_max(120))
scan_area(x_min(0),x_max(200),y_min(0),y_max(100))
position(x(0.0),y(0.0),theta(0.0))
scan_params(scan_width(20.0),leadinleadout(5.0),min_scan_length(10.0),nadir_width(3.0),boat_speed(20.0),time_limit(500.0),min_scan_angle_diff(30))
observations(contacts([]),count(0))
grouped_observations(groups([]),count(0))
true.
```

### Generate vector access primitives

An initialised state vector can be accessed by a set of "access primitives",
Prolog predicates that are automatically generated by the same specification as
an initialised vector.

The following query will automatically generate access primitives for all stae
vector terms, and their arguments, in the specification:

```
?- write_primitives, primitives_file(_F), edit(_F).
true.
```

This will also open the file where the access primitives are written in the
SWI-Prolog IDE. The name of the file is taken from the configuration option
`primitives_file/1`.

Access primitives perform one of two operations on state vector terms and their
arguments: inspect; and modify. "Inspect" means that the value of a term or
argument in a state vector is retrieved, without modification. "Modify" means
that the argument is modified.

#### Inspect predicates

```
% inspect/3 clause for position term.
inspect(position,position(X,Y,Theta),[_,_,position(X,Y,Theta),_,_,_]).

% inspect/4 clauses for arguments of position term.
inspect(position,x,X,[_,_,position(X,_,_),_,_,_]).
inspect(position,y,Y,[_,_,position(_,Y,_),_,_,_]).
inspect(position,theta,Theta,[_,_,position(_,_,Theta),_,_,_]).
```

Two inspect predicates are generated: one with arity 3 and one with arity 4.

Clauses of `inspect/3` inspect terms in a state vector. Clauses of `inspect/4`
inspect the arguments of terms. In both cases, an initialised state vector must
be passed as the last argument of the head literal of the inspect clause.
Inspect clauses are always unit clauses (without a body).

#### Modify predicates

```
% modify/4 clause for position term.
modify(position,position(X,Y,Theta),[Map_area,Scan_area,_Position,Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations]).

% modify/5 clauses for arguments of position term.
modify(position,increment,x,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X_,Y,Theta),Scan_params,Observations,Grouped_observations]) :-
    increment_float(X,X_).
modify(position,decrement,x,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X_,Y,Theta),Scan_params,Observations,Grouped_observations]) :-
    decrement_float(X,X_).
modify(position,set,x(Param),[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X_,Y,Theta),Scan_params,Observations,Grouped_observations]) :-
    set_val(Param,X,X_).
modify(position,increment,y,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y_,Theta),Scan_params,Observations,Grouped_observations]) :-
    increment_float(Y,Y_).
modify(position,decrement,y,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y_,Theta),Scan_params,Observations,Grouped_observations]) :-
    decrement_float(Y,Y_).
modify(position,set,y(Param),[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y_,Theta),Scan_params,Observations,Grouped_observations]) :-
    set_val(Param,Y,Y_).
modify(position,increment,theta,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y,Theta_),Scan_params,Observations,Grouped_observations]) :-
    increment_float(Theta,Theta_).
modify(position,decrement,theta,[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y,Theta_),Scan_params,Observations,Grouped_observations]) :-
    decrement_float(Theta,Theta_).
modify(position,set,theta(Param),[Map_area,Scan_area,position(X,Y,Theta),Scan_params,Observations,Grouped_observations],[Map_area,Scan_area,position(X,Y,Theta_),Scan_params,Observations,Grouped_observations]) :-
    set_val(Param,Theta,Theta_).

```

Two modify predicates are also generated: one with arity 4 and one with arity 5.

Clauses of `modify/4` modify a state vector term by replacing it with another,
new, term of the same kind. Clauses of `modify/5` modify each of the arguments
of a term. 

In both cases, an initialised state vector must be passed as the penultimate
argument of the head literal of the modify clause. A modified state vector is
then bound to the last argument of the head literal of the modify clause on
exit.

Clauses of modify predicates are only generated for state vector terms declared
as fluents in the specification. Currently, if a term is declared as fluent,
modify predicates are generated for each of their arguments - there is no option
to declare only some arguments as changeable.

Clauses of `modify/5` modify arguments of terms according to arguments' types.
Each type recognised by the system, i.e. defined in `types.pl`, is associated
with one or more type-modifiers. These type modifiers are used to generate the
modify clauses for each argument of a given type.

#### Accessing list-type terms

```
% inspect/4 clause for "groups" argument in grouped_observations term:

inspect(grouped_observations,groups,Groups,[_,_,_,_,_,grouped_observations(Groups,_)]).

% inspect/4 clauses for each element of the list Groups in the groups argument
% of a grouped_observations term. Elements are accessed by GroupId which must be
% the first argument of the group term.

inspect(group,GroupId,group(GroupId,Contacts,Count),[group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,_,_,group(GroupId,Contacts,Count)|_]).

% inspect/4 clauses for arguments of group terms, in the list Groups of the
% groups argument of a grouped_observations term.

inspect(group,groupId,GroupId,group(GroupId,_,_)).
inspect(group,contacts,Contacts,group(_,Contacts,_)).
inspect(group,count,Count,group(_,_,Count)).

% inspect/4 clauses for elements of a list of contact terms in the "contacts"
% argument of an element of a group term in a list of group terms in a "groups"
% argument of a grouped_observations term. Try saying that five times quickly.

inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle),[contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle),[_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle),[_,_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle),[_,_,_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
inspect(contact,ContactId,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle),[_,_,_,_,contact(ContactId,ActionId,Time,X,Y,Range,Scan_angle)|_]).
```

If a term is declared in the specification as a list-type term, inspect/4
clauses are also generated for each element in this list, as well as for terms
of that type, and their arguments. List-type terms can themselves have list-type
arguments, and in this case the necessary access predicates are generated also.

```
Note:

So far, only two levels of nesting of that sort have been tried: the
grouped_observations term has an argument named "groups", whose value is a list
of "group" terms. Each group term has an argument named "contacts" whose value
is a list of contact ids (where 'id' is a type defined in types.pl). 

Thus, to access the list of contact ids inside a group, the list of groups
inside a grouped_observations term must first be accessed and access predicates
are generated for the entire closure.
```

For all of the above to work the first argument of a list-type term (such as
"group" or "contact") must be that term's id (i.e. an argument with a value of
type "id"). That is because currently the argument of inspect/4 that matches the
id of a list-type element tries to match it in the first agrument of that term.
In the future, this may be changed to allow an arbitrary argument of a list-type
term to be matched as an "id" (or, really, a key).

#### List budget

We do not know ahead of time how many instances of a list-type term will need to
be created during a simulation. For example, we don't know ahead of time how
many contacts will be made and how many groups they will be grouped into.

This can be a problem as the lists containing list-type terms, or, rather, their
access predicates, are automatically generated along with the clauses of those
access predicates, and so must have a fixed length. Thus, those lists are much
like arrays. 

This can be a problem as the access predicates' clauses for elements of lists in
terms' arguments are automatically generated at the rate of one for each element
that must be accessed. Thus, those lists are much like arrays, with a fixed
size. Indeed, the motivation for this strange implementation is efficiency:
list-type terms can now be accessed at a constant time. 

In order to avoid adding new clauses of inspect predicates for list-type terms
every time a new term has to be added to a list (and removing that clause when
that term must be removed from the list!), a "list budget" for each list-type
term must be set in the configuration option `list_budget/2`.

```
% List budget option in the specification.

list_budget(group,5).
list_budget(contact,10).
```

In that option, the first argument is the name of the list-type term for which
the budget is set, while the second term is the number of elements of a list
that will contain elements of the listed kind of term. Thus, setting the
list-budget for "group" terms to "5" will make the system generate five clauses
accessing elements of lists of group terms:

```
% list_budget(group,5).

inspect(group,GroupId,group(GroupId,Contacts,Count),[group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,_,_,group(GroupId,Contacts,Count)|_]).

% Other stuff
```

While setting it to "3" will only generate three:

```
% list_budget(group,0).

inspect(group,GroupId,group(GroupId,Contacts,Count),[group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,group(GroupId,Contacts,Count)|_]).
inspect(group,GroupId,group(GroupId,Contacts,Count),[_,_,group(GroupId,Contacts,Count)|_]).

% Other stuff
```

What happens if you set the list-budget for a list-type term to "0"? Why don't
you try that and find out?


```
Note:

The alternative to lists of list-type terms with a fixed length is to manipulate
the dynamic database, with assert/1 and retract/1 and friends, e.g. to add a new
clause of an inspect/4 predicate for a group term, when a new group is created,
or to remove that clause when the group is un-grouped. Let me be frank: this
will only happen over my cold, dead body. The dynamic database is evil - SP.
```

Useful queries
--------------

The following is a list of useful queries testing various bits of functionality,
currently mainly to do with the vector access primitives.

In the following sections queries are listed along with their results at the
time of writing. Ultimately it would be a good idea to have a more concise
documentation of all this functionality. And maybe some unit tests to make sure
nothing breaks every once in a while.

### Vector Initialisation

```
% List state vector specification:

  ?- vector_specification(_Ss), maplist(writeln,_Ss).
  state_vector_term(map_area,[x_min:length,x_max:length,y_min:length,y_max:length])
  state_vector_term(scan_area,[x_min:length,x_max:length,y_min:length,y_max:length])
  state_vector_term(position,[x:coordinate,y:coordinate,theta:angle])
  state_vector_term(scan_params,[scan_width:float,leadinleadout:float,min_scan_length:float,nadir_width:float,boat_speed:speed,time_limit:time,min_scan_angle_diff:length])
  state_vector_term(observations,[contacts:list(contact),count:integer])
  state_vector_term(grouped_observations,[groups:list(group),count:integer])
  state_vector_term(group,[groupId:id,contacts:list(id),count:integer])
  state_vector_term(contact,[contactId:id,actionId:id,time:time,x:coordinate,y:coordinate,range:distance,scan_angle:angle])
  true.

% List initialisation values in specification:

  ?- vector_initialisation(_Is), maplist(writeln,_Is).
  state_vector_term_init(map_area,[x_min: -20,x_max:220,y_min: -20,y_max:120])
  state_vector_term_init(scan_area,[x_min:0,x_max:200,y_min:0,y_max:100])
  state_vector_term_init(position,[x:0.0,y:0.0,theta:0.0])
  state_vector_term_init(scan_params,[scan_width:20.0,leadinleadout:5.0,min_scan_length:10.0,nadir_width:3.0,boat_speed:20.0,time_limit:500.0,min_scan_angle_diff:30])
  state_vector_term_init(observations,[contacts:[],count:0])
  state_vector_term_init(grouped_observations,[groups:[],count:0])
  true.

% Initialise vector to default values by type:

  ?- initialised_state_vector(_Vs), maplist(writeln,_Vs).
  map_area(x_min(0),x_max(0),y_min(0),y_max(0))
  scan_area(x_min(0),x_max(0),y_min(0),y_max(0))
  position(x(0.0),y(0.0),theta(0.0))
  scan_params(scan_width(0.0),leadinleadout(0.0),min_scan_length(0.0),nadir_width(0.0),boat_speed(0.0),time_limit(0.0),min_scan_angle_diff(0))
  observations(contacts([]),count(0))
  grouped_observations(groups([]),count(0))
  true.

% Initialise state vector to specification values:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs), maplist(writeln,_Vs).
  map_area(x_min(-20),x_max(220),y_min(-20),y_max(120))
  scan_area(x_min(0),x_max(200),y_min(0),y_max(100))
  position(x(0.0),y(0.0),theta(0.0))
  scan_params(scan_width(20.0),leadinleadout(5.0),min_scan_length(10.0),nadir_width(3.0),boat_speed(20.0),time_limit(500.0),min_scan_angle_diff(30))
  observations(contacts([]),count(0))
  grouped_observations(groups([]),count(0))
  true.
```

### Term initialisation

```
% Initialise top-level term to defaults:

  ?- initialised_term(map_area,MA).
  MA = map_area(x_min(0), x_max(0), y_min(0), y_max(0)).

% Initialise top-level term to simulation values:

  ?- _I = state_vector_term_init(map_area,[x_min:(-20),x_max:220,y_min:(-20),y_max:120]), initialised_term(_I,MA).
  MA = map_area(x_min(-20), x_max(220), y_min(-20), y_max(120)).
```

### List-type term initialisation

```
% Initialise list-type term to defaults:

  ?- initialised_term(contact,C).
  C = contact(contactId(0), actionId(0), time(0.0), x(0.0), y(0.0), range(0), scan_angle(0.0)).

  ?- initialised_term(group,G).
  G = group(groupId(0), contacts([]), count(0)).

% Initialise list-type term to simulation values:

  ?- _I = state_vector_term_init(contact,[contactId:0,actionId:3,time:488.4,x:21.8,y:38.2,range:0,scan_angle:0.0]), initialised_term(_I,C).
  C = contact(contactId(0), actionId(3), time(488.4), x(21.8), y(38.2), range(0), scan_angle(0.0)).

  ?- _I = state_vector_term_init(group,[groupId:0,contacts:[0,3,7,8,10,11,12],count:7]), initialised_term(_I,G).
  G = group(groupId(0), contacts([0, 3, 7, 8, 10, 11, 12]), count(7)).

% Try to initialise arguments to wrong types - raises type error:

  ?- _I = state_vector_term_init(contact,[contactId:0,actionId:3,time:488.4,x:21.8,y:38.2,range:0.0,scan_angle:0.0]), initialised_term(_I,C).
  ERROR: Type error: `distance' expected, found `0.0' (a float) (Argument: range:distance->0.0)
  ERROR: In:
  ERROR:   [18] throw(error(type_error(distance,0.0),context(...,'Argument: range:distance->0.0')))
  ERROR:   [16] '<meta-call>'(state_vector:(...,...)) <foreign>
  ERROR:   [15] findall_loop(_34882,state_vector:(...,...),_34886,[]) at c:/program files/swipl/boot/bags.pl:109
  ERROR:   [14] cleanup_bag('$bags':findall_loop(_34936,...,_34940,[]),'$bags':'$destroy_findall_bag') at c:/program files/swipl/boot/bags.pl:106
  ERROR:   [11] state_vector:initialised_term(state_vector_term_init(contact,[...|...]),6) at c:/users/yegoblynqueenne/downloads/phd_reading_material/post_docs/surrey/explainable_ml_project/toy_problem/prolog_code/src/state_vector.pl:302
  ERROR:    [9] toplevel_call(user:user: ...) at c:/program files/swipl/boot/toplevel.pl:1173
  ERROR: 
  ERROR: Note: some frames are missing due to last-call optimization.
  ERROR: Re-run your program in debug mode (:- debug.) to get more detail.
     Exception: (14) cleanup_bag('$bags':findall_loop(_33868, state_vector:(nth1(_33022, [contactId:0, actionId:3, ... : ...|...], _33886:_33888), nth1(_33022, [contactId:id, ... : ...|...], _33886:_33908), argument_of_value((_33886:_33908->_33888), _33868)), _33928, []), '$bags':'$destroy_findall_bag') ? abort
  % Execution Aborted
```

### Primitives file

```
% Write access terms to output file defined in configuration:

  ?- write_primitives, primitives_file(_F), edit(_F).
  true.

% Load the generated primitives file to memory:

    ?- configuration:primitives_file(_F), use_module(_F).
    true.

```

### Inspect predicates

Ensure that the primitives module file is loaded into memory, as shown earlier
(with `use_module/1`).


```
% Inspect term in vector initialised to specification values:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs), inspect(map_area,M,_Vs).
  M = map_area(x_min(-20), x_max(220), y_min(-20), y_max(120)).

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs), inspect(map_area,x_max,X_Max,_Vs).
  X_Max = x_max(220) ;
  false.


% Inspect element of list-type argument by index:

  ?- _I = state_vector_term_init(contact,[contactId:0,actionId:3,time:488.4,x:21.8,y:38.2,range:0,scan_angle:0.0]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), initialised_term(_I,_C), add_new_contact(_Vs1,_C,_Vs2), inspect(observations,contacts,contacts(_Cs),_Vs2), inspect(contact,contactId(0),C,_Cs).
  C = contact(contactId(0), actionId(3), time(488.4), x(21.8), y(38.2), range(0), scan_angle(0.0)) ;
  false.

  ?- _I = state_vector_term_init(group,[groupId:0,contacts:[0,3,7,8,10,11,12],count:7]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), initialised_term(_I,_G), add_new_group(_Vs1,_G,_Vs2), inspect(grouped_observations,groups,groups(_Gs),_Vs2), inspect(group,groupId(0),Gi,_Gs).
  Gi = group(groupId(0), contacts([0, 3, 7, 8, 10, 11, 12]), count(7)) ;
  false.

% Inspect argument of term in list:

  ?- _I = state_vector_term_init(contact,[contactId:0,actionId:3,time:488.4,x:21.8,y:38.2,range:0,scan_angle:0.0]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), initialised_term(_I,_C), add_new_contact(_Vs1,_C,_Vs2), inspect(observations,contacts,contacts(_Cs),_Vs2), inspect(contact,contactId(0),_Ci,_Cs), inspect(contact,actionId,Id,_Ci).
  Id = actionId(3) ;
  false.

  ?- _I = state_vector_term_init(group,[groupId:0,contacts:[0,3,7,8,10,11,12],count:7]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), initialised_term(_I,_G), add_new_group(_Vs1,_G,_Vs2), inspect(grouped_observations,groups,groups(_Gs),_Vs2), inspect(group,groupId(0),_Gi,_Gs), inspect(group,count,Count,_Gi).
  Count = count(7) ;
  false.
```

### Modify predicates

```
% Modify top-level term (by complete replacement):

  ?- initialised_state_vector(_Vs1), _I = state_vector_term_init(map_area,[x_min:(-20),x_max:220,y_min:(-20),y_max:120]), initialised_term(_I,_MA), modify(map_area,_MA,_Vs1,_Vs2), maplist(writeln,_Vs2).
  map_area(x_min(-20),x_max(220),y_min(-20),y_max(120))
  scan_area(x_min(0),x_max(0),y_min(0),y_max(0))
  position(x(0.0),y(0.0),theta(0.0))
  scan_params(scan_width(0.0),leadinleadout(0.0),min_scan_length(0.0),nadir_width(0.0),boat_speed(0.0),time_limit(0.0),min_scan_angle_diff(0))
  observations(contacts([]),count(0))
  grouped_observations(groups([]),count(0))
  true.

% Modify argument of term - increment a value:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(position,P,_Vs1), modify(position,increment,x,_Vs1,_Vs2), inspect(position,P2,_Vs2).
  P = position(x(0.0), y(0.0), theta(0.0)),
  P2 = position(x(0.1), y(0.0), theta(0.0)) ;
  false.

% Modify argument of term - add an element (dummy):

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(observations,Os1,_Vs1), modify(observations,add,contacts(placeholder),_Vs1,_Vs2), inspect(observations,Os2,_Vs2).
  Os1 = observations(contacts([]), count(0)),
  Os2 = observations(contacts([placeholder]), count(0)).

```

### Auxiliary predicates

Auxiliary predicats are used to compose PDDL-like actions.

```
% Add new contact initialised to defaults:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(observations,Os1,_Vs1), initialised_term(contact,_C), add_new_contact(_Vs1,_C,_Vs2), inspect(observations,Os2,_Vs2).
  Os1 = observations(contacts([]), count(0)),
  Os2 = observations(contacts([contact(contactId(0), actionId(0), time(0.0), x(0.0), y(0.0), range(0), scan_angle(0.0))]), count(1)) ;
  false.

% Add new contact initialised to simulation values:

  ?- _I = state_vector_term_init(contact,[contactId:0,actionId:3,time:488.4,x:21.8,y:38.2,range:0,scan_angle:0.0]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(observations,Os1,_Vs1), initialised_term(_I,_C), add_new_contact(_Vs1,_C,_Vs2), inspect(observations,Os2,_Vs2).
  Os1 = observations(contacts([]), count(0)),
  Os2 = observations(contacts([contact(contactId(0), actionId(3), time(488.4), x(21.8), y(38.2), range(0), scan_angle(0.0))]), count(1)) ;
  false.

% Add new group initialised to defaults:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(grouped_observations,Gs1,_Vs1), initialised_term(group,_G), add_new_group(_Vs1,_G,_Vs2), inspect(grouped_observations,Gs2,_Vs2).
  Gs1 = grouped_observations(groups([]), count(0)),
  Gs2 = grouped_observations(groups([group(groupId(0), contacts([]), count(0))]), count(1)) ;
  false.

% Add new group initialised to simulation values:

  ?- _I = state_vector_term_init(group,[groupId:0,contacts:[0,3,7,8,10,11,12],count:7]), vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(grouped_observations,Gs1,_Vs1), initialised_term(_I,_G), add_new_group(_Vs1,_G,_Vs2), inspect(grouped_observations,Gs2,_Vs2).
  Gs1 = grouped_observations(groups([]), count(0)),
  Gs2 = grouped_observations(groups([group(groupId(0), contacts([0, 3, 7, 8, 10, 11, 12]), count(7))]), count(1)) ;
  false.

% Try adding group to contacts - nothing is added:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(grouped_observations,Gs1,_Vs1), initialised_term(group,_G), add_new_contact(_Vs1,_G,_Vs2), inspect(grouped_observations,Gs2,_Vs2).
  Gs1 = Gs2, Gs2 = grouped_observations(groups([]), count(0)) ;
  false.

% Create new group and add to groups, changing its default-ish group id:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), create_group([0,3,7,8,10,11,12],G), add_new_group(_Vs1,G,_Vs2), inspect(grouped_observations,Gs2,_Vs2).
  G = group(groupId(-1), contacts([0, 3, 7, 8, 10, 11, 12]), count(7)),
  Gs2 = grouped_observations(groups([group(count(0), contacts([0, 3, 7, 8, 10, 11, 12]), count(7))]), count(1)) ;
  false.

% Remove contact from group:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_Vs1), inspect(observations,Os1,_Vs1), initialised_term(contact,_C), add_new_contact(_Vs1,_C,_Vs2), inspect(observations,Os2,_Vs2), modify(observations,delete,contacts(_C),_Vs2,_Vs3), inspect(observations,Os3,_Vs3).
  Os1 = observations(contacts([]), count(0)),
  Os2 = observations(contacts([contact(contactId(0), actionId(0), time(0.0), x(0.0), y(0.0), range(0), scan_angle(0.0))]), count(1)),
  Os3 = observations(contacts([]), count(1)) ;
  false.
```

### PDDL-like actions

```
% Basic simulator: move action; set starting position at (1.0, 1.0) first
% to allow southerly and westerly moves:

  % In specification:
  % state_vector_term_init(position,[x:1.0,y:1.0,theta:0.0]).

  ?- _Ms = [move_north, move_north_east, move_east, move_south_east, move_south, move_south_west, move_west, move_north_west], member(_M,_Ms), vector_initialisation(_Is), initialised_state_vector(_Is,_S1), inspect(position,P1,_S1), _Move =..[_M,_S1,_S2], call(_Move), writeln(_M), inspect(position,P2,_S2).
  move_north
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(1.1), y(1.0), theta(0.0)) ;
  move_north_east
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(1.1), y(1.1), theta(0.45)) ;
  move_east
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(1.0), y(1.1), theta(0.9)) ;
  move_south_east
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(0.9), y(1.1), theta(0.135)) ;
  move_south
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(0.9), y(1.0), theta(0.18)) ;
  move_south_west
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(0.9), y(0.9), theta(0.225)) ;
  move_west
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(1.0), y(0.9), theta(0.27)) ;
  move_north_west
  P1 = position(x(1.0), y(1.0), theta(0.0)),
  P2 = position(x(1.1), y(0.9), theta(0.315)) ;
  false.

% Basic Simulator: group action:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_S1), initialised_term(contact,_C0), initialised_term(contact,_C02), modify(contact,set,contactId(1),_C02,_C1), add_new_contact(_S1,_C0,_S2), add_new_contact(_S2,_C1,_S3), group_contacts(_S3,[contactId(0),contactId(1)],_S4), inspect(grouped_observations,Gs,_S4).
  Gs = grouped_observations(groups([group(groupId(0), contacts([contactId(0), contactId(1)]), count(2))]), count(1)) ;
  false.

% Basic Simulator: ungroup action:

  ?- vector_initialisation(_Is), initialised_state_vector(_Is,_S1), initialised_term(contact,_C0), initialised_term(contact,_C02), modify(contact,set,contactId(1),_C02,_C1), add_new_contact(_S1,_C0,_S2), add_new_contact(_S2,_C1,_S3), group_contacts(_S3,[contactId(0),contactId(1)],_S4), inspect(grouped_observations,Gs1,_S4), ungroup_contacts(_S4,groupId(0),_S5), inspect(grouped_observations,Gs2,_S5).
  Gs1 = grouped_observations(groups([group(groupId(0), contacts([contactId(0), contactId(1)]), count(2))]), count(1)),
  Gs2 = grouped_observations(groups([]), count(0)) ;
  false.

```
