:-module(model_configuration, [list_budget/2
                              ,map/3
                              ,move_constraint/1
                              ,paths_episodes_root/1
                              ,paths_save_loc/1
                              ,paths_plans_root/1
                              ,paths_python_root/1
                              ,primitives_file/1
                              ,state_vector_terms/1
                              ,stride/2
                              ,term_arguments/1
                              ]).

/** <module> Configuration options.
*/

:-dynamic list_budget/2, stride/2.

%!      list_budget(?Term,?Budget) is semidet.
%
%       Max cardinality of a compiled access list for the given Term.
%
list_budget(action,200).
list_budget(group,5).
list_budget(contact,10).
% list_budget(obstacle,10).


:-dynamic map/3.

%!      map(?Id,?Max_x,?Max_y) is semidet.
%
%       A map of an environment in the simulation.
%
%       Id is the identifier of a map where the simulation will run.
%
%       Max_y and Max_y are the maximum x and y coordinates of the map.
%
%map(model_root(data/occ_grid_1),400,255).
%map(model_root(data/occ_grid_1a),400,255).
%map(model_root(data/occ_grid_1b),400,255).
%map(model_root(data/occ_grid_1c),400,255).
%map(model_root(data/occ_grid_1d),400,255).
%map(model_root(data/occ_grid_1e),400,255).
map(model_root(data/occ_grid_2_free),250,180).
%map(model_root(data/occ_grid_2_obstructed),250,180).
%map(model_root(data/occ_grid_3),710,741).
%map(model_root(data/occ_grid_west_north),250,180).
%map(model_root(data/occ_grid_west_south),250,180).
%map(model_root(data/occ_grid_middle_north),250,180).
%map(model_root(data/occ_grid_middle_south),250,180).
%map(model_root(data/occ_grid_east_north),250,180).
%map(model_root(data/occ_grid_east_south),250,180).


%!      move_constraint(?Predicate) is semidet.
%
%       Predicate indicator, Symbol/arity of a move constraint.
%
%       Used to choose the constraints that will apply to move actions.
%
%       Known constraints:
%
%       *within_action_budget/2: constraints the length of the list of
%       actions in the state plan term in the state vector to the value
%       of B in list_budget(action,B).
%
%       *in_scan_area/2: rejects moves that end with the boat outside
%       the limits defined in the scan area term in the state vector.
%
%       *passable/2: rejects moves that end in unpassable map terrain
%       as read off the current occlusion grid map.
%
%       *moving_forward/2: ensures the boat keeps moving towards the
%       coordinates of the destination term in the state vector.
%
%move_constraint(within_action_budget/2).
move_constraint(in_scan_area/2).
move_constraint(passable/2).
move_constraint(moving_forward/2).


%!      paths_episodes_root(?Root) is semidet.
%
%       Root directory of training and testing episodes.
%
%       Used by path_exec.pl.
%
%paths_episodes_root('bath/episodes').
paths_episodes_root('obstacle_avoidance/episodes').


%!      paths_save_loc(?Location) is semidet.
%
%       Save location for playback episodes.
%
%       Used by path_exec.pl.
%
paths_save_loc('../episodes/playback').


%!      paths_plans_root(?Root) is semidet.
%
%       Root directory for learned plan files.
%
%       Used by path_exec.pl.
%
paths_plans_root('obstacle_avoidance/episodes/plans').


%!      paths_python_root(?Root) is semidet.
%
%       Root directory for Python plan scripts.
%
%       Used by path_exec.pl.
%
paths_python_root('obstacle_avoidance/python').


%!      primitives_file(?Path) is semidet.
%
%       Path to the output file of generated primitive state operations.
%
%       The name of the output file, without extension, is also used as
%       the module name for the vector access module.
%
primitives_file(model_output('vector_access.pl')).


%!      state_vector_terms(?Names) is semidet.
%
%       Names of terms to include in a state vector.
%
%       Used to select state_vector_term/1 clauses to generate primitive
%       state operations for.
%
state_vector_terms([destination
                   ,position
                   %,plan
                   %,map_area
                   ,scan_area
                   %,scan_params
                   %,observations
                   %,grouped_observations
                   %,group
                   %,contact
                   ,action
                   %,obstacles
                   %,objects
                   %,obstacle
                   ]).


%!      stride(?Type,?Stride) is semidet.
%
%       Stride for modification of terms of a Type.
%
%       Type is the atomic name of one of the types known to the system.
%       Type must be a numeric type (i.e. either integer or float, or
%       one of their subtypes). It must also be modifiable.
%
%       Stride is an increment by which to modify terms of the given
%       Type.
%
stride(integer,1).
stride(float,10.0).


%!      term_arguments(?Form) is semidet.
%
%       Whether to mark arguments of terms by their names.
%
%       This predicate controls the form of arguments of state-vector
%       terms, as constructed by initialised_terms/2.
%
%       Form must be one of: terms, constants.
%
%       If Form is 'terms', arguments of state-vector terms are
%       themselves compound Prolog terms where the functor name is the
%       name of the argument and the single argument is a value of the
%       appropriate type, as defined in the current specification of
%       state-vector terms.
%
%       If Bool is 'constants', then arguments of state-vector terms are
%       values of the type defined in the specification.
%
%       Examples:
%       ==
%       % term_arguments(terms)
%
%       ?- initialised_terms([x_min:length,x_max:length,y_min:length,y_max:length],I).
%       I = [x_min(0), x_max(0), y_min(0), y_max(0)].
%
%       % named_argument(constants)
%       ?- initialised_terms([x_min:length,x_max:length,y_min:length,y_max:length],I).
%       I = [0, 0, 0, 0].
%       ==
%
%       The trade-off between the two Forms is that 'constant' form
%       makes manipulation (e.g. by modify/5 operations) simpler, while
%       'term' makes debugging easier.
%
term_arguments(terms).
%term_arguments(constants).
