:-module(actions, [move/5
                  ,move/4
                  ,move_north/2
                  ,move_north_east/2
                  ,move_east/2
                  ,move_south_east/2
                  ,move_south/2
                  ,move_south_west/2
                  ,move_west/2
                  ,move_north_west/2
                  ,debug_move/3
                  ,move_constraints/2
                  ,in_scan_area/2
                  ,bounded_by/3
                  ,passable/2
                  ,passable_coords/2
                  ,map/5
                  ,truncate/2
                  ,moving_forward/2
                  ,magnitude/3
                  ,group_contacts/3
                  ,ungroup_contacts/3
                  ,add_new_group/3
                  ,create_group/2
                  ,add_new_contact/3
                  ,create_contact/7
                  ,register_action/5
                  ,register_action/4
                  ,action_params/3
                  ,within_action_budget/1
                  ,create_action/4
                  ,atomise_params/2
                  ]).

:-use_module(model_output(vector_access)).
:-use_module(model_src(state_vector)).
:-use_module(model_src(types)).

/** <module> Actions that can be taken by an agent.

*/

%!      load_map is det.
%
%       Load the occlusion grid for a map.
%
%       The map to be loaded is defined in model_configuration.pl.
%
load_map :-
        model_configuration:map(P,_,_)
        ,load_files(P, [module(occlusion_grid)
                       ,redefine_module(true)
                       ])
        ,debug(load_map,'Loaded Map ~w',[P]).
:-load_map.


%!      move_north(+State1,-State2) is det.
%
%       Move the robot on a North heading.
%
move_north(S1,S2):-
        move(move_north,'North',S1,false/increment,S2).


%!      move_north_east(+State1,-State2) is det.
%
%       Move the robot on a North-East heading at 45 degrees.
%
move_north_east(S1,S2):-
        move(move_north_east,'North East',S1,increment/increment,S2).


%!      move_east(+State1,-State2) is det.
%
%       Move the robot on an East heading.
%
move_east(S1,S2):-
        move(move_east,'East',S1,increment/false,S2).


%!      move_south_east(+State1,-State2) is det.
%
%       Move the robot on a South-East heading at 135 degrees.
%
move_south_east(S1,S2):-
        move(move_south_east,'South East',S1,increment/decrement,S2).


%!      move_south(+State1,-State2) is det.
%
%       Move the robot on a South heading.
%
move_south(S1,S2):-
        move(move_south,'South',S1,false/decrement,S2).


%!      move_south_west(+State1,-State2) is det.
%
%       Move the robot on a South-West heading at 255 degrees.
%
move_south_west(S1,S2):-
        move(move_south_west,'South West',S1,decrement/decrement,S2).


%!      move_west(+State1,-State2) is det.
%
%       Move the robot on a West heading.
%
move_west(S1,S2):-
        move(move_west,'West',S1,decrement/false,S2).


%!      move_north_west(+State1,-State2) is det.
%
%       Move the robot on a North-West heading at 315 degrees.
%
move_north_west(S1,S2):-
        move(move_north_west,'North West',S1,decrement/increment,S2).


%!      move(+Name,+Debug,State1,+Deltas,-State2) is det.
%
%       Make one move to one of the eight clock directions.
%
%       Abstracts away the eight PDDL-like, lower-level moves.
%
%       Name is the symbol of a move predicate, in: [move_north,
%       move_north_east, move_east, move_south_east, move_south,
%       move_south_west, move_west].
%
%       Debug is an atomic form of Name used to debug the move.
%
%       State1 is the input state vector of the move.
%
%       Deltas is a pair X/Y where each of X and Y can be one of the
%       atoms: [false,increment,decrement]. Each atom in the pair
%       determines how, and whether, the corresponding coordinate (i.e.
%       x or y for X and Y in X/Y, respectively) will be modified.
%
%       State2 is the output state vector of the move.
%
:-if((model_configuration:state_vector_terms(Ss)
     ,memberchk(plan,Ss)
     )
    ).
move(N,DS,S1,Dx/Dy,S2):-
        %modify(position,set,theta(0.0),S1,S3) ??
        move(Dx,x,S1,S3)
        ,move(Dy,y,S3,S4)
        ,move_constraints(S1,S4)
        ,register_action(S4,move,N,S2)
        ,debug_move(DS,S1,S2).
:-else.
move(_N,DS,S1,Dx/Dy,S2):-
        %modify(position,set,theta(0.0),S1,S3) ??
        move(Dx,x,S1,S3)
        ,move(Dy,y,S3,S2)
        ,move_constraints(S1,S2)
        ,debug_move(DS,S1,S2).
:-endif.

%!      move(+Delta,+Coordinate,+State1,-State2) is det.
%
%       Modify one coordiate in the position term.
%
%       Delta is one of [false, increment, decrement], denoting whether
%       and how the given Coordinate is to be modified.
%
%       Coordinate is one of the atoms: [x,y], denoting what coordinate
%       is to be modified.
%
%       State1 and State2 are the input and output state-vectors for the
%       move modelled by this predicate.
%
move(false,_XY,Ss,Ss).
move(D,XY,S1,S2):-
        D \= false
        ,modify(position,D,XY,S1,S2).


%!      debug_move(+Name,+State1,+State2) is det.
%
%       Debug a Named move from State1 to State2.
%
debug_move(N,S1,S2):-
        inspect(position,position(X1,Y1,Theta1),S1)
        ,inspect(position,position(X2,Y2,Theta2),S2)
        ,debug(actions,'Moved ~w from ~w to ~w',[N
                                                ,position(X1,Y1,Theta1)
                                                ,position(X2,Y2,Theta2)
                                                ]).


%!      move_constraints(+State1,+State2) is det.
%
%       Check move constraints in current State1 and next State2.
%
%       State1, State2 are state vectors. State1 is the state vector of
%       the state at the start of a move and State2 is the state vector
%       at the end of a move.
%
%       This predicate goes through all constraints defined in the model
%       configuration option move_constraint/1 and calls each of them.
%
%       Constraints must be defined as dyadic predicates. State1 and
%       State2 will be passed to each such predicate.
%
%       If any of those constraints fails, a move action transitioning
%       from State1 to State2 will not be taken.
%
move_constraints(S1,S2):-
        forall(model_configuration:move_constraint(C/2)
              ,(T =.. [C,S1,S2]
               ,call(T)
               )
              ).


%!      in_scan_area(+State1,+State2) is det.
%
%       True when the robot is within the scan area limits.
%
%       Used as move constraint. Enabled by setting option
%       move_constraint(in_scan_area/2) in model configuration.
%
%       Does not need to use State2.
%
in_scan_area(S,_):-
        inspect(scan_area,scan_area(X_min,X_max,Y_min,Y_max),S)
        ,inspect(position,x,X,S)
        ,inspect(position,y,Y,S)
        ,bounded_by(X,X_min,X_max)
        ,bounded_by(Y,Y_min,Y_max).


%!      bounded_by(+X,+Lower,+Upper) is det.
%
%       True when X is within a Lower and Upper bound.
%
bounded_by(X,L,U):-
        numeric_inequality(L,=<,X)
        ,numeric_inequality(X,=<,U).


%!      passable(+State1,+State2) is det.
%
%       True when in the State2 the agent is on passable terrain.
%
%       Used as move constraint. Enabled by setting option
%       move_constraint(passable/2) in model configuration.
%
%       Does not need to use State1.
%
passable(_,S2):-
        inspect(position,position(X,Y,_Theta),S2)
        ,passable_coords(X,Y).



%!      passable_coords(+X,+Y) is det.
%
%       True when the terrain at coordinates X/Y is passable.
%
%       X and Y are of type coordinate and denote the coordinates of a
%       location in a map used in the simulation.
%
%       The map is the one set in the model_configuration option map/3.
%
passable_coords(X,Y):-
        model_configuration:map(Id,Max_x,_)
        ,map(Id,Max_x,X,Y,0)
        ,debug(passable,'Passable terrain at ~w/~w.',[X,Y]).


%!      map(+Id,+Max_x,+X,+Y,+Terrain) is det.
%
%       Query the type of Terrain in a map at the given coordinates.
%
%       Id is the identifier of a map in the model configuration.
%
%       Max_x is the maximum coordinate in the x direction in the map.
%
%       X and Y are the coordinates of a location in the map.
%
%       Terrain is the terrain type at that location, according to the
%       occupancy grid generated from the map: either "0" for passable
%       or "1" for unpassable, terrain. The agent can't move over
%       unpassable terrain. Duh.
%
map(_Id,Max_x,X,Y,T):-
        succ(Max_x,Max_x_)
        ,maplist(term_value,[X,Y],[Xv,Yv])
        ,maplist(truncate,[Xv,Yv],[Xi,Yi])
        ,functor(R,occ_grid,Max_x_)
        ,nb_setarg(1,R,Yi)
        % Skip over row-index first arg of map term.
        % Plus one because the Xs are 0-based but arg/3 is not.
        ,Xp is Xi + 2
        ,call(R)
        ,arg(Xp,R,T).


%!      truncate(+Float,-Integer) is det.
%
%       Truncate a Float to an Integer.
%
%       Helper predicate used with maplist/2 in map/5.s
%
truncate(X,Y):-
        Y is truncate(X).


%!      moving_forward(+State1,+State2) is det.
%
%       Ensure the agent is moving towards its destination.
%
%       True when the distance of the agent's position in State1 from
%       its destination is less than the distance of the agent's
%       position State2 from its destination.
%
%       Used as move constraint. Enabled by setting option
%       move_constraint(moving_forward/2) in model configuration.
%
moving_forward(S1,S2):-
        inspect(position,position(X1,Y1,_),S1)
        ,inspect(position,position(X2,Y2,_),S2)
        ,inspect(destination,destination(Xd,Yd,_),S1)
        ,maplist(term_value,[X1,Y1,X2,Y2,Xd,Yd],[X1_v,Y1_v,X2_v,Y2_v,Xd_v,Yd_v])
        ,magnitude(Xd_v/Yd_v,X1_v/Y1_v,M1)
        ,magnitude(Xd_v/Yd_v,X2_v/Y2_v,M2)
        ,M2 < M1.


%!      magnitude(+C1,+C2,-Magnitude) is det.
%
%       Magnitude of a vector between two sets of coordinates.
%
%       Example:
%       ==
%       ?- actions:magnitude(0/0,3/4,M).
%       M = 5.0.
%       ==
%
magnitude(X1/Y1,X2/Y2,M):-
        Dx is X2 - X1
        ,Dy is Y2 - Y1
        ,M is sqrt(Dx^2 + Dy^2).


%!      angle(+C1,+C2,-Radians) is det.
%
%       Angle of a vector between two sets of coordinates.
%
angle(X1/Y1,X2/Y2,R):-
        Dx is X2 - X1
        ,Dy is Y2 - Y1
        ,R is atan(Dx/Dy).


%!      within_action_budget(+State) is det.
%
%       True when the action budget in State is not yet exhausted.
%
within_action_budget(S):-
        model_configuration:list_budget(action,B)
        ,inspect(plan,count,C,S)
        ,term_value(C,V)
        , V < B.



%!      group_contacts(+State1,+Contacts,-State2) is det.
%
%       Group a set of Contacts.
%
%       Contacts is a list of Ids of contacts observed in State1, to be
%       added to a new group in State2.
%
%       Preconditions:
%       * There must be at least two contact ids in Contacts.
%       * Each contact Id in Contacts must be the Id of a contact in the
%       list of Contacts in the observation term in State1.
%       * None of the contact Ids in Contacts may be in the list
%       Contacts of a group in the grouped_observations term in State1.
%
%       Effets:
%       * A new group term is created with its list of contacts set to
%       the given Contacts.
%       * The new group is added to the list of groups in the
%       grouped_observations term in State2.
%
%
group_contacts(S1,Cs,S2):-
        Cs = [_C1,_C2|_]
        ,in_observations(S1,Cs)
        ,create_group(Cs,G)
        ,\+ in_grouped_observations(S1,Cs)
        ,add_new_group(S1,G,S3)
        ,register_action(S3,group,group_contacts,Cs,S2).


%!      in_observations(+State,+Contacts) is det.
%
%       True when a list of Contacts is observed in a State.
%
in_observations(Ss,Cs1):-
        inspect(observations,contacts,contacts(Cs),Ss)
        ,findall(ContactId
                ,inspect(contact,ContactId,_C,Cs)
                ,Cs2)
        ,sort(Cs1,Cs1_s)
        ,sort(Cs2,Cs2_s)
        ,ord_subtract(Cs1_s,Cs2_s,[]).


%!      in_grouped_observations(+State,+Contacts) is det.
%
%       True when a list of contacts is grouped in a State.
%
in_grouped_observations(Ss,Cs):-
        sort(Cs,Cs_s)
        ,inspect(grouped_observations,groups(Gs),Ss)
        ,forall(inspect(group,_GroupId,Gi,Gs)
               ,(inspect(group,contacts,Cs_i,Gi)
                ,sort(Cs_i,Csi_s)
                ,ord_intersect(Cs_s,Csi_s,[])
                )
               ).



%!      ungroup_contacts(+State1,+Group,-State2) is det.
%
%       Undo the grouping of contacts in a state.
%
ungroup_contacts(S1,GroupId,S2):-
        in_groups(S1,GroupId)
        ,group_with_id(S1,GroupId,G)
        ,modify(grouped_observations,delete,groups(G),S1,S3)
        ,modify(grouped_observations,decrement,count,S3,S4)
        ,register_action(S4,ungroup,ungroup_contacts,[GroupId],S2).


%!      in_groups(+State,+GroupId) is det.
%
%       True when a group with the given GroupId exists in a State.
%
in_groups(Ss,GroupId):-
        inspect(grouped_observations,groups,groups(Gs),Ss)
        ,inspect(group,GroupId,_G,Gs).


%!      group_with_id(+State,+GroupId,-Group) is det.
%
%       Retrive a Group in a State by its GroupId.
%
group_with_id(Ss,GroupId,G):-
        inspect(grouped_observations,groups,groups(Gs),Ss)
        ,inspect(group,GroupId,G,Gs).



%!      add_new_group(+State1,+Group,-State2) is det.
%
%       Add a new Group to the current state.
%
add_new_group(S1,G,S2):-
        inspect(grouped_observations,count,C,S1)
        ,term_value(C,V)
        ,modify(group,set,groupId(V),G,G_)
        ,modify(grouped_observations,add,groups(G_),S1,S3)
        ,modify(grouped_observations,increment,count,S3,S2).


%!      create_group(+Contacts,-Group) is det.
%
%       Create a new Group given a list of Contacts.
%
create_group(Cs,G):-
        length(Cs,N)
        ,I = state_vector_term_init(group,[groupId:(-1),contacts:Cs,count:N])
        ,initialised_term(I,G).


%!      add_new_contact(+State1,+Contact,-State2) is det.
%
%       Add a new Contact to the current state.
%
add_new_contact(S1,C,S2):-
        inspect(observations,count,K,S1)
        ,term_value(K,V)
        ,modify(contact,set,contactId(V),C,C_)
        ,modify(observations,add,contacts(C_),S1,S3)
        ,modify(observations,increment,count,S3,S2).



%!      create_contact(+A,+T,+X,+Y,+R,+Th,-Contact) is det.
%
%       Create a new Contact term.
%
%       A, T, X, Y R and Th are the arguments necessary to initialise a
%       Contact term:
%       * A is an actionId
%       * T is a time
%       * X is the x coordinate
%       * Y is the y coordinate
%       * R is the range
%       * Th is the scan angle
%
create_contact(A,T,X,Y,R,Th,C):-
        I = state_vector_term_init(contact,[contactId:(-1)
                                           ,actionId:A
                                           ,time:T
                                           ,x:X
                                           ,y:Y
                                           ,range:R
                                           ,scan_angle:Th])
        ,initialised_term(I,C).



%!      register_action(+State1,+Type,+Name,+Parameters,-State2) is det.
%
%       Add an action to the plan stored in the current State.
%
%       Type is the atomic type of the action. For the Toy Problem
%       that's currently one of: move, group, and ungroup.
%
%       Name is the predicate symbol of the predicate implementing the
%       named Action.
%
%       Parameters is a list of Prolog atoms denoting the parameters of
%       the named Action.
%
%       @tbd Used only by group_contacts/3 and ungroup_contacts/3 which
%       should be changed to use register_action/4 instead.
%
register_action(S1,T,N,Ps,S2):-
        within_action_budget(S1)
        ,inspect(plan,P,S1)
        ,debug(plan,'Current plan ~w',[P])
        ,create_action(T,N,Ps,A)
        ,inspect(plan,count,C,S1)
        ,term_value(C,V)
        ,modify(action,set,actionId(V),A,A_)
        ,modify(plan,add,actions(A_),S1,S3)
        ,modify(plan,increment,count,S3,S2)
        ,debug(plan,'Registerd action ~w',[A_]).


%!      register_action(+State1,+Type,+Name,-State2) is det.
%
%       Add an action tot he plan stored in the current State.
%
%       As register_action/5 but extracts the action's parameters from
%       the current state, State1.
%
%       Type is the atomic type of the action. For the Toy Problem
%       that's currently one of: move, group, and ungroup.
%
%       Name is the predicate symbol of the predicate implementing the
%       named Action.
%
%       @tbd Only implemented for move actions, currently. Needs
%       action_params/3 clause for group and ungroup actions.
%
register_action(S1,T,N,S2):-
        inspect(plan,P,S1)
        ,debug(plan,'Current plan ~w',[P])
        ,action_params(T,S1,Ps)
        ,create_action(T,N,Ps,A)
        ,inspect(plan,count,C,S1)
        ,term_value(C,V)
        ,modify(action,set,actionId(V),A,A_)
        ,modify(plan,add,actions(A_),S1,S3)
        ,modify(plan,increment,count,S3,S2)
        ,debug(plan,'Registerd action ~w',[A_]).


%!      action_params(+Type,+State,-Parameters) is det.
%
%       Extract the Parameters of an action according to its Type.
%
%       Type is the atomic type name of an action: move, group or
%       ungroup.
%
%       State is the current state-vector where the action is taken.
%
%       Parameters are the parameters of the action, according to its
%       Type.
%
%       @tbd Currently only implemented for move actions.
%
action_params(move,Ss,[X,Y]):-
        inspect(position,position(X,Y,_Theta),Ss).
% TODO: add group and ungroup actions.


%!      create_action(+Type,+Name,+Parameters,-Action) is det.
%
%       Create a new Action given its name and Parameters.
%
%       Type is the atomic type of the action. For the Toy Problem
%       that's currently one of: move, group, and ungroup.
%
%       Name is the predicate symbol of the predicate implementing the
%       named Action.
%
%       Parameters is a list of Prolog atoms denoting the parameters of
%       the named Action.
%
create_action(T,N,Ps,A):-
        atomise_params(Ps,Ps_)
        ,I = state_vector_term_init(action,[actionId:(-1)
                                           ,actionType:T
                                           ,actionName:N
                                           ,actionParams:Ps_])
        ,initialised_term(I,A).


%!      atomise_params(+Parameters,-Atomised) is det.
%
%       Convert between a list of parameter terms and a list of atoms.
%
%       Used to convert arbitrarily-typed action parameters to strings
%       for use in create_action/3.
%
atomise_params(Ps,Ps_):-
        findall(P_
               ,(member(P,Ps)
                ,term_to_atom(P,P_)
                )
               ,Ps_).
