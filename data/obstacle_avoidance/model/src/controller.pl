:-module(controller, [oda/4
                     ,oda/3
                     ]).

:-use_module(model_src(actions)).


/** <module> Finite State Controller for Toy Problem.

*/

%!	oda(+Goals,+State,-End) is nondet.
%
%	Goal-based Observe-Decide-Act loop.
%
%       Similar to oda/4, but the first argument is a list of _goals_ to
%       be completed by the agent, not a list of actions, as in a plan.
%       This loop doesn't handle actions directly and only passes goals
%       to the agent, where the agent is basically a set of logic
%       progarms implementing actions. I guess.
%
%       The last clause, the resolution clause, "resolves" goals (which
%       are agent goals and not Horn goals, as in SLD-Resolution) is
%       supposed to be adding a new set of goals to the goal-stack as
%       the result of taking an action, but I don't know how to do that
%       yet: take an action, and update your goals. We'll have to figure
%       that out.
%
%       I think that this version requires the State vector to have a
%       term representing the current goal, which is the one popped from
%       the goal-stack in the first branch of the branching clause, and
%       resolved in the resolution clause. So there's only one goal in
%       the State vector. I think.
%
oda([],Ss,Ss):-
    !.
oda((G,Gs),S1,S3):-
    oda(G,S1,S2)
    ,oda(Gs,S2,S3).
oda((G),S1,S5):-
    observation(S1,S2)
    ,decision(G,S2,G_,S3)
    ,action(G_,S3,Gs,S4)
    ,oda(Gs,S4,S5).



%!	oda(+Plan,+Start,-Current,+Goal) is nondet.
%
%	Observation-decision-action loop for plans.
%
%       Plan is a plan, as a Prolog tree of literals (a term (L1, ...,
%       Ln).
%
%       Start is a state-vector for a planning domain, given as a list
%       of logical atoms (i.e. state-vector terms) representing elements
%       of the domain. Start represents the (or a) starting state.
%
%       Current is the state vector at the current step of the Plan's
%       execution.
%
%       Goal is the goal state, a state-vector describing the state of
%       the world when the initial goal for which the Plan is a, well,
%       plan, is complete. There may be more than one goal states.
%
%       The motivation for this predicate is that we want to execute
%       each action in a Plan, but allow for interruptions as the state
%       of the world changes potentially invalidating the Plan, or
%       forcing its posptonement.
%
%       In an observation-decision-action loop, the following can
%       happen.
%
%       First, the current state of the world is observed. If the world
%       has changed, the state-vector representing the world's state is
%       updated.
%
%       Second, a decision is made according to the current state of the
%       world, on whether to:
%       a) Make a new plan, and execute its first action, or
%       b) Execute the next action in the current plan
%
%       When the decision is made to create a new plan, the next action
%       in the previous plan may be either discarded (in which case the
%       entire plan is discarded) or deferred. In the latter case, the
%       action is pushed onto the action-stack (in the first argument of
%       oda/4) and then the literals representing the actions of a new
%       plan are pushed on top of it. Thus a deferred action has a
%       chance to be continued later on, if possible and necessary.
%
%       Finally, the action chosen in the decision step is executed in
%       the action step. If the action succeeds, the ODA loop continues
%       with the next cycle. If the action fails, the ODA loop
%       backtracks to the last decision, and chooses a new action. And
%       so on.
%
oda(true,G,G,G):-
    !.
oda(true,S,S,_G):-
    !.
oda((A,As),S1,S3,G):-
    oda(A,S1,S2,G)
    ,oda(As,S2,S3,G).
oda((A),S1,S5,G):-
    observation(S1,S2)
    ,decision(A,S2,A_,S3)
    ,action(A_,S3,As,S4)
    ,oda(As,S4,S5,G).


%!	observation(+Current,+Updated) is det.
%
%	Observe, and update, the state of the world.
%
%	Currently just a stub: returns the input state.
%
%	@tbd This should update the time kept in the plan.
%
observation(S,S).


%!	decision(+Action,+State,-NewAction,-NewState) is det.
%
%	Decide on an action to execute.
%
%	Currently just a stub: returns the input state and action.
%
decision(A,S,A,S).

%!	action(+Action,+State,-Result,-New) is det.
%
%	Execute an action according to a decision.
%
%	Currently just a stub: returns the input state and action.
%
/*
action(A,S1,true,S2):-
        A =.. [_S,_Ps,S1,S2]
        ,call(A).
*/
/*
action(A,S1,true,S2):-
        A =.. [_N,S1,S2]
        ,call(A).
*/
action(A,S1,B,S2):-
        A =.. [_N,S1,S2]
        ,clause(A,B).


