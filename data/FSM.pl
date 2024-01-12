fsm(State,[],[],State)
fsm(State1,[X|Tx],[Y|Tx],State3):-
    event()

fsm(State,State).
fsm(State1,State3):-
    event(State1,State2),
    fsm(State2,State3)

parse(S):-parse(q0,S,[]).
parse(Q,[],[]):-acceptor(Q).
parse(Q,[H|T],Y):-
    delta(Q,H,P),
    parse(P,T,Y).

