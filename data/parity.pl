% :-module(parity, [
%     background_knowledge/2,
%     metarules/2,
%     positive_example/2,
%     negative_example/2,
%     parity/2
% ]).

even([]).
even([0|T]):- even(T).
even([1|T]):- odd(T).
odd([0|T]):- odd(T).
odd([1|T]):- even(T).