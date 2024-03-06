:-module(sim_parser, [episode_initialisation/2
                     ,episode_actions/2
                     ,parse_episode/2
                     ]).

:-use_module(model_src(state_vector)).
:-use_module(model_root(specification)).

% Input to grammar rules will be handled as character code lists.
:-user:set_prolog_flag(back_quotes, codes).
% When tracing, lists of character codes will be displayed as atoms.
:- portray_text(true).

/** <module> A parser for survey-simulation log files.

*/


%!      episode_actions(+Parsed,-Actions) is det.
%
%       Create Position etc. terms from the actions in a Parsed episode.
%
%       Parsed is a list of lines from a survery simulation episode
%       ACTIONS log file, as returned by parse_episode/2.
%
%       As is a list of position, or group, terms in the order in which
%       the relevant actions were taken.
%
%       This predicate is meant to be used to extract position and group
%       terms to compose examples for Louise to learn to take move,
%       group and ungroup actions.
%
%       Example call:
%       ==
%       ?- toy_problem:actions(_F), parse_episode(_F,Ps), episode_actions(Ps,As).
%       Ps = [action(0, move, [0.0, 0.0]), action(1, move, [205.8, 74.9]), action(2, end)],
%       As = [position(x(0.0),y(0.0), theta(0.0)), position(x(210.0), y(70.0), theta(0.0))].
%       ==
%
%       @tbd Group and ungroup actions are currently not implemented.
%
episode_actions(Ps,As):-
        findall(Init
               ,(member(action(_Id,N,Par),Ps)
                ,(   N == move
                 ->  Par = [X,Y]
                    ,maplist(clip_num,[X,Y],[X_,Y_])
                    ,I = state_vector_term_init(position
                                               ,[x:X_,y:Y_,theta:0.0])
                    ,initialised_term(I,Init)
                 ;   N == group
                     ,length(Par,C)
                 ->  I = state_vector_term_init(group,[groupId:0,contacts:Par,count:C])
                    ,initialised_term(I,Init)
                 %;   N == ungroup
                 %->  A =.. [ungroup|Par]
                 ;   fail
                 )
                )
               ,As).


%!      clip_num(+Float,-Clipped) is det.
%
%       Round a Float to remove decimals above 0.
%
clip_num(N,N):-
        X is N / 10
        ,0.0 is float_integer_part(X)
        ,!.
clip_num(N,N_):-
        X is N / 10
        ,Y is round(X)
        ,N_ is Y * 10 * 1.0.



%!      episode_initialisation(+Parsed,-Initialised) is det.
%
%       Create vector initialisation terms from a Parsed episode.
%
%       Parsed is a list of lines from a survery simulation episode
%       META log file, as returned by parse_episode/2.
%
%       Initialised is a state vector, initialised to the values in the
%       log.
%
episode_initialisation(Ps,Is):-
        vector_specification(Ss)
        ,findall(N:V
               ,member(term_arg(scan_params,N:V),Ps)
               ,NVs)
        ,S = state_vector_term_init(scan_params,NVs)
        ,findall(I
                ,(member(state_vector_term(T,Args),Ss)
                 ,(   memberchk(state_vector_term_init(T,Args_),[S|Ps])
                  ->  initialised_term(state_vector_term_init(T,Args_), I)
                  ;   \+ list_type(T)
                  ->  initialised_term(state_vector_term(T,Args),I)
                  )
                 )
                ,Is).


%!      parse_episode(+Filename,-Lines) is det.
%
%       Parse all Lines from a survery simulation episode log file.
%
%       Filename is the file name of the file to read. Currently, only
%       "Actions" and "Meta" files can be parsed.
%
%       Lines is the lines in the given file, parsed into Prolog terms.
%
parse_episode(F,Ps):-
        read_lines(F,Ls)
        ,parse_lines(Ls,Ps).


%!      read_lines(+File, -Lines) is det.
%
%       Read lines from a File until the end_of_file marker.
%
read_lines(F,Ls):-
        O = (expand_file_search_path(F,F_)
            ,open(F_,read,S,[alias(input_file)
                      ,close_on_abort(true)
                      ])
            )
        ,R = read_lines(S,[],Ls)
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).

%!      read_lines(+Stream,+Acc,-Lines) is det.
%
%       Business end of read_lines/2.
%
read_lines(S,Acc,Bind):-
        read_line_to_codes(S,Cs)
        ,is_list(Cs)
        ,!
        % For developing only
        ,atom_codes(A,Cs)
        ,read_lines(S,[A|Acc],Bind).
read_lines(S,Acc,Ls):-
        read_line_to_codes(S,end_of_file)
        ,reverse(Acc,Ls).


%!      parse_lines(+Lines, -Parsed) is det.
%
%       Parse Lines from a file to log info terms.
%
parse_lines(Ls, Ps):-
        parse_lines(Ls, [], Ps).

%!      parse_lines(+Lines, +Acc, -Parsed) is det.
%
%       Business end of parse_lines/2.
%
parse_lines([], Acc, Ls):-
        reverse(Acc, Ls)
        ,!.
parse_lines([L|Ls], Acc, Bind):-
        atom_codes(L, Cs)
        ,phrase(log_info(S), Cs)
        ,!
        ,parse_lines(Ls,[S|Acc],Bind).
parse_lines([_L|Ls], Acc, Bind):-
        parse_lines(Ls, Acc, Bind).


% Action file
log_info(action(I,end)) --> actionId(I), sp, end_action, !.
log_info(action(I,N,Ps)) --> actionId(I), sp, actionName(N), sp, actionParams(Ps).
% Meta file
% Scan area term
log_info(state_vector_term_init(scan_area,[x_min:X1,x_max:X2,y_min:Y1,y_max:Y2])) --> scan_area_limits(X1,X2,Y1,Y2), !.
% Map area term
log_info(state_vector_term_init(map_area,[x_min:X1,x_max:X2,y_min:Y1,y_max:Y2])) --> map_area_limits(X1,X2,Y1,Y2), !.
% Position term
%log_info(state_vector_term_init(position,[x:X,y:Y,theta:0.0])) --> agent_start(X,Y), !.
log_info(state_vector_term_init(position,[x:X_,y:Y_,theta:0.0])) -->
        agent_start(X,Y)
        ,!
        ,{ maplist(clip_num,[X,Y],[X_,Y_]) }.
% Scan params term
log_info(term_arg(scan_params,scan_width:W)) --> scan_width(W), !.
log_info(term_arg(scan_params,leadinleadout:L)) --> leadinleadout(L), !.
log_info(term_arg(scan_params,min_scan_length:M)) --> min_scan_l(M), !.
log_info(term_arg(scan_params,nadir_width:N)) --> nadir_width(N), !.
log_info(term_arg(scan_params,boat_speed:P)) --> agent_speed(P), !.
log_info(term_arg(scan_params,time_limit:L)) --> time_limit(L), !.
log_info(term_arg(scan_params,min_scan_angle_diff:F)) --> min_scan_angle_diff(F), !.
% Stride value
log_info(configuration(stride,G)) --> grid_res(G), !.


% Actions
%
actionId(Id) --> integer(Id).

end_action --> `end`, sp.

actionName(move) -->  `move`.
actionName(group) -->  `group`.
actionName(ungroup) -->  `ungroup`.

actionParams([P|Ps]) --> actionParam(P), sp, !, actionParams(Ps).
actionParams([]) --> [].

actionParam(F) --> float(F).
actionParam(I) --> integer(I).


% Meta
%
agent_start(X,Y) --> `agent_start`, colon, sp, lp, comma_nums([X,Y]), rp, !.

comma_nums([N|Ns]) -->
        nums(Ds)
        ,comma
        ,sp
        ,{ atom_codes(A,Ds), atom_number(A,N) }
        ,comma_nums(Ns).
comma_nums([N]) --> nums(Ds),{ atom_codes(A,Ds), atom_number(A,N) }.

scan_width(W) --> `scan_width`, colon, sp, float(W).

leadinleadout(L) --> `leadinleadout`, colon, sp, float(L).

min_scan_l(M) --> `min_scan_l`, colon, sp, float(M).

nadir_width(W) --> `nadir_width`, colon, sp, float(W).

agent_speed(S) --> `agent_speed`, colon, sp, float(S).


scan_area_limits(X_Min,X_Max,Y_Min,Y_Max) -->
        `scan_area_lims`
        ,colon
        ,sp
        ,lp
        ,comma_nums([X_Min,X_Max,Y_Min,Y_Max])
        ,rp.

map_area_limits(X_Min,X_Max,Y_Min,Y_Max) -->
        `map_area_lims`
        ,colon
        ,sp
        ,lp
        ,comma_nums([X_Min,X_Max,Y_Min,Y_Max])
        ,rp.

time_limit(L) --> `time_lim`, colon, sp, float(L).

min_scan_angle_diff(D) --> `min_scan_angle_diff`, colon, sp, integer(D).

grid_res(G) --> `grid_res`, colon, sp, integer(G).


% Auxiliaries
%
float(F_) --> nums(Ds), { atom_codes(F,Ds), atom_number(F,F_) }.

integer(I_) --> nums(Is), { atom_codes(I,Is), atom_number(I,I_) }, !.

nums([N,.|Ns]) --> num(N), dot, nums(Ns).
nums([N|Ns]) --> num(N), nums(Ns).
nums([S,N|Ns]) --> num_sign(S), num(N), nums(Ns).
nums([N]) --> num(N).
nums([S,N]) --> num_sign(S), num(N).

num(N) -->  [A], { between(0'0,0'9,A), atom_codes(N,[A]) }.

dot --> `.`.

comma --> `,`.

colon --> `:`.

lsb --> `[`.
rsb --> `]`.

lp --> `(`.
rp --> `)`.

num_sign(-) --> `-`.
% 0 is the ASCCI code for "blank".
%num_sign('\u0000') --> [].

% Mainly used to skip whitespace.
sp --> ` `, !.
sp --> `\t`, !.
sp --> `\n`, !.
