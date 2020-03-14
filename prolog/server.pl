:- module(server, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(chr)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).

go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    create_chr_thread,
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- http_handler('/static/', http_reply_from_files('../web/html/', []), [prefix]).

:- chr_constraint
    chr_reset/1,
    thing/2,
    make_player_inited/1,
    inited/1,
    collect_things/2,
    thing_to_collect/2,
    all_things/2,
    remove_thing/2,
    error/3,
    collect_errors/3.


:- http_handler('/game_turn', game_turn , []).

game_turn(Request) :-
    http_in_session(S),
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    do_in_chr_thread(new_state(S, Payload),
                     get_chr_response_dict(S, Response)),
    format('Access-Control-Allow-Origin: *~n'),
    reply_json_dict(Response).

		 /*******************************
		 *   Overall Game Logic          *
		 *******************************/

new_state(S, Payload) :-
    make_player_inited(S),
    _{  add: List,
        remove: RemoveList
     }  :< Payload,
    maplist(remove_thing(S), RemoveList),
    maplist(add_thing(S), List).

get_chr_response_dict(S, Response) :-
    collect_things(S, Results),
    collect_errors(S, "",  ErrStr),
    Response = _{
                   result: Results,
                   error: ErrStr
               }.

collect_things(S, _), thing(S, Name) ==> thing_to_collect(S, Name).
collect_things(S, L) <=> all_things(S, L).

thing_to_collect(S, Name), all_things(S, L) <=>
         L = [Name |L1],
         all_things(S, L1).
all_things(_, L) <=> L = [].

collect_errors(S, SoFar, Ret), error(S, Fmt, Vars) <=>
         format(string(Str), Fmt, Vars),
         string_concat(SoFar, Str, NewSoFar),
         collect_errors(S, NewSoFar, Ret).
collect_errors(_, SoFar, Ret) <=> SoFar = Ret.

% reset to the start of game state. Not same as make_player_inited
% which establishes initial conditions when session first seen
%
chr_reset(S) \ thing(S, _) <=> true.
chr_reset(S) <=>
    thing(S, salt),
    thing(S, water).

inited(S) \ make_player_inited(S) <=> true.
make_player_inited(S) <=>
    chr_reset(S),
    inited(S).

inited(S) \ inited(S) <=> true.

% prolog so we're not subject to CHR injection attacks
add_thing(S, Name) :-
    thing(S, Name).

remove_thing(S, Name), thing(S, Name) <=> true.
remove_thing(S, Name) <=> error(S, 'we\'d love to sell the ~w but we don\'t have one', [Name]).


		 /*******************************
		 *              Game Logic      *
		 *******************************/

    Response = _{
thing(S, salt), thing(S, water) <=> thing(S, salt_water).

		 /*******************************
		 * Debug help                   *
		 *******************************/
debug_constraints(Where) :-
    find_chr_constraint(X),
    debug(constraint(Where), '~w', [X]),
    fail.
debug_constraints(_).


		 /*******************************
		 *  Thread Component            *
		 *******************************/

create_chr_thread :-
   message_queue_create(_, [ alias(sub) ]),
   message_queue_create(_, [ alias(par) ]),
   thread_create(polling_sub, _, [ alias(chr),
           at_exit(debug(lines, 'CHR thread exited', []))]).

polling_sub :-
   % listen for new message on `sub` queue
   thread_get_message(sub, sync(ActionCHR, ResultCHR)),
   debug_constraints(polling_sub),
   % do the actual constraint call
   (   call(ActionCHR)
   ;
       debug(constraint(polling_sub),
             'action constraint ~w failed unexpectedly~n',
             [ActionCHR])
   ),
   debug_constraints(polling_sub),
   % get the result using the get_foo pattern
   ResultCHR =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(CallMe)
   ;
       debug(constraint(polling_sub),
             'result constraint ~w failed unexpectedly~n',
             [ResultCHR])
   ),
   !, % nondet calls not allowed
   % send it back to the `par` message queue
   thread_send_message(par, Result),
   % repeat
   polling_sub.

%!  do_in_chr_thread(+ActionCHR:chr_constraint,
%!         +ResultCHR:chr_constraint) is det
%
%   queries ActionCHR in the chr thread, which must be
%   grounded chr_constraint or prolog predicate,
%   then calls ResultCHR, whose last argument must be unbound.
%   the last argument will be bound as if a direct chr call
%   was made.
%
% eg to touch the egg to the pan and then get the egg's costume do
% do_in_chr_thread(touch(S, egg, pan), get_costume(S, egg, Costume))
%
% Note that these are effectively called in once/1
%
do_in_chr_thread(ActionCHR, ResultCHR) :-
   ResultCHR =.. List,
   append(_, [Result], List),
   thread_send_message(sub, sync(ActionCHR, ResultCHR)),
   thread_get_message(par, Result).

:- debug(constraint(_)).
:- debug(lines).
