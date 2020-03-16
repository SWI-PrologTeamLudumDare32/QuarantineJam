:- module(server, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(chr)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(game).

go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    create_chr_thread,
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- http_handler('/', http_reply_file('../web/html/index.html', []), []).

:- http_handler('/static/html/', http_reply_from_files('../web/html/', []), [prefix]).

:- http_handler('/static/svg/', http_reply_from_files('../web/svg/', []), [prefix]).

:- http_handler('/static/img/', http_reply_from_files('../web/img/', []), [prefix]).

:- http_handler('/static/js/', http_reply_from_files('../web/js/', []), [prefix]).

:- http_handler('/static/font/', http_reply_from_files('../web/fonts/', []), [prefix]).

:- http_handler('/static/image/', http_reply_from_files('../web/image/', []), [prefix]).

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

stub_mode.

new_state(S, Payload) :-
    \+ stub_mode,
    make_player_inited(S),
    _{ action: Action }  :< Payload,
    (
        Action = get_state -> true;
        game:known_action(Action) -> game:act(S, Action);
        game:error(S, "invalid_action", [])
    ).
new_state(_, _) :-
    stub_mode.

get_chr_response_dict(S, Response) :-
    \+ stub_mode,
    game:get_state(S, Response).
get_chr_response_dict(_, Response) :-
    stub_mode,
    random_permutation([
                       _{ action: buy_cow,
                          letter:
                          ["Dear Priscilla;",
                           "I suggest you buy a cow for $500",
                           "Sincerely",
                            "Annette"]
                        },
                       _{ action: sell_cow,
                          letter:
                          ["Dear Priscilla;",
                           "I suggest you sell your cow",
                           "It should fetch about $300",
                           "Sincerely",
                            "Annette"]
                        },
                       _{ action: time_pass,
                          letter:
                          ["Dear Priscilla;",
                           "Tom brought me flowers today.",
                           "He\s so romantic!",
                           "Hope things are going well for you",
                           "Sincerely",
                            "Annette"]
                        }
                   ], Annette),
    random_member(Priscilla,
                  [
                      ["June 2020", "We live in a trailer and eat canned beans",
                    "I\'m clueless about this stuff. Please help us.",
                    "love,",
                    "Priscilla"],
                      ["November 2020",
                       "Thanks for all your help. We were sure lost when we started.",
                       "Do I have to feed the chickens something? They seem sickly.",
                    "love,",
                    "Priscilla"],
                      ["June 2022",
                       "Thanks for all your help. We were sure lost when we started. Remember when I tried to feed the cow Frosted Flakes?",
                       "The goat died.",
                    "love,",
                    "Priscilla"]
                  ]),
    random_permutation([
        _{ item: chickens, cnt:42, status: well },
        _{ item: chickens, cnt:42, status: sick },
        _{ item: trailer, cnt: 1 , status: 'run down'},
        _{ item: field, cnt: 1, status: 'planted in wheat, due to harvest in July'},
        _{ item: 'wine press', cnt: 1, status: ok }], Inv),
    append(Inv, [_{ item: money, cnt: 14000, status: ok}], Inventory),
    random_member(Pic, [
               '/static/img/pix/truck.jpg',
               '/static/img/pix/turkeys.jpg',
               '/static/img/pix/tractor.jpg']),
    Response = _{
                   priscilla: Priscilla,
                   annette: Annette,
                   image: Pic,
                   inventory: Inventory
               }.


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
   thread_create((init_global, polling_sub), _, [ alias(chr),
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
