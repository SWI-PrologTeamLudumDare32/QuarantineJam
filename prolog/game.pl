:- module(game, [
  get_state/2,
  make_player_inited/1,
  act/2,
  init_global/0,
  current_date/1
]).

:- use_module(library(chr)).
:- use_module(happenings).

:- chr_constraint
  cur_ticks/2,          % session, ticks     the current time in ticks for session s
  get_ticks/2,          % session, Ticks      transient getter of the current ticks
  days_go_by/2,         % session, ticks      increment the current time (and make world go)
  reset_time/1,         % session             reset the current time to start

  chr_reset/1,          % session            transient reset the game state
  inited/1,             % session            exists if this session initialized
  make_player_inited/1, % session            transient idempotic insure session inited

  act/2,                % session, actionname  transient, causes this action to happen
  potential_action/1,    % actionname         same as known_action but as CHR constraint
  get_available_actions/2, % session, ActionName   transient get all actions we can do now
  collect_available_actions/2, % session, Actions  transient, after we've made available_actions collect them
  available_action/2,    % session, actionname     transient this action is available

  thing/3.             % session, Type, Status  an individual object

get_state(S, Response) :-
    b_setval(session, S),
    get_available_actions(S, Actions),
    maplist(annette_letter, Actions, Annette),
% TODO work downwards from here
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
		 *  Things
		 *
		 *  Rules about things.
		 *  A thing is a unique item like a cow
		 *  with a status (sick, ok, etc).
		 *******************************/

% on reset wipe out all the things
chr_reset(S) \ thing(S, _, _) <=> true.
% and add the initial inventory
chr_reset(S) ==>
    thing(S, field, ok),   % don't want to trigger news, etc so add directly
    thing(S, trailer, run_down),
    thing(S, cow, ok),
    thing(S, money, 14000).

% money is fungible
thing(S, money, A), thing(S, money, B) <=>
    NM is A + B,
    thing(S, money, NM).


		 /*******************************
		 *          Actions
		 *
		 *          known or potential - an action somebody
		 *          might do some day
		 *          available - an action the player can do now
		 *
		 *******************************/

:- discontiguous
  action_advice/2,   % Action, Advice   map action atoms to Advice realstrings
  known_action/1.    % Action           this is a potential action

% set semantics for actions
available_action(S, A) \ available_action(S, A) <=> true.

		 /*******************************
		 *      Individual actions
		 *******************************/

% game_state is a special action that's 'known', but never available.
% It's used by FE to just update the display
known_action(game_state).
act(_, game_state) <=> true.

action_advice(buy_cow, "I suggest you buy a cow. You can get a nice milker for around $500.").
known_action(buy_cow).
get_available_actions(S, _),  thing(S, money, M) ==>  M > 500 | available_action(S, buy_cow).
thing(S, money, M), act(S, buy_cow) ==>
         thing(S, cow, ok),
         NM is M - 500,
         NM >= 0,
         thing(S, money, NM),
         days_go_by(S, 1).

known_action(sell_cow).
action_advice(sell_cow, "I suggest you sell your cow. It should fetch about $300.").
get_available_actions(S, _), thing(S, cow, ok) ==> available_action(S, sell_cow).
thing(S, money, M), act(S, sell_cow), thing(S, cow, ok) <=>
     NewM is M + 300,
     thing(S, money, NewM),
     days_go_by(S, 1).

known_action(time_passes).
action_advice(time_passes, "Tom brought me flowers today. He\'s so romantic!").
get_available_actions(S, _) ==> available_action(S, time_passes).
act(S, time_passes) <=> days_go_by(S, 7).


		 /*******************************
		 *        Annettes Letter       *
		 *******************************/

annette_letter(Action, _{action: Action,
                         letter: Letter}) :-
    action_advice(Action, Advice),
    current_date(string(Date)),
    random_happenings(annette, Happenings),
    flatten([Date,
              "Dear Priscilla;",
              Advice,
              Happenings,
              "Sincerely,",
              "Annette"], Letter).

		 /*******************************
		 * Global initialization.
		 *
		 * Things that happen once at startup.
		 *
		 *******************************/
init_global :-
  setof(X, known_action(X), List),
  maplist(potential_action, List).

		 /*******************************
		 * Game Initialization.
		 *
		 * transient make_player_inited idempoticly ensures that
		 * the session is initialized
                 * chr_reset is a transiet that (re)starts the game
                 * chr_reset is handled in many places to init state
		 * ******************************/

inited(S) \ make_player_inited(S) <=> true.
make_player_inited(S) <=>
    chr_reset(S),
    inited(S).

inited(S) \ inited(S) <=> true.

		 /*******************************
		 * Handling time.
		 *
                 * Time in game is ticks, 1 tick is one
		 * day, the epoch is Jan 1 2020
		 * The year is 360 days long , each month is 30 days
		 * ******************************/

%! current_date(Date:term) is semidet
%
% @arg Date - one of:
%  * string(Str)   Str is bound to a real string rep like "Feb 13, 2020"
%  * stringmo(Str) Str is bound to a real string rep like "Feb, 2020"
%  * between(L, U) succeeds if the current date is between 1 based month
%  number L to U inclusive. If the numbers are reversed it includes new
%  year
%
current_date(string(Str)) :-
  b_getval(session, S),
  get_ticks(S, Ticks),
  Yr is Ticks div 360 + 2020,
  Mo is ((Ticks div 30) mod 12) + 1,
  Day is Ticks mod 30,
  nth1(Mo, ["Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"], MoName),
  format(string(Str), '~w ~w, ~w', [MoName, Day, Yr]).
current_date(stringmo(Str)) :-
  b_getval(session, S),
  get_ticks(S, Ticks),
  Yr is Ticks div 360 + 2020,
  Mo is ((Ticks div 30) mod 12) + 1,
  nth1(Mo, ["Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"], MoName),
  format(string(Str), '~w ~w', [MoName, Yr]).
current_date(between(L, U)) :-
  L > U,
  current_date(between(1, U)),
  current_date(between(L, 12)).
current_date(between(L, U)) :-
  L =< U,
  b_getval(session, S),
  get_ticks(S, Ticks),
  Mo is ((Ticks div 30) mod 12) + 1,
  L =< Mo,
  Mo =< U.

% init time
chr_reset(S) ==> reset_time(S).
reset_time(S) \ cur_ticks(S, _) <=> true.
reset_time(S) <=> writeln('reseting the time'),
    cur_ticks(S, 3).  % we start in march of 2020

% retrieve tick count
cur_ticks(S, T) \ get_ticks(S, Ticks) <=> T = Ticks.

% must be below anything that happens as days_go_by
cur_ticks(S, T), days_go_by(S, N) <=>
            NewT is T + N,
            cur_ticks(S, NewT).

		 /*******************************
		 *  CHR cleanups that need to be at bottom of file
		 *******************************/

chr_reset(_) <=> true.

% end of the distributed get_foo pattern for actions
get_available_actions(S, A) <=> collect_available_actions(S, A).
available_action(S, A), collect_available_actions(S, L) <=>
     L = [A|L1],
     collect_available_actions(S, L1).
collect_available_actions(_, L) <=> L = [].

