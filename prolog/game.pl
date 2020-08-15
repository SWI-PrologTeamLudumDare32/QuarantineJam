:- module(game, [
  get_state/2,
  make_player_inited/1,
  act/2,
  init_global/0,
  current_date/1,
  has/3
]).
/** <Module>
 *
 * And it is holy writ - things which have another status, such
 * as gravid or sick, shall not have a count!
 *
 * So chickens might be fungible, but if you want sick chickens you have
 * to make all of them  sick (presumably with an outside constraint)
 */

:- use_module(library(chr)).
:- use_module(happenings).

:- chr_constraint
  cur_ticks/2,            % session, ticks         the current time in ticks for session s
  get_ticks/2,            % session, Ticks         transient getter of the current ticks
  days_go_by/2,           % session, ticks         increment the current time (and make world go)
  reset_time/1,           % session                reset the current time to start

  chr_reset/1,            % session                transient reset the game state
  inited/1,               % session                exists if this session initialized
  make_player_inited/1,   % session                transient idempotic insure session inited

  act/2,                  % session, actionname    transient, causes this action to happen
  potential_action/1,     % actionname             same as known_action but as CHR constraint
  get_available_actions/2, % session, ActionName   transient get all actions we can do now
  collect_available_actions/2, % session, Actions  transient, after we've made available_actions collect them
  available_action/2,     % session, actionname     transient this action is available
  acty_done/2,            % session, actyname      simple one time activities. succeeds if this acty done
  acty/2,                 % session, actyname      record we did this acty


  thing/3,               % session, type, status   an individual object
  count_things/3,        % session, type, Count    return the count of things
  set_init_inventory/1,  % session                 transient create the initial inventory
  get_things/2,          % session, List get all the things in format needed by front end

  news/2,                % session, news           the news for this turn
  get_news/2.            % session, News           getter for this turns news

%!  news(+Session:atom, +News:news) is det
%
%   News is either a real string, or the compound pic/1,
%   whose argument is a relative URI to an image for the picture field.
%
get_state(S, Response) :-
    (   get_state_(S, Response)
    ->  true
    ;   gtrace
    ).
get_state_(S, Response) :-
    b_setval(session, S),
    get_available_actions(S, Actions),
    maplist(annette_letter, Actions, Annette),
    priscilla_letter(Priscilla, Pic),
    get_things(S, Inventory),
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
:- discontiguous fe_form/3, known_thing/1.

known_thing(field).
fe_form(field, X, _{ desc: "A fallow field"}) :-
    planted{ crop: fallow} :< X.
known_thing(trailer).
fe_form(trailer, run_down,  _{ desc: "A run down airstream"}).
fe_form(cow, X, _{ desc: "A cow"}) :-
    livestock{ health: ok, gravid: -1} :< X.
fe_form(cow, X, _{ desc: "A sick cow"}) :-
    livestock{ cnt: 1, health: sick, gravid: -1} :< X.
fe_form(cow, X, _{ desc: "A pregnant cow"}) :-
    livestock{ cnt: 1, health: ok, gravid: N} :< X,
    N > 0.
fe_form(money, Amt, _{ desc: Desc }) :-
    format(string(Desc) , 'Bank Balance: $~w', [Amt]).

% and add the initial inventory
set_init_inventory(S) \ thing(S, _, _) <=> true.
set_init_inventory(S) <=>
    thing(S, field, planted{ crop: fallow}),   % don't want to trigger news, etc so add directly
    thing(S, trailer, run_down),
    thing(S, cow, livestock{ cnt: 1, health: ok, gravid: -1}),
    thing(S, money, 14000).

has(Type, Op, Count) :-
    b_getval(session, S),
    count_things(S, Type, N),
    call(Op, N, Count).


:- chr_constraint countable/2, find_countables/2, count/4.

count_things(S, Type, N) ==> find_countables(S, Type), count(S, Type, 0, N).

find_countables(S, Type), thing(S, Type, _) ==> countable(S, Type).
find_countables(_, _) <=> true.

count(S, Type, SoFar, N), countable(S, Type) <=>
                    succ(SoFar, NSF),
                    count(S, Type, NSF, N).
count(_, _, SoFar, N) <=> SoFar = N.

% money is fungible
thing(S, money, A), thing(S, money, B) <=>
    NM is A + B,
    thing(S, money, NM).


cow_names(['Daisy', 'Maisy', 'Annabelle', 'Clarabelle',
                        'Gertrude', 'Candie', 'Minnie', 'Flower',
                        'Dahlia', 'Margie', 'Margo', 'Rose', 'Bella',
                        'Darla', 'Meg', 'Shelly', 'Molly', 'Moon',
                        'Annabelle', 'Bell', 'Ginger', 'Bovina',
                        'Baby', 'Teacup', 'Annie', 'Pinky',
                        'Emma', 'Sunshine', 'Betty Sue', 'Muffin',
                        'Penelope', 'Penny', 'Cocoa', 'Princess',
                       'Bertha', 'Dorothy']).
cow_breeds(['Abondance', 'Devon', 'White Park', 'Belgian Blue',
                          'Angus', 'Angus', 'Angus',
                          'Holstein', 'Holstein', 'Holstein', 'Holstein',
                          'Hereford', 'Hereford', 'Hereford',
                          'Guernsey', 'Guernsey', 'Guernsey', 'Guernsey',
                          'Jersey', 'Jersey', 'Jersey',
                          'Swedish'
                         ]).

:- chr_constraint collect_things/2, a_thing/3.
get_things(S, _), thing(S, Type, Status) ==> a_thing(S, Type, Status).
get_things(S, Things) <=> collect_things(S, Things).

collect_things(S, Things), a_thing(S, Type, Status) <=>
                 Things = [ThisThing | Tail],
                 fe_form(Type, Status, ThisThing),
                 collect_things(S, Tail).
collect_things(_, Things) <=> Things = [].



		 /*******************************
		 *          Actions
		 *
		 *          known or potential - an action somebody
		 *          might do some day
		 *          available - an action the player can do now
		 *
		 *******************************/

% simpl one-time activity.
% an activity is something we want to remember having happened
% this could be a game mechanics activity like sending the start letter
% or moving back to town, or something priscillas done at the winery
acty(S, A) \ acty_done(S, A) <=> true.
acty_done(_, _) <=> fail.

:- discontiguous
  action_advice/2,   % Action, Advice   map action atoms to Advice realstrings
  known_action/1.    % Action           this is a potential actionPhotos

% set semantics for actions
available_action(S, A) \ available_action(S, A) <=> true.



		 /*******************************
		 *      Individual actions
		 *******************************/

% game_state is a special action that's 'known', but never available.
% It's used by FE to just update the display
known_action(game_state).
act(_, game_state) <=> true.

:- discontiguous  buy_price/2, buy_advice/2, buy_news/2,
                  sell_thing/2, sell_price/2, sell_advice/2.

buy_thing(buy_cow, cow).
buy_price(buy_cow, 500).
buy_advice(buy_cow, "I suggest you buy a cow. You can get a nice milker for around $500.").
buy_news(buy_cow, [News, pic('/static/img/pix/cow.jpg')]) :-
    cow_names(Names),
    random_member(Cow, Names),
    cow_breeds(Breeds),
    random_member(Breed, Breeds),
    format(string(News),
           'We bought a nice ~w. I\'m going to name her ~w', [Breed, Cow]).
action_advice(buy_cow, "I suggest you buy a cow. You can get a nice milker for around $500.").

known_action(buy_cow).
get_available_actions(S, _),  thing(S, money, M) ==>  M > 500 | available_action(S, buy_cow).
thing(S, money, M), act(S, buy_cow) <=>
         thing(S, cow, livestock{ health: ok, gravid: -1}),
         NM is M - 500,  % mustnt fail here
         thing(S, money, NM),
         buy_news(buy_cow, News),
         news(S, News),
         days_go_by(S, 1).

sell_thing(sell_cow, cow).
sell_price(sell_cow, 300).
sell_advice(sell_cow, "I suggest you sell your cow. It should fetch about $300.").
sell_news(sell_cow, "Sold a cow.").

known_action(sell_cow).
action_advice(sell_cow, "I suggest you sell your cow. It should fetch about $300.").
get_available_actions(S, _), thing(S, cow, Status) ==>
     livestock{ health: ok} :< Status |
     available_action(S, sell_cow).
thing(S, money, M),  act(S, sell_cow), thing(S, cow, Status) <=>
% care about status to make sure we sell the ok one, not the sick one,
% if we have 2 cows
     livestock{health: ok } :< Status,
     NewM is M + 300,
     thing(S, money, NewM),
     sell_news(sell_cow, News),
     news(S, News),
     days_go_by(S, 1).

known_action(time_passes).
action_advice(time_passes, Adv) :-
     random_member(Adv, [
                     "Tom brought me flowers today. He\'s so romantic!",
                     "I so love our lovely home here.",
                     "You can use butter if you don\'t have cold cream.",
                     "Maybe I should try keeping bees",
                     "There\'s nothing as good as eating things you\'ve grown yourself",
                     "Tom bought a water wheel from an estate sale. Another project.",
                     "Made rabbit stew last night. Really tasty with potatos from the garden."
                   ]).
get_available_actions(S, _) ==> available_action(S, time_passes).
act(S, time_passes) <=> days_go_by(S, 7).

		 /*******************************
		 *          News - things that
		 *  happened at Priscillas,
		 *  that are 'real'
		 *
		 *******************************/

% Get the news destructively
% (removes the news items as it collects them)
% TODO go back to short form
get_news(S, L), news(S, Item)  <=>
      L = [Item | Tail],
      get_news(S, Tail).
get_news(_, L) <=> L = [].

chr_reset(S) \ news(S, _) <=> true.


		 /*******************************
		 *        Letters               *
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



priscilla_letter(Priscilla, '/static/img/pix/wannabe.jpg') :-
    b_getval(session, S),
    \+ acty_done(S, start_letter_sent),
    !,
    acty(S, start_letter_sent),
    Priscilla = [
        "March 2020",
        "We\'ve done it! We closed on Wannabe Winery on wednesday. For now we\re living in the trailer.",
        "Eeek, we\re so new to all this! What do you think we should do first?",
        "love",
        "Priscilla"].
priscilla_letter(Priscilla, Pic) :-
    b_getval(session, S),
    acty_done(S, start_letter_sent),
    current_date(stringmo(Date)),
    random_happenings(priscilla, Happenings),
    gnap(S, News, Pic),
    flatten([
        Date,
        "Hey Annette",
        News,
        Happenings,
        "love",
        "Priscilla"], Priscilla).

gnap(S, News, Pic) :-
    get_news(S, NewsAndPix),
    flatten(NewsAndPix, FlatNewsAndPix),
    partition([X]>>(X = pic(_)), FlatNewsAndPix, Pix, News),
    (   random_member(pic(Pic), Pix)
    ;   random_member(Pic, [
               '/static/img/pix/truck.jpg',
               '/static/img/pix/turkeys.jpg',
               '/static/img/pix/tractor.jpg'])
    ).



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
%  * mo(N) N is bound to the number of months since jan 1 2020 on exit
%  (note this starts at 2)
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
current_date(mo(N)) :-
    b_getval(session, S),
    get_ticks(S, T),
    N = T div 30.

% init time
chr_reset(S) ==> reset_time(S).
reset_time(S) \ cur_ticks(S, _) <=> true.
reset_time(S) <=> cur_ticks(S, 90).  % we start march 1 of 2020
% on reset wipe out all the things

chr_reset(S) ==> set_init_inventory(S).

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

act(S, A) <=>
      format(string(Str), 'we tried but couldnt ~w', [A]),
      news(S, Str).


% backup if we forgot the fe_form
fe_form(Type, Status, Form) :-
    format(string(Form), 'A ~w status ~q', [Type, Status]).
