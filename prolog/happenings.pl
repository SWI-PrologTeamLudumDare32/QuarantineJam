:- module(happenings, [random_happenings/2]).
/** <Module> happenings - random bits of stuff reported in letters
 *
 */

:- use_module(game, [current_date/1, has/3]).
:- use_module(util).
:- use_module(library(chr)).

:- chr_constraint  said/2, reset_said/1, not_said/2.

random_happenings(Who, Happenings) :-
  b_getval(session, S),
  random_between(1,2, N),
  findall(X, (happening(Who, X), not_said(S, X)), OrdAllPossible),
  (   OrdAllPossible = []
  ->
      reset_said(S),
      random_happenings(Who, Happenings)
  ;
      random_permutation(OrdAllPossible, AllPossible),
      take(N, AllPossible, Happenings),
      maplist(said(S), Happenings)
  ).

% TODO this isn't ideal, as it's really only the 'sent' letter that
% should be 'said', but that's kind of a mess to implement.
% and I'm not even sure it's better gameplay

reset_said(S) \ said(S, _) <=> true.
reset_said(_) <=> true.

said(S, Str) \ not_said(S, Str) <=> fail.
not_said(_, _) <=> true.

:- discontiguous happening/2, happening/4.

happening(Who, What) :-
  happening(Who, What, L, U),
  integer(L),
  integer(U),
  current_date(between(L, U)).
happening(Who, What) :-
  happening(Who, What, C, N),
  memberchk(C, [ < , > , =< , >= , =:= , \= ]),
  integer(N),
  current_date(mo(T)),
  call(C, T, N).

happening(annette, "I could think this was late February. Time to begin seeing jonquils coming up.  Unfortunately, the birds appear to have similar misperception of where we are in the year's progression.", 2,3).
happening(annette, "This morning, the birds singing sounded more like spring than winter.", 10, 3).
happening(annette, "Soon, we will have to begin the enormous task of pruning the grapevines.", 3,4).
happening(annette, "A coyote got into the chickens. Killed the rooster, we'll have to get another one.").
happening(annette, "Hit a big rock with the tractor, and we may have to buy a wheel. Big expense.", 4, 10).
happening(annette, "Big sale at the church. We\'ll haul some of our rusted junk there to sell.").
happening(annette, "I try not to read the newspapers or look at the internet.").
happening(annette, "Friends gave us 8 ducks. I\'ve got them set up in a a tank with water, food, and a heat lamp.").
happening(annette, "Spent the last two days on TTB paperwork. What a headache.").
happening(annette, "Bill and Ethyl came by. Bill brought his banjo, Tom and he had fun playing tunes.").
happening(annette, "Found fruit flies around some garbage near the wine shed. Hope that\'s not a sign they're in the wine bins.").
happening(annette, "Tom shot a buck. Big mess dragging it back, but we\'re happily stocking the larder.", 8, 11).
happening(annette, "Deer in our vineyard. One trellis is pretty well wiped out. What they didn\'t eat, they knocked over.", 4, 9).
happening(annette, "The turkeys are doing a good job keeping down the grasshoppers.", 5, 10).
happening(annette, "Sorry for not writing, we\'ve been working til we drop to get the harvest in.", 8,9).
happening(annette, "Tom scraped enough snow that we can get in and out.", 12, 2).
happening(annette, "Saw a gopher hole in the vegetable garden. Poked in dryer sheets.", 3,11).
happening(annette, "The vineyard\'s far from the house. Made some wind chimes from old pipe, the sound keeps various critters away.").
happening(annette, "Been a mild winter so far.", 11, 3).
happening(annette, "Hoping for rain.", 4, 9).
happening(annette, "Pump for the house has died. Another expense. Sigh.").
happening(annette, "Neighbor\s cow in the field. We\'re not happy.", 3, 11).
happening(annette, "Going to build some shelves in the wine shed.", 10, 6).


happening(priscilla, "I\'m clueless about this stuff. Please help us.", < , 12).
happening(priscilla, "Sometimes I think it\'d be better to give up and move back to town.") :-
  current_date(turn(N)),
  N > 6,
  N < 24.
happening(priscilla, "I'm sure sick of living in a trailer and eating canned beans") :-
     has(house, < , 1),
     has(trailer, > , 0).
happening(priscilla,
          ["I can\'t believe we were living in a tent. I told George it\'s move back to town or get a divorce",
           "He wanted to soldier on, but finally agreed. We had a good cry. ",
           "We're in a small apartment in town. George is interviewing, I've gotten a job at Nixon Elementary.",
           "(The game is over - click restart to play again)"]) :-
     has(house, < , 1),
     has(trailer, < , 1).
happening(priscilla,
          "Do I have to feed the chickens something? They seem sickly."). % TODO make this conditional
