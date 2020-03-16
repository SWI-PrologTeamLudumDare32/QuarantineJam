:- module(happenings, [random_happenings/2]).
/** <Module> happenings - random bits of stuff reported in letters
 *
 */

:- use_module(game, [current_date/1]).

random_happenings(Who, Happenings) :-
  random_between(1,3, N),
  length(LHappenings, N),
  setof(X, happening(Who, X), OrdAllPossible),
  random_permutation(OrdAllPossible, AllPossible),
  (   append(LHappenings, _, AllPossible),
      Happenings = LHappenings
  ;   Happenings = AllPossible
  ).

happening(annette, "I could think this was late February. Time to begin seeing jonquils coming up.  Unfortunately, the birds appear to have similar misperception of where we are in the year's progression.") :- current_date(between(2,3)).
happening(annette, "This morning, the birds singing sounded more like spring than winter.") :-
  current_date(between(10, 3)).
happening(annette, "Soon, we will have to begin the enormous task of pruning the grapevines.") :-
  current_date(between(3,4)).
happening(annette, "A coyote got into the chickens. Killed the rooster, we'll have to get another one.").
happening(annette, "Hit a big rock with the tractor, and we may have to buy a wheel. Big expense.") :-
  current_date(between(4, 10)).
happening(annette, "Big sale at the church. We\'ll haul some of our rusted junk there to sell.").
happening(annette, "I try not to read the newspapers or look at the internet.").
happening(annette, "Friends gave us 8 ducks. I\'ve got them set up in a a tank with water, food, and a heat lamp.").
happening(annette, "Spent the last two days on TTB paperwork. What a headache.").
happening(annette, "Bill and Ethyl came by. Bill brought his banjo, Tom and he had fun playing tunes.").
happening(annette, "Found fruit flies around some garbage near the wine shed. Hope that\'s not a sign they're in the wine bins.").
happening(annette, "Tom shot a buck. Big mess dragging it back, but we\'re happily stocking the larder.") :-
  current_date(between(8, 11)).
happening(annette, "Had some deer get into our vineyard. One trellis is pretty well wiped out. What they didn\'t eat, they knocked over.") :- current_date(between(4, 9)).
happening(annette, "The turkeys are doing a good job keeping down the grasshoppers.") :- current_date(between(5, 10)).
happening(annette, "Sorry for not writing, we\'ve been working til we drop to get the harvest in.") :- current_date(between(8,9)).
happening(annette, "Tom scraped enough snow that we can get in and out.") :- current_date(between(12, 2)).

