:- module(game, [
    collect_env/2,
    collect_things/2,
    collect_available_actions/2,
    collect_errors/3,
    make_player_inited/1,
    known_action/1,
    error/3,
    act/2
  ]).

:- use_module(library(chr)).

:- chr_constraint
  chr_reset/1,
  thing/2,
  make_player_inited/1,
  inited/1,

  act/2,
  available_action/2,

  collect_things/2,
  thing_to_collect/2,
  all_things/2,

  collect_available_actions/2,
  available_action_to_collect/2,
  all_available_actions/2,

  collect_env/2,
  env_to_collect/3,
  all_env/2,

  env/3,
  time_passing/1,
  time_event/2,
  error/3,
  collect_errors/3.

%
% Player Actions
%
known_action(buy_cow).
known_action(end_turn).

act(S, buy_cow), thing(S, money) <=> thing(S, cow).
act(S, buy_cow) <=> error(S, 'we\'d love to sell a cow but we don\'t have one', []).

%
% End turn game logic
%
act(S, end_turn) <=> time_passing(S).

time_passing(S) ==> time_event(S, tick).

time_event(S, tick), env(S, time, T0) <=> T is T0 + 1, env(S, time, T).


% reset to the start of game state. Not same as make_player_inited
% which establishes initial conditions when session first seen
%
chr_reset(S) \ thing(S, _) <=> true.
chr_reset(S) <=>
    available_action(S, buy_cow),
    available_action(S, end_turn),
    env(S, time, 1),
    thing(S, field),
    thing(S, house),
    thing(S, money),
    thing(S, money),
    thing(S, money),
    thing(S, money),
    thing(S, money),
    thing(S, money),
    thing(S, money).

%
%  Data manipulation
%
collect_things(S, _), thing(S, Name) ==> thing_to_collect(S, Name).
collect_things(S, L) <=> all_things(S, L).

thing_to_collect(S, Name), all_things(S, L) <=>
         L = [Name |L1],
         all_things(S, L1).
all_things(_, L) <=> L = [].


collect_available_actions(S, _), available_action(S, Name) ==> available_action_to_collect(S, Name).
collect_available_actions(S, L) <=> all_available_actions(S, L).

available_action_to_collect(S, Name), all_available_actions(S, L) <=>
         L = [Name |L1],
         all_available_actions(S, L1).
all_available_actions(_, L) <=> L = [].


collect_env(S, _), env(S, Key, Value) ==> env_to_collect(S, Key, Value).
collect_env(S, L) <=> all_env(S, L).

env_to_collect(S, Key, Value), all_env(S, L) <=>
         L = [[Key,Value] |L1],
         all_env(S, L1).
all_env(_, L) <=> L = [].

collect_errors(S, SoFar, Ret), error(S, Fmt, Vars) <=>
         format(string(Str), Fmt, Vars),
         string_concat(SoFar, Str, NewSoFar),
         collect_errors(S, NewSoFar, Ret).
collect_errors(_, SoFar, Ret) <=> SoFar = Ret.


inited(S) \ make_player_inited(S) <=> true.
make_player_inited(S) <=>
    game:chr_reset(S),
    inited(S).

inited(S) \ inited(S) <=> true.
