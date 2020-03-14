%
% Sample query:
% worker(farmer,hired), worker(farmer,hired), building(farm, 10, built), item(food), item(food), cow(20, 0, 10), tick.
%
:- use_module(library(chr)).
:- chr_constraint
  % Player actions
  harvest/1,
  % Resources
  worker/2,
  building/3,
  cow/3,
  item/1,
  % Engine
  tick/0, tapped/1, cleanup/0.

%
% Worker & Building Requirements
%
worker(Type, hired) ==> worker(Type, hungry).
building(Type, Id, built) ==> building(Type, Id, dormant).

worker(farmer, hungry), item(food) <=> worker(farmer, fed).
worker(villager, hungry), item(food) <=> worker(villager, fed).

building(farm, Id, dormant), worker(farmer, fed), worker(farmer, fed)
<=> building(farm, Id, active).

%
% Player Actions
%
harvest(Cow), cow(Cow, _, _) <=> meat, meat, meat, meat.

%
% Resource Farming
%
% Cows yield milk every 5 ticks
tick \ cow(Cow, 0, Farm), building(farm, Farm, active)
<=>
  tapped(cow(Cow, 4, Farm)),
  item(food).

% If it's not time to yield, just count down by 1.
tick \ cow(Cow, N0, Farm) <=> N is N0 - 1, tapped(cow(Cow, N, Farm)).

%
% Engine
%
tick <=> cleanup.

cleanup \ tapped(X) <=> call(X).
cleanup \ worker(_, hungry) <=> true.
cleanup \ building(_, _, active) <=> true.
cleanup <=> true.
