/*  

  Machiavelli domain for INDIGOLOG

*/


:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.


% There is nothing to do caching on (required becase cache/1 is static)
cache(_) :- fail.


/* PREDICATES */

/* range of numbers for our domain */
max_num(13).
num(N) :- max_num(M), between(1, M, N).

/* seeds for our domain */
seed(X) :- member(X, [d,c,s,h]).

/* define number succession */
succ(N1, N2) :- num(N1), num(N2), N2 is N1 + 1.

/* define the cards */
card(cAD).  card(cAC).  card(cAH).  card(cAS).
card(c2D).  card(c2C).  card(c2H).  card(c2S).
card(c3D).  card(c3C).  card(c3H).  card(c3S).
card(c4D).  card(c4C).  card(c4H).  card(c4S).
card(c5D).  card(c5C).  card(c5H).  card(c5S).
card(c6D).  card(c6C).  card(c6H).  card(c6S).
card(c7D).  card(c7C).  card(c7H).  card(c7S).
card(c8D).  card(c8C).  card(c8H).  card(c8S).
card(c9D).  card(c9C).  card(c9H).  card(c9S).
card(c10D). card(c10C). card(c10H). card(c10S).
card(cJD).  card(cJC).  card(cJH).  card(cJS).
card(cQD).  card(cQC).  card(cQH).  card(cQS).
card(cKD).  card(cKC).  card(cKH).  card(cKS).

/* associate numbers to the cards */
has_number(cAD, 1).  has_number(cAC, 1).  has_number(cAH, 1).  has_number(cAS, 1).
has_number(c2D, 2).  has_number(c2C, 2).  has_number(c2H, 2).  has_number(c2S, 2).
has_number(c3D, 3).  has_number(c3C, 3).  has_number(c3H, 3).  has_number(c3S, 3).
has_number(c4D, 4).  has_number(c4C, 4).  has_number(c4H, 4).  has_number(c4S, 4).
has_number(c5D, 5).  has_number(c5C, 5).  has_number(c5H, 5).  has_number(c5S, 5).
has_number(c6D, 6).  has_number(c6C, 6).  has_number(c6H, 6).  has_number(c6S, 6).
has_number(c7D, 7).  has_number(c7C, 7).  has_number(c7H, 7).  has_number(c7S, 7).
has_number(c8D, 8).  has_number(c8C, 8).  has_number(c8H, 8).  has_number(c8S, 8).
has_number(c9D, 9).  has_number(c9C, 9).  has_number(c9H, 9).  has_number(c9S, 9).
has_number(c10D, 10). has_number(c10C, 10). has_number(c10H, 10). has_number(c10S, 10).
has_number(cJD, 11).  has_number(cJC, 11).  has_number(cJH, 11).  has_number(cJS, 11).
has_number(cQD, 12).  has_number(cQC, 12).  has_number(cQH, 12).  has_number(cQS, 12).
has_number(cKD, 13).  has_number(cKC, 13).  has_number(cKH, 13).  has_number(cKS, 13).

/* associate seeds to the cards */
has_seed(cAD, d).  has_seed(cAC, c).  has_seed(cAH, h).  has_seed(cAS, s).
has_seed(c2D, d).  has_seed(c2C, c).  has_seed(c2H, h).  has_seed(c2S, s).
has_seed(c3D, d).  has_seed(c3C, c).  has_seed(c3H, h).  has_seed(c3S, s).
has_seed(c4D, d).  has_seed(c4C, c).  has_seed(c4H, h).  has_seed(c4S, s).
has_seed(c5D, d).  has_seed(c5C, c).  has_seed(c5H, h).  has_seed(c5S, s).
has_seed(c6D, d).  has_seed(c6C, c).  has_seed(c6H, h).  has_seed(c6S, s).
has_seed(c7D, d).  has_seed(c7C, c).  has_seed(c7H, h).  has_seed(c7S, s).
has_seed(c8D, d).  has_seed(c8C, c).  has_seed(c8H, h).  has_seed(c8S, s).
has_seed(c9D, d).  has_seed(c9C, c).  has_seed(c9H, h).  has_seed(c9S, s).
has_seed(c10D, d). has_seed(c10C, c). has_seed(c10H, h). has_seed(c10S, s).
has_seed(cJD, d).  has_seed(cJC, c).  has_seed(cJH, h).  has_seed(cJS, s).
has_seed(cQD, d).  has_seed(cQC, c).  has_seed(cQH, h).  has_seed(cQS, s).
has_seed(cKD, d).  has_seed(cKC, c).  has_seed(cKH, h).  has_seed(cKS, s).


/* FLUENTS and CAUSAL LAWS */

/* free: if a card is not part of any pile (it's in the hand) */
rel_fluent(free(C)) :- card(C).

% causal laws for numpile actions
causes_true(dismantle_numpile(R), free(C), in_numpile_of(C,R)).
causes_false(build_numpile(C,_,_), free(C), true).
causes_false(build_numpile(_,C,_), free(C), true).
causes_false(build_numpile(_,_,C), free(C), true).
causes_false(add_to_numpile(C,_), free(C), true).

% causal laws for seedpile actions
causes_true(dismantle_seedpile(R), free(C), in_seedpile_of(C,R)).
causes_false(build_seedpile(C,_,_), free(C), true).
causes_false(build_seedpile(_,C,_), free(C), true).
causes_false(build_seedpile(_,_,C), free(C), true).
causes_false(add_to_seedpile(C,_), free(C), true).

% causal laws for exogenous actions
causes_true(card_appears(C), free(C), true).
causes_true(pile_collapses(R), free(C), or(in_numpile_of(C,R), in_seedpile_of(C,R))).
causes_true(card_disappears(C), free(C2), 
  and(
    or(
      and(in_numpile_of(C,R), in_numpile_of(C2,R)),
      and(in_seedpile_of(C,R), in_seedpile_of(C2,R))
    ),
    neg(=(C,C2))
  )
).
causes_false(card_disappears(C), free(C), free(C)).


/* in_numpile_of: if c is in the pile of cards with 
   the same number having reference ref */
rel_fluent(in_numpile_of(C,R)) :- card(C), card(R).

% causal laws for nunmpile actions
causes_true(build_numpile(R,_,_), in_numpile_of(R,R), true).
causes_true(build_numpile(R,C,_), in_numpile_of(C,R), true).
causes_true(build_numpile(R,_,C), in_numpile_of(C,R), true).
causes_true(add_to_numpile(C,R), in_numpile_of(C,R), true).
causes_false(dismantle_numpile(R), in_numpile_of(C,R), in_numpile_of(C,R)).

% causal laws for exogenous actions
causes_false(pile_collapses(R), in_numpile_of(C,R), in_numpile_of(C,R)).
causes_false(card_disappears(C), in_numpile_of(C2,R), 
  and(in_numpile_of(C,R), in_numpile_of(C2,R))
).

/* in_seedpile_of: if c is in the pile of cards with 
   the same seed having reference ref */
rel_fluent(in_seedpile_of(C,R)) :- card(C), card(R).

% causal laws for seedpile actions
causes_true(build_seedpile(R,_,_), in_seedpile_of(R,R), true).
causes_true(build_seedpile(R,C,_), in_seedpile_of(C,R), true).
causes_true(build_seedpile(R,_,C), in_seedpile_of(C,R), true).
causes_true(add_to_seedpile(C,R), in_seedpile_of(C,R), true).
causes_false(dismantle_seedpile(R), in_seedpile_of(C,R), in_seedpile_of(C,R)).

% causal laws for exogenous actions
causes_false(pile_collapses(R), in_seedpile_of(C,R), in_seedpile_of(C,R)).
causes_false(card_disappears(C), in_seedpile_of(C2,R), 
  and(in_seedpile_of(C,R), in_seedpile_of(C2,R))
).

/* some_changes: if some exogenous action has been executed and 
   it wasn't dealt with yet */
rel_fluent(some_changes).

causes_true(card_appears(_), some_changes, true).
causes_true(card_disappears(_), some_changes, true).
causes_true(pile_collapses(_), some_changes, true).


/* ACTIONS and PRECONDITIONS */

prim_action(build_numpile(C1,C2,C3)) :- card(C1), card(C2), card(C3).
poss(build_numpile(C1,C2,C3), (
  \+ (=(C1,C2)),
  \+ (=(C1,C3)),
  \+ (=(C2,C3)),
  free(C1),
  free(C2),
  free(C3),
  has_number(C1,N),
  has_number(C2,N),
  has_number(C3,N)
)).


prim_action(add_to_numpile(C,R)) :- card(C), card(R).
poss(add_to_numpile(C,R), (
  free(C),
  in_numpile_of(R,R),
  has_number(C,N),
  has_number(R,N)
)).


prim_action(dismantle_numpile(R)) :- card(R).
poss(dismantle_numpile(R), in_numpile_of(R,R)).


prim_action(build_seedpile(C1, C2, C3)) :- card(C1), card(C2), card(C3).
poss(build_seedpile(C1,C2,C3), (
  \+ (=(C1,C2)),
  \+ (=(C1,C3)),
  \+ (=(C2,C3)),
  free(C1),
  free(C2),
  free(C3),
  has_seed(C1,S),
  has_seed(C2,S),
  has_seed(C3,S),
  has_number(C1,N1),
  has_number(C2,N2),
  has_number(C3,N3),
  succ(N1,N2),
  succ(N2,N3)
)).


prim_action(add_to_seedpile(C,R)) :- card(C), card(R).
poss(add_to_seedpile(C,R), (
  free(C),
  in_seedpile_of(R,R),
  has_seed(C,S),
  has_seed(R,S),
  has_number(C,N),
  in_seedpile_of(C2,R),
  has_number(C2,N2),
  (succ(N,N2) ; succ(N2,N))
)).


prim_action(dismantle_seedpile(R)):- card(R).
poss(dismantle_seedpile(R), in_seedpile_of(R,R)).


/* EXOGENOUS ACTIONS */

/* add to the game (in the hand of the player) a new card */
exog_action(card_appears(C)) :- card(C).

/* pile collapses */
exog_action(pile_collapses(R)) :- card(R).

/* remove card from the game */
exog_action(card_disappears(C)) :- card(C).

prim_action(Act) :- exog_action(Act).

poss(card_appears(C), (
  \+ free(C),
  \+ in_numpile_of(C,R),
  \+ in_seedpile_of(C,R)
)) :- card(C), card(R).

poss(card_disappears(C), (
  free(C);
  in_numpile_of(C,R);
  in_seedpile_of(C,R)
)) :- card(C), card(R).

poss(pile_collapses(R), (
  in_numpile_of(R,R);
  in_seedpile_of(R,R)
)) :- card(R).


/* INITIAL STATE */
initially(free(C), true) :- card(C), member(C, [c7D, c8D, c9D, c4D, cQC]).
initially(free(C), false) :- card(C), \+ initially(free(C), true).

initially(in_numpile_of(C,R), true) :- card(C), card(R), =(R,c4C), member(C, [c4C, c4H, c4S]).
initially(in_numpile_of(C,R), true) :- card(C), card(R), =(R,cQS), member(C, [cQS, cQD, cQH]).
initially(in_numpile_of(C,R), false) :- card(C), card(R), \+ initially(in_numpile_of(C,R), true).

initially(in_seedpile_of(C,R), false) :- card(C), card(R), \+ initially(in_seedpile_of(C,R), true).

initially(some_changes, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% all actions with non-deterministically chosen parameters
proc(pi_build_numpile,
  pi([c1,c2,c3], build_numpile(c1,c2,c3))
).

proc(pi_add_to_numpile,
  pi([c,r], add_to_numpile(c,r))
).

proc(pi_dismantle_numpile,
  pi(r, dismantle_numpile(r))
).

proc(pi_build_seedpile,
  pi([c1,c2,c3], build_seedpile(c1,c2,c3))
).

proc(pi_add_to_seedpile,
  pi([c,r], add_to_seedpile(c,r))
).

proc(pi_dismantle_seedpile,
  pi(r, dismantle_seedpile(r))
).


% choosing randomly between all 6 actions
proc(choose_action_full,
  ndet(
    ndet(pi_add_to_numpile, pi_add_to_seedpile),
    ndet(
      ndet(pi_build_numpile, pi_build_seedpile),
      ndet(pi_dismantle_numpile, pi_dismantle_seedpile)
    )
  )
).

% check if there is some free card
proc(some_free,
  some(c, and(card(c), free(c)))
).


/* full_search controller: choose non-deterministically random actions 
   until it finds a solution */
proc(control(full_search), search(full_search)).
proc(full_search, [
  star(choose_action_full),
  ?(neg(some_free))
]).


% dismantle a randomly chosen pile
proc(dismantle_some_pile, pi(r, ndet(
  [?(in_numpile_of(r,r)), dismantle_numpile(r)],
  [?(in_seedpile_of(r,r)), dismantle_seedpile(r)]
))).

% check if there is a pile
proc(exists_some_pile, some(r, or(in_numpile_of(r,r), in_seedpile_of(r,r)))).

% choose randomly between 4 actions (adds and builds)
proc(choose_action_create,
  ndet(
    ndet(pi_add_to_numpile, pi_add_to_seedpile),
    ndet(pi_build_numpile, pi_build_seedpile)
  )
).

/* Dismantle and Rebuild controller: dismantle all piles and 
   rebuild everything from scratch */
proc(control(dismantle_and_rebuild), dismantle_and_rebuild).
proc(dismantle_and_rebuild, [
  while(
    exists_some_pile,
    dismantle_some_pile
  ),
  search([
    star(choose_action_create),
    ?(neg(some_free))
  ])
]).


/* Reactive controller: when it receives an exogenous action, interrupt the 
   current search (or execution) and restarts. Terminates when it reaches
   the goal. */
proc(control(reactive), [prioritized_interrupts([
  interrupt(some_free, [
    if(some_changes, unset(some_changes), []),
    gexec(neg(some_changes), search(full_search))
  ])
])]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
