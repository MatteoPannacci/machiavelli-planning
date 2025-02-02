
/*
  Complex elevator domain as per the original INDIGOLOG code

  @author Sebastian Sardina - ssardina@gmail.com  (2001-2006)
		(based on code previously written by Hector Levesque)

  This file contains 4 of the controllers from the original code
  written by Hector Levesque for the 1st INDIGOLOG version:

  1. controller(1) : (example2.pl in the original INDIGOLOG)
  The dumb controller tries without search but commits too soon

  2. controller(2) : (example2.pl in the original INDIGOLOG)
  The smart controller uses search to minimize the up-down motion

  3. controller(3) : (example3.pl in the original INDIGOLOG)
  This is the elevator that appears in the IJCAI-97 paper on ConGolog
  It uses exogenous actions for temperature, smoke, and call buttons

  4. controller(4) : (example4.pl in the original INDIGOLOG)
  This is the elevator with no exogenous events, but with sensing
  actions for each call button of the elevator
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



/* DOMAINS-SORTS AVAILABLE */

/* acceptable numbers for our domain */
max_num(13).
num(N) :- max_num(M), between(1, M, N).

/* acceptable seeds for our domain */
seed(X) :- member(X, [d,c,s,h]).

/* define successors succ (could not be needed)*/
succ(N1, N2) :- num(N1), num(N2), N2 is N1 + 1.

/* define the cards */

% define diamonds cards
card(cAD). has_number(cAD, 1). has_seed(cAD, d).
card(c2D). has_number(c2D, 2). has_seed(c2D, d).
card(c3D). has_number(c3D, 3). has_seed(c3D, d).
card(c4D). has_number(c4D, 4). has_seed(c4D, d).
card(c5D). has_number(c5D, 5). has_seed(c5D, d).
card(c6D). has_number(c6D, 6). has_seed(c6D, d).
card(c7D). has_number(c7D, 7). has_seed(c7D, d).
card(c8D). has_number(c8D, 8). has_seed(c8D, d).
card(c9D). has_number(c9D, 9). has_seed(c9D, d).
card(c10D). has_number(c10D, 10). has_seed(c10D, d).
card(cJD). has_number(cJD, 11). has_seed(cJD, d).
card(cQD). has_number(cQD, 12). has_seed(cQD, d).
card(cKD). has_number(cKD, 13). has_seed(cKD, d).

% define clubs cards
card(cAC). has_number(cAC, 1). has_seed(cAC, c).
card(c2C). has_number(c2C, 2). has_seed(c2C, c).
card(c3C). has_number(c3C, 3). has_seed(c3C, c).
card(c4C). has_number(c4C, 4). has_seed(c4C, c).
card(c5C). has_number(c5C, 5). has_seed(c5C, c).
card(c6C). has_number(c6C, 6). has_seed(c6C, c).
card(c7C). has_number(c7C, 7). has_seed(c7C, c).
card(c8C). has_number(c8C, 8). has_seed(c8C, c).
card(c9C). has_number(c9C, 9). has_seed(c9C, c).
card(c10C). has_number(c10C, 10). has_seed(c10C, c).
card(cJC). has_number(cJC, 11). has_seed(cJC, c).
card(cQC). has_number(cQC, 12). has_seed(cQC, c).
card(cKC). has_number(cKC, 13). has_seed(cKC, c).

% define hearts cards
card(cAH). has_number(cAH, 1). has_seed(cAH, h).
card(c2H). has_number(c2H, 2). has_seed(c2H, h).
card(c3H). has_number(c3H, 3). has_seed(c3H, h).
card(c4H). has_number(c4H, 4). has_seed(c4H, h).
card(c5H). has_number(c5H, 5). has_seed(c5H, h).
card(c6H). has_number(c6H, 6). has_seed(c6H, h).
card(c7H). has_number(c7H, 7). has_seed(c7H, h).
card(c8H). has_number(c8H, 8). has_seed(c8H, h).
card(c9H). has_number(c9H, 9). has_seed(c9H, h).
card(c10H). has_number(c10H, 10). has_seed(c10H, h).
card(cJH). has_number(cJH, 11). has_seed(cJH, h).
card(cQH). has_number(cQH, 12). has_seed(cQH, h).
card(cKH). has_number(cKH, 13). has_seed(cKH, h).

% define spades cards
card(cAS). has_number(cAS, 1). has_seed(cAS, s).
card(c2S). has_number(c2S, 2). has_seed(c2S, s).
card(c3S). has_number(c3S, 3). has_seed(c3S, s).
card(c4S). has_number(c4S, 4). has_seed(c4S, s).
card(c5S). has_number(c5S, 5). has_seed(c5S, s).
card(c6S). has_number(c6S, 6). has_seed(c6S, s).
card(c7S). has_number(c7S, 7). has_seed(c7S, s).
card(c8S). has_number(c8S, 8). has_seed(c8S, s).
card(c9S). has_number(c9S, 9). has_seed(c9S, s).
card(c10S). has_number(c10S, 10). has_seed(c10S, s).
card(cJS). has_number(cJS, 11). has_seed(cJS, s).
card(cQS). has_number(cQS, 12). has_seed(cQS, s).
card(cKS). has_number(cKS, 13). has_seed(cKS, s).


/* FLUENTS and CAUSAL LAWS */

/* free: if a card is not part of any pile */
/* CAN THEY BE REGROUPED? */
rel_fluent(free(C)) :- card(C).

/* maybe it's more efficent to put conditions with :- ? */
causes_true(dismantle_numpile(R), free(C), in_numpile_of(C,R)).
causes_false(build_numpile(C,_,_), free(C), true).
causes_false(build_numpile(_,C,_), free(C), true).
causes_false(build_numpile(_,_,C), free(C), true).
causes_false(add_to_numpile(C,R), free(C), true).

causes_true(dismantle_seedpile(R), free(C), in_seedpile_of(C,R)).
causes_false(build_seedpile(C,_,_), free(C), true).
causes_false(build_seedpile(_,C,_), free(C), true).
causes_false(build_seedpile(_,_,C), free(C), true).
causes_false(add_to_seedpile(C,R), free(C), true).

/* in_numpile_of: if c is in the pile of cards with 
   the same number having reference ref */
rel_fluent(in_numpile_of(C,R)) :- card(C), card(R).

causes_true(build_numpile(R,_,_), in_numpile_of(R,R), true).
causes_true(build_numpile(R,C,_), in_numpile_of(C,R), true).
causes_true(build_numpile(R,_,C), in_numpile_of(C,R), true).
causes_true(add_to_numpile(C,R), in_numpile_of(C,R), true).
causes_false(dismantle_numpile(R), in_numpile_of(C,R), in_numpile_of(C,R)).

/* in_seedpile_of: if c is in the pile of cards with 
   the same seed having reference ref */
rel_fluent(in_seedpile_of(C,R)) :- card(C), card(R).

causes_true(build_seedpile(R,_,_), in_seedpile_of(R,R), true).
causes_true(build_seedpile(R,C,_), in_seedpile_of(C,R), true).
causes_true(build_seedpile(R,_,C), in_seedpile_of(C,R), true).
causes_true(add_to_seedpile(C,R), in_seedpile_of(C,R), true).
causes_false(dismantle_seedpile(R), in_seedpile_of(C,R), in_seedpile_of(C,R)).


/* ACTIONS and PRECONDITIONS */

prim_action(build_numpile(C1,C2,C3)) :- card(C1), card(C2), card(C3). % are the arguments needed?
poss(build_numpile, and(
  neg(=(C1,C2)),
  neg(=(C1,C3)),
  neg(=(C2,C3)),
  free(C1),
  free(C2),
  free(C3),
  has_number(C1,N),
  has_number(C2,N),
  has_number(C3,N)
  % should we bind N (using "some")?
)).

prim_action(add_to_numpile(C,R)) :- card(C), card(R).
poss(add_to_numpile(C,R), and(
  free(C),
  in_numpile_of(R,R),
  has_number(C,N),
  has_number(R,N)
)).

prim_action(dismantle_numpile(R)) :- card(R).
poss(dismantle_numpile(R), in_numpile_of(R,R)).

prim_action(build_seedpile(C1, C2, C3)) :- card(C1), card(C2), card(C3). % not convinced about this notation.
poss(build_seedpile, and(
  neg(=(C1,C2)),
  neg(=(C1,C3)),
  neg(=(C2,C3)),
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
poss(add_to_seedpile(C,R), and(
  free(C),
  in_seedpile_of(R,R),
  has_seed(C,S),
  has_seed(R,S),
  has_number(C,N),
  card(C2),
  in_seedpile_of(C2,R),
  has_number(C2,N2),
  or(succ(N,N2), succ(N2,N)) % pretty sure this is syntactically wrong
)).

prim_action(dismantle_seedpile(R)):- card(R).
poss(dismantle_seedpile(R), in_seedpile_of(R,R)).


/* EXOGENOUS ACTIONS */
/*
exog_action(heat).            % increase temperature
exog_action(cold).            % decrease temperature
exog_action(smoke).           % smoke enters elevator
exog_action(reset).           % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).  % turn on call button on floor n

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).
*/

/* ABBREVIATIONS */
/*
proc(too_hot, temp > 22).
proc(too_cold, temp < 16).
proc(above_floor(N), floor > N).
proc(below_floor(N), floor < N).
proc(pending_floor(N), light(N)).
proc(some_pending, some(n, light(n))).
*/

/* INITIAL STATE */
initially(free(C), true) :- card(N), member(N, [c7D, c8D, c9D, c4D]).
initially(free(C), false) :- card(N), \+ initially(free(C), true).

initially(in_numpile_of(C,R), true) :- card(C), card(R), =(R,c4C), member(C, [c4C, c4H, c4S]).
initially(in_numpile_of(C,R), true) :- card(C), card(R), =(R,cQS), member(C, [cQS, cQD, cQH]).
initially(in_numpile_of(C,R), false) :- card(C), card(R), \+ initially(in_numpile_of(C,R), true).

initially(in_seedpile_of(C,R), false) :- card(C), card(R), \+ initially(in_seedpile_of(C,R), true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(control(base), search(minimize_cost(0))).

proc(minimize_cost(Max),
  ndet(solve_in_max(Max), pi(m, [?(m is Max+1), minimize_cost(m)] )).
)

proc(solve_in_max(Max),
  ndet(

    [?(neg(some(X, and(card(X), free(X)))))],

    pi(c, pi(r, [
      ?(m is Max - 1),
      add_to_numpile(c,r),
      solve_in_max(m)
    ])),

    pi(c1, pi(c2, pi(c3, [
      ?(and(m is Max - 3, m > 0)),
      build_numpile(c1,c2,c3),
      solve_in_max(m)
    ]))),
    
    pi(r, [
      ?(and(m is Max - 3, m > 0)),
      dismantle_numpile(r),
      solve_in_max(m)
    ]),

    pi(c, pi(r, [
      ?(m is Max - 1),
      add_to_seedpile(c,r),
      solve_in_max(m)
    ])),

    pi(c1, pi(c2, pi(c3, [
      ?(and(m is Max - 3, m > 0)),
      build_seedpile(c1,c2,c3),
      solve_in_max(m)
    ]))),
    
    pi(r, [
      ?(and(m is Max - 3, m > 0)),
      dismantle_seedpile(r),
      solve_in_max(m)
    ]),

  )
)




/* REACTIVE CONTROLLER:

This is an extension of the elevator that appears in the IJCAI-97 AND
AIJ-03 papers on ConGolog

It uses exogenous actions for temperature, smoke, and call buttons. It
also uses prioritized interrupts to handle the exogenous events and the
call buttons.

The serving of floors is still naive: just serve some pending floor

It is extended to:

  - track the state of the door
  - wait at ground floor for more requests

*/

/* --------------------------------------------
   TO CHANGE!
  ---------------------------------------------

proc(control(congolog), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
         interrupt(n, pending_floor(n), serve_floor(n)),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open),
         interrupt(true, ?(wait_exog_action))])]).


%  REACTIVE + PLANNING CONTROLLERS
proc(control(indigolog), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
         interrupt(some_pending,
            [ unset(new_request),
              gexec(neg(new_request), search(minimize_motion(0), "Searching for plan"))
              ]),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open),
         interrupt(true, ?(wait_exog_action))])]).


proc(control(indigolog_ends), [prioritized_interrupts(
          [interrupt(and(too_hot, neg(fan)), toggle),
           interrupt(and(too_cold, fan), toggle),
           interrupt(alarm, ring),
           interrupt(some_pending,
              [ unset(new_request),
                gexec(neg(new_request), search(minimize_motion(0), "Searching for plan"))
                ]),
           interrupt(above_floor(1), down),
           interrupt(neg(door_open), open)])
           ]).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
