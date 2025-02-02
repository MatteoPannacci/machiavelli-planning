
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

/* associate cards with seeds and numbers */


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
  neg(=,C1,C2),
  neg(=,C1,C3),
  neg(=,C2,C3),
  free(C1),
  free(C2),
  free(C3),
  has_number(C1,N),
  has_number(C2,N),
  has_number(C3,N)
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
  neg(=,C1,C2),
  neg(=,C1,C3),
  neg(=,C2,C3),
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

prim_action(add_to_seedpile(C, R)).
poss(add_to_seedpile(C, R), and(
  free(C),
  in_seedpile_of(R,R),
  has_seed(C,S),
  has_seed(R,S),
  has_number(C,N),
  has_number(R,N2),
  card(C2),
  in_seedpile_of(C2,R),
  has_number(C2,N2),
  or (succ(N,N2) , succ(N2,N)) % pretty sure this is syntactically wrong
)).

prim_action(dismantle_seedpile(R)):- card(R).
poss(dismantle_seedpile(R), in_seedpile_of(R,R)).



prim_action(down).
poss(down, and(neg(door_open), neg(floor = 1))).

prim_action(up).
poss(up, and(neg(door_open), neg(floor = N))) :- max_floor(N).

prim_action(toggle).  % toggle ring alarm
poss(toggle, true).

prim_action(ring).    % do one ring
poss(ring, true).

prim_action(off(N)) :- fl(N).    % turn off call button on floor n
poss(off(N), and(floor = N, light(N))).
prim_action(open).		% open door
poss(open, true).

prim_action(close).   % close door
poss(close, true).

prim_action(look(N)) :- fl(N).  % sense floor N light
poss(look(_), true).


  /* EXOGENOUS ACTIONS */
exog_action(heat).            % increase temperature
exog_action(cold).            % decrease temperature
exog_action(smoke).           % smoke enters elevator
exog_action(reset).           % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).  % turn on call button on floor n

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

  /* ABBREVIATIONS */
proc(too_hot, temp > 22).
proc(too_cold, temp < 16).
proc(above_floor(N), floor > N).
proc(below_floor(N), floor < N).
proc(pending_floor(N), light(N)).
proc(some_pending, some(n, light(n))).


  /* INITIAL STATE */
initially(floor, 2).
initially(light(N), true) :- fl(N), member(N, [1, 3, 7, 8]).
initially(light(N), false) :- fl(N), \+ initially(light(N), true).
initially(temp, 2).
initially(fan, false).
initially(alarm, false).
initially(new_request, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(go_floor(N),
  [if(door_open, close, []),
    while(neg(floor = N), if(below_floor(N), up, down))]).
proc(serve_floor(N), [go_floor(N), open, close, off(N)]).

% pick a floor that is pending to be served and serve it
proc(serve_some_floor, pi(n, [?(pending_floor(n)), serve_floor(n)])).


% DUMB: just kep serving some pending floor and then go down
%     NO REACTION TO EXOGENOUS ACTIONS OR SENSING
proc(control(dumb),
   [ while(some_pending, serve_some_floor ),
     go_floor(1), % go to floor 1 to park
     open ] ).

% SMART: build a shorter plan and then execute it all
%     NO REACTION TO EXOGENOUS ACTIONS OR SENSING
proc(control(smart), search(minimize_motion(0)) ).  /* eventually succeeds */

proc(minimize_motion(Max),  /* iterative deepening search */
    ndet( handle_reqs(Max), pi(m, [?(m is Max + 1), minimize_motion(m)]))).

proc(handle_reqs(Max),      /* handle all elevator reqs in Max steps */
    ndet(  [?(and(neg(some_pending), Max >= floor - 1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n), m is Max - abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
