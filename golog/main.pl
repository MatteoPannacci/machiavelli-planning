/*  

	Main file for Machiavelli domain

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT INDIGOLOG FRAMEWORK
%
%    Configuration files
%    Interpreter
%    Environment manager
%    Evaluation engine/Projector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dir(indigolog, F), consult(F).
:- dir(eval_bat, F), consult(F).    % after interpreter always!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT APPLICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [machiavelli].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPECIFY ADDRESS OF ENVIRONMENT MANAGER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Any port available would be ok for the EM.
em_address(localhost, 8000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENVIRONMENTS/DEVICES TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_devices([simulator]).

% start env_sim.pl tcl/tk interaction interface

load_device(simulator, Host:Port, [pid(PID)]) :-
	dir(dev_simulator, File),
	ARGS = ['-e', 'swipl', '-t', 'start', File, '--host', Host, '--port', Port],
	logging(
		info(5, app), "Command to initialize device simulator: xterm -e ~w", [ARGS]),
	process_create(
		path(xterm), ARGS,
		[process(PID)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

how_to_execute(Action, simulator, sense(Action)) :-
	sensing_action(Action, _).

how_to_execute(Action, simulator, Action) :-
	 \+ sensing_action(Action, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%
%          translate_exog(Code, Action)
%          translate_sensing(Action, Outcome, Value)
%
% OBS: If not present, then the translation is 1-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_exog(ActionCode, Action) :-
	actionNum(Action, ActionCode), !.
translate_exog(A, A).
translate_sensing(_, SR, SR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets INDIGOLOG to evaluate a chosen mainControl procedure

main :-
	findall(C,
		proc(
			control(C), _), LC),
	length(LC, N), repeat,
	format('Controllers available: ~w\n', [LC]),
	forall(
		(
			between(1, N, I),
			nth1(I, LC, C)),
		format('~d. ~w\n', [I, C])), nl, nl,
	write('Select controller: '),
	read(NC), nl,
	number(NC),
	nth1(NC, LC, C),
	format('Executing controller: *~w*\n', [C]), !,
	main(
		control(C)).

main(C) :-
	assert(
		control(C)),
	indigolog(C).


:- set_option(log_level, 5).
:- set_option(log_level, em(1)).
:- set_option(wait_step, 5).


% Legality Task (read from terminal the sequence of actions)
legality_task :-
	format("Write sequence of actions:\n"),
	read(SEQ), nl,
	indigolog(SEQ).

% Projection Task (read from terminal the condition and the sequence of actions)
projection_task :-
	format("Choose condition to satisfy:\n"),
	read(COND), nl,
	format("Write sequence of actions:\n"),
	read(SEQ), nl,
	indigolog([SEQ, ?(COND)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
