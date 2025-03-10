# machiavelli-planning

Project developed for the course of "Planning and Reasoning" during the MSc in Artificial Intelligence and Robotics at Sapienza University of Rome, A.Y. 2024-2025.


## Summary

Implementation in PDDL and IndiGolog of a task from the [italian card game Machiavelli](https://en.wikipedia.org/wiki/Machiavelli_(Italian_card_game)).

We take into consideration a single turn of a player during the course of the game. The goal is to check whether it’s possible for the player to win from the current configuration (board state and player's hand) and, in the positive case, to find a sequence of legal actions that do so.

In the case of PDDL we can search for an optimal plan instead of a satisfying one, with the purpose of finding the sequence of actions that is the easiest to execute for a human player. In the case of IndiGolog instead we consider the existence of toy exogenous actions to study the behaviour of the controllers in a dynamic environment.
