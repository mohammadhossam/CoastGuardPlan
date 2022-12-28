:- include('KB2.pl').

move_validity(X, Y, A) :-
    (A = left, Y >= 0);
    (A = up, X >= 0);
    (A = right, grid(D, D), Y < D);
    (A = down, grid(D, D), X < D).

pickup_validity(X, Y, Load, RemainingShips) :-
    member([X, Y], RemainingShips),
    capacity(C),
    Load < C.

drop_validity(X, Y, Load) :-
    station(X, Y),
    Load > 0.


describe_situation(X, Y, Load, ShipLocations, s0) :-
    agent_loc(X, Y),
    Load = 0,
    ships_loc(ShipLocations).

% Successor State Axiom
describe_situation(X, Y, Load, ShipLocations, result(Action, S)) :-
    describe_situation(X1, Y1, Load_old, ShipLocations_old, S),
    (
        (Action = left, X is X1, Y is Y1 - 1, Load is Load_old, move_validity(X, Y, Action), ShipLocations = ShipLocations_old);
        (Action = up, X is X1 - 1, Y is Y1, Load is Load_old, move_validity(X, Y, Action), ShipLocations = ShipLocations_old);
        (Action = right, X is X1, Y is Y1 + 1, Load is Load_old, move_validity(X, Y, Action), ShipLocations = ShipLocations_old);
        (Action = down, X is X1 + 1, Y is Y1, Load is Load_old, move_validity(X, Y, Action), ShipLocations = ShipLocations_old);
        (Action = pickup, X is X1, Y is Y1, pickup_validity(X, Y, Load_old, ShipLocations_old), Load is Load_old + 1, delete(ShipLocations_old, [X, Y], ShipLocations));
        (Action = drop, X is X1, Y is Y1, drop_validity(X, Y, Load_old), Load is 0, ShipLocations = ShipLocations_old)
    ).

goal(S) :-
    ids(S,1).

goal2(S) :-
    describe_situation(_, _, 0, [], S).

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded,
                                                L1 is L+1, ids(X,L1)).