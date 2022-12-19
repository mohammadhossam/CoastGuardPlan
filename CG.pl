:- include('KB3.pl').

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, s0) :-
    agent_loc(X, Y),
    Load = 0,
    ships_loc(ShipLocations).

% There are 2 ships and we're gonna pickup from the first one
describe_situation(guard_loc(X, Y), guard_load(Load), [[X2, Y2]], result(pickup, S)) :-
    describe_situation(guard_loc(X, Y), guard_load(Load_old), [[X, Y], [X2, Y2]], S),
    capacity(Capacity),
    Load is Load_old + 1,
    Load =< Capacity.

% There are 2 ships and we're gonna pickup from the second one
describe_situation(guard_loc(X, Y), guard_load(Load), [[X1, Y1]], result(pickup, S)) :-
    describe_situation(guard_loc(X, Y), guard_load(Load_old), [[X1, Y1], [X, Y]], S),
    capacity(Capacity),
    Load is Load_old + 1,
    Load =< Capacity.

% There's only one ship and we're at its location
describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(pickup, S)) :-
    describe_situation(guard_loc(X, Y), guard_load(Load_old), [[X, Y]], S),
    capacity(Capacity),
    Load is Load_old + 1,
    Load =< Capacity,
    ShipLocations = [].


% describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load, Ship2_load], result(pickup, S)) :-
%     describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old, Ship2_load_old], S),
%     Y = Y_old,
%     X = X_old,
%     capacity(Capacity),
%     Load is Load_old + 1,
%     Load =< Capacity,
%     ships_loc([[X1, Y1], [X2, Y2]]),
%     ((X = X1, Y = Y1, Ship1_load_old = 1, Ship1_load = 0, Ship2_load = Ship2_load_old);
%     (X = X2, Y = Y2, Ship2_load_old = 1, Ship2_load = 0, Ship1_load = Ship1_load_old)).

% describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load], result(pickup, S)) :-
%     describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old], S),
%     Y = Y_old,
%     X = X_old,
%     capacity(Capacity),
%     Load is Load_old + 1,
%     Load =< Capacity,
%     ships_loc([[X1, Y1]]),
%     ((X = X1, Y = Y1, Ship1_load_old = 1, Ship1_load = 0)).

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(drop, S)) :-
    station(X, Y),
    describe_situation(guard_loc(X, Y), guard_load(Load_old), ShipLocations, S),
    Load_old > 0,
    Load = 0.

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(left, S)) :-
    describe_situation(guard_loc(X, Y_old), guard_load(Load_old), ShipLocations, S),
    Y is Y_old - 1,
    Y >= 0,
    Load = Load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(up, S)) :-
    describe_situation(guard_loc(X_old, Y), guard_load(Load_old), ShipLocations, S),
    X is X_old - 1,
    X >= 0,
    Load = Load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(right, S)) :-
    describe_situation(guard_loc(X, Y_old), guard_load(Load_old), ShipLocations, S),
    grid(D, D),
    Y is Y_old + 1,
    Y < D,
    Load = Load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), ShipLocations, result(down, S)) :-
    describe_situation(guard_loc(X_old, Y), guard_load(Load_old), ShipLocations, S),
    grid(D, D),
    X is X_old + 1,
    X < D,
    Load = Load_old.


goal(S) :-
    ids(S,1).

goal2(S) :-
    describe_situation(guard_loc(_, _), guard_load(Load), ShipLocations, S),
    Load = 0,
    ShipLocations = [].

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded,
                                                L1 is L+1, ids(X,L1)).