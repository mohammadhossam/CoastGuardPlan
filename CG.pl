:- include('KB3.pl').

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], s0) :-
    agent_loc(X, Y),
    Load = 0,
    ships_loc([_ | S2]),
    Ship1_load = 1,
    ((S2 = [], Ship2_load = []); (Ship2_load = [1])).

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(pickup, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    Y = Y_old,
    X = X_old,
    capacity(Capacity),
    Load is Load_old + 1,
    Load =< Capacity,
    ships_loc([[X1, Y1] | Loc2]),
    ((X = X1, Y = Y1, Ship1_load_old = 1, Ship1_load = 0, Ship2_load = Ship2_load_old);
    (Loc2 = [[X2, Y2]], X = X2, Y = Y2, Ship2_load_old = [1], Ship2_load = [0], Ship1_load = Ship1_load_old)).

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

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(drop, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    Y = Y_old,
    X = X_old,
    Load_old > 0,
    Load = 0,
    station(X1, Y1),
    X = X1,
    Y = Y1,
    Ship1_load = Ship1_load_old,
    Ship2_load = Ship2_load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(left, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    Y is Y_old - 1,
    Y >= 0,
    X = X_old,
    Load = Load_old,
    Ship1_load = Ship1_load_old,
    Ship2_load = Ship2_load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(up, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    Y = Y_old,
    X is X_old - 1,
    X >= 0,
    Load = Load_old,
    Ship1_load = Ship1_load_old,
    Ship2_load = Ship2_load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(right, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    grid(D, D),
    Y is Y_old + 1,
    Y < D,
    X = X_old,
    Load = Load_old,
    Ship1_load = Ship1_load_old,
    Ship2_load = Ship2_load_old.

describe_situation(guard_loc(X, Y), guard_load(Load), [Ship1_load | Ship2_load], result(down, S)) :-
    describe_situation(guard_loc(X_old, Y_old), guard_load(Load_old), [Ship1_load_old | Ship2_load_old], S),
    grid(D, D),
    Y = Y_old,
    X is X_old + 1,
    X < D,
    Load = Load_old,
    Ship1_load = Ship1_load_old,
    Ship2_load = Ship2_load_old.


goal(S) :-
    ids(S,1).

goal2(S) :-
    describe_situation(guard_loc(_, _), guard_load(Load), [Ship1_load | Ship2_load], S),
    Load = 0,
    Ship1_load = 0,
    (Ship2_load = []; Ship2_load = [0]).

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded,
                                                L1 is L+1, ids(X,L1)).