clauses

f(0, 5):-!.
f(X, N):-Y is X - 1, f(Y, N).
f(_, 3).

goal

f(4,X).
