clauses

  a(X):-b(X),!.
  a(X):-c(X).
  b(X):-d(X).
  c(4).
  d(3):-!.
goal

  a(X).
