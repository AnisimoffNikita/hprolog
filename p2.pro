clauses

  a(X):-b(X).
  a(X):-c(X).
  b(3):-!.
  c(4).
  d(3):-!.
goal

  a(X).
