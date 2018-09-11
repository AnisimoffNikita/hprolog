clauses

  append([], L, L).
	append([H|T], L, [H|X]):-
    append(T, L, X).

goal

  append([1,2],[3],X).
