f(1,1):-!.
f(N,X):-M is N - 1, f(M,Y), X is Y * N.