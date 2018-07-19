f(1,1):-!.
f(N,X):-M is N - 1, f(M,Y), X is Y * N.

mapf([],Acc,Acc).
mapf([H|T], Acc, Res):-f(H,NH),mapf(T,[NH|Acc],Res).

equal(X,X).