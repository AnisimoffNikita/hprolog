clauses

  permutation([X,Y|T],[Y,X|T]):-
                X > Y,!.
	permutation([X|T],[X|T1]):-
                permutation(T,T1).
	bubble(L,L1):-
     		permutation(L,LL),!, bubble(LL,L1).
	bubble(L,L).
goal

  bubble([3,2,1], X).
