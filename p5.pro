clauses

  append([], List2, List2).
	append([Head|Tail], List2, [Head|TailResult]):-
   		append(Tail, List2, TailResult).

  divide([], _, [], []):-!.
	divide([Head|Tail], Pivot, [Head|GreaterList], SmallerList):-Head >= Pivot,
			!,
  		divide(Tail, Pivot, GreaterList, SmallerList).
	divide([Head|Tail], Pivot, GreaterList, [Head|SmallerList]):-
  		divide(Tail, Pivot, GreaterList, SmallerList).

	qsort([], []).
	qsort([Elem], [Elem]).
	qsort([Pivot|Tail], SortedList):-
  		divide(Tail, Pivot, GreaterList, SmallerList),
  		qsort(GreaterList, SortedGreaterList),
  		qsort(SmallerList, SortedSmallerList),!,
  		append(SortedSmallerList, [Pivot|SortedGreaterList], SortedList).


goal

  qsort([3,4,2,1], X).
