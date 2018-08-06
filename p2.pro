

perm([], []).
perm(Source, [Element|Tail]) :- member_list_exclude(Element, Source, SourceExcluded), 
             perm(SourceExcluded, Tail).

member_list_exclude(X, [X|L], L).
member_list_exclude(X, [Y|L], [Y|Ls]) :- member_list_exclude(X, L, Ls).