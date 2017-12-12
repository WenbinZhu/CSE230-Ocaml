% fa13 %

link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

path_2(A, B) :- link(A, C), link(C, B).
path_3(A, B) :- path_2(A, C), link(C, B).
% case for N = 1
path_N(A, B, N) :- N = 1, link(A, B).
% case for N > 1
path_N(A, B, N) :- N > 1, M is N - 1, path_N(A, C, M), link(C, B).

path(A, B) :- path_helper(A, B, [A]).
path_helper(A, B, Seen) :- link(A, B), not(member(B, Seen)).
path_helper(A, B, Seen) :- 
    link(A, C),
    not(member(C, Seen)),
    path_helper(C, B, [C|Seen]).


% sp13 %

zip([], [], []).
zip([H1|T1], [H2|T2], [H3|T3]) :- H3 = [H1, H2], zip(T1, T2, T3).

part([], _, [], []).
part([H|T], P, [H|T1], R2) :- H =< P, part(T, P, T1, R2).
part([H|T], P, R1, [H|T2]) :- H > P, part(T, P, R1, T2).

qsort([],[]).
qsort([H|T], R) :- part(T, H, R1, R2), qsort(R1, RS1), qsort(R2, RS2), append(RS1, [H|RS2], R).


% wi13 %

remove_all(_, [], []).
remove_all(X, [H1|T1], L2) :- H1 = X, remove_all(X, T1, L2).
remove_all(X, [H1|T1], [H1|T2]) :- H1 \= X, remove_all(X, T1, T2).

remove_first(_, [], []).
remove_first(X, [H1|T1], L2) :- H1 = X, T1 = L2.
remove_first(X, [H1|T1], [H1|T2]) :- H1 \= X, remove_first(X, T1, T2).

prefix([], _).
prefix([H1|T1], [H1|T2]) :- prefix(T1, T2).

segment(A, B) :- prefix(A, B).
segment(A, [_|T2]) :- segment(A, T2).
