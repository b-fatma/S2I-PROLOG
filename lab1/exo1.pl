appartient(X, [X|_]).
appartient(X, [_ , Y|L]) :- appartient(X, [Y|L]).

premier(X, [X|_]).

dernier(X, [X]) :- X.
dernier(X, [_, Y|L]) :- dernier(X, [Y|L]).

avantdernier(X, [X, _]).
avantdernier(X, [_, Y, Z|L]) :- avantdernier(X, [Y, Z|L]).

supprimerK(1, [_|L], L).
supprimerK(K, [X, Y|L1], [X|L2]) :- K2 is K-1, 
    supprimerK(K2, [Y|L1], L2).

substitue(_, _, [], []).
substitue(X, Y, [X|Lin], [Y|Lout]) :- !,
    substitue(X, Y, Lin, Lout).
substitue(X, Y, [Z|Lin], [Z|Lout]) :- substitue(X, Y, Lin, Lout).

longueur([], 0).
longueur([_|L], K) :- longueur(L, K2),
    K is K2 + 1.

somme([X], X).
somme([X, Y|L], S) :- somme([Y|L], S2),
    S is S2 + X.

affiche1([]).
affiche1([X|L]) :- write(X),
    nl,
    affiche1(L).

affiche2([]).
affiche2([X|L]) :- affiche2(L),
    nl,
    write(X).

npair([]).
npair([_, _|L]) :- npair(L).

present2fois(X, [X|L]) :- !,
    appartient(X, L).
present2fois(X, [Y|L]) :- present2fois(X, L).

concat([], L2, L2).
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).

palindrome([X]).
palindrome([]).
palindrome([Y, L, Y]) :- palindrome(L).