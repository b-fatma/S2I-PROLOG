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

substitue(X, Y, [X|Lin], [Y|Lout]) :- !, 
    substitute(X, Y, Lin, Lout).
substitue(X, Y, [Z|Lin], [Z|Lout]) :- substitute(X, Y, Lin, Lout).
