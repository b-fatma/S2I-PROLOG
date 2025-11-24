supp_rep([], []).
supp_rep([X], [X]).
supp_rep([X, Y|L1], [X|L2]) :- supp_toutes(X, [Y|L1], Lout),
    supp_rep(Lout, L2).

supp_toutes(_, [], []).
supp_toutes(X, [X|L1], L2) :- !,
    supp_toutes(X, L1, L2).
supp_toutes(X, [Y|L1], [Y|L2]) :- supp_toutes(X, L1, L2).