tri_sel([X], [X]).
tri_sel([X, Y|L1], [Z|L2]) :- min([X, Y|L1], Z),
    supp_occ_1(Z, [X, Y|L1], L3),
    tri_sel(L3, L2).

min([X], X).
min([X, Y|L1], Z) :- min([Y|L1], Z),
    Z < X,
    !.
min([X, _|_], X).