:-use_module(library('clp/bounds')).

sudoku(Vars) :-
    length(Vars, 81),
    Vars in 1..9,
    cont_lignes(Vars),
    cont_colonnes(Vars, 1),
    cont_carres(Vars, 1),
    label(Vars),
    afficher(Vars).

cont_lignes([]).
cont_lignes(Vars) :- 
    length(Vars1, 9), 
    append(Vars1, Vars2, Vars),
    all_different(Vars1),
    cont_lignes(Vars2).

cont_colonnes(_, 10) :- !.
cont_colonnes(Vars, I) :- 
    extraire_colonne(Vars, I, Colonne),
    all_different(Colonne),
    Iplus1 is I + 1,
    cont_colonnes(Vars, Iplus1).

extraire_colonne([], _, []) :- !.
extraire_colonne(Vars, I, [X|Colonne]) :- 
    length(Vars1, 9), 
    append(Vars1, Vars2, Vars),
    Imoins1 is I - 1,
    length(Vars3, Imoins1),
    append(Vars3, [X|_], Vars1),
    extraire_colonne(Vars2, I, Colonne).

cont_carres(_, 10) :- !.
cont_carres(Vars, I) :- 
    extraire_carre(Vars, I, Carre),
    all_different(Carre),
    Iplus1 is I + 1,
    cont_carres(Vars, Iplus1).

extraire_carre(Vars, I, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    Row is (I - 1) div 3,
    Col is (I - 1) mod 3,
    Debut0 is Row * 27 + Col * 3,
    length(Vars1, Debut0),
    append(Vars1, [X1, X2, X3|_], Vars),
    Debut1 is Debut0 + 9,
    length(Vars2, Debut1),
    append(Vars2, [X4, X5, X6|_], Vars),
    Debut2 is Debut1 + 9,
    length(Vars3, Debut2),
    append(Vars3, [X7, X8, X9|_], Vars).

afficher([]) :- !.
afficher(Vars) :- length(Vars1, 9),
    append(Vars1, Vars2, Vars),
    write(Vars1),
    nl,
    afficher(Vars2).