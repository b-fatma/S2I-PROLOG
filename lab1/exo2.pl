%Q1
homme(ali).
homme(hacene).
homme(hakim).
homme(mohamed).
homme(said).
homme(samir).

femme(djamila).
femme(fatma).
femme(houria).
femme(lilia).
femme(linda).

pere(mohamed, samir).
pere(samir, lilia).
pere(samir, said).
pere(said, hacene).
pere(said, linda).
pere(hakim, ali).

mere(fatma, samir).
mere(houria, lilia).
mere(houria, said).
mere(lilia, ali).
mere(djamila, hacene).
mere(djamila, linda).

%Q2
%homme(said).

%femme(ali).

%femme(X).

%homme(X).

%mere(djamila, linda).
%mere(djamila, ali).

%mere(X, samir).

%pere(samir, X).

%homme(X), pere(X, _).

%Q3
parent(X, Y) :- pere(X, Y).
parent(X, Y):- mere(X, Y).

fils(X, Y) :- homme(X),
    parent(Y, X).

fille(X, Y) :- femme(X),
    parent(Y, X).

enfant(X, Y) :- parent(Y, X).

grand_pere(X, Y) :- homme(X),
    pere(X, Z),
    parent(Z, Y).

grand_mere(X, Y) :- femme(X),
    mere(X, Z),
    parent(Z, Y).

%half brothers included
frere(X, Y) :- fils(X, Z),
    enfant(Y, Z).
%WO half brothers
frere2(X, Y) :- fils(X, P),
    fils(X, M),
    pere(P, Y),
    mere(M, Y).

soeur(X, Y) :- fille(X, Z),
    enfant(Y, Z).
soeur2(X, Y) :- fille(X, P),
    fille(X, M),
    pere(P, Y),
    mere(M, Y).

frere_ou_soeur(X, Y) :- enfant(X, Z),
    enfant(Y, Z).
frere_ou_soeur2(X, Y) :- enfant(X, P),
    enfant(X, M),
    pere(P, Y),
    mere(M, Y).

tante(X, Y) :- enfant(Y, Parent),
    soeur(Y, Parent).

