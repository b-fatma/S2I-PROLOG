# Prolog CSP Labs - Cheatsheet

## Syntax & Basics
- **Fact:** `pere(samir, lilia).`
- **Rule:** `parent(X,Y) :- pere(X,Y).`
- **Query:** `?- parent(X, lilia).` → `X = samir`
- **Head|Tail:** `[H|T]` where H is first, T is rest
- **Unification:** `=` (structural)
- **Arithmetic:** `is` (evaluates expressions)
- **Cut:** `!` (prevents backtracking)

## List Predicates (Lab 1)

```prolog
% Membership
appartient(X, [X|_]).
appartient(X, [_,Y|L]) :- appartient(X, [Y|L]).

% First/Last
premier(X, [X|_]).
dernier(X, [X]).
dernier(X, [_,Y|L]) :- dernier(X, [Y|L]).

% Length
longueur([], 0).
longueur([_|L], K) :- longueur(L, K2), K is K2+1.

% Sum
somme([X], X).
somme([X,Y|L], S) :- somme([Y|L], S2), S is S2+X.

% Concatenate
concat([], L2, L2).
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).

% Substitute all occurrences
substitue(_, _, [], []).
substitue(X, Y, [X|In], [Y|Out]) :- !, substitue(X, Y, In, Out).
substitue(X, Y, [Z|In], [Z|Out]) :- substitue(X, Y, In, Out).

% Remove k-th element
supprimerK(1, [_|L], L).
supprimerK(K, [X,Y|L1], [X|L2]) :- K2 is K-1, supprimerK(K2, [Y|L1], L2).

% Check even length
npair([]).
npair([_,_|L]) :- npair(L).
```

**Key points:**
- `dernier` base case: `[X]` not `[]` (must have element)
- `longueur`/`somme` use `is` for result computation
- `substitue` cut prevents backtracking to other matching clauses

## Sorting (Lab 1)

### Merge Sort
```prolog
fusion([], L, L) :- !.
fusion(L, [], L).
fusion([X|L1], [Y|L2], [X|L3]) :- X < Y, !, fusion(L1, [Y|L2], L3).
fusion(L1, [Y|L2], [Y|L3]) :- fusion(L1, L2, L3).
```
Compare heads, take smaller, recurse. Cuts prevent redundant checks.

### Selection Sort
```prolog
tri_sel([X], [X]).
tri_sel([X,Y|L1], [Z|L2]) :- min([X,Y|L1], Z), 
  supp_occ_1(Z, [X,Y|L1], L3), tri_sel(L3, L2).

min([X], X).
min([X, Y|L1], Z) :- min([Y|L1], Z), Z < X, !.
min([X, _|_], X).

supp_occ_1(X, [X|L], L) :- !.
supp_occ_1(X, [Y|L1], [Y|L2]) :- supp_occ_1(X, L1, L2).
```
Find min → remove → recurse. Cut in `min` ensures first solution.

### Insertion Sort
```prolog
tri_ins([X], [X]) :- !.
tri_ins([X|L1], L2) :- tri_ins(L1, L3), inserer(X, L3, L2).

inserer(X, [], [X]).
inserer(X, [Y|L1], [X, Y|L1]) :- X =< Y, !.
inserer(X, [Y|L1], [Y|L2]) :- inserer(X, L1, L2).
```
Recurse first → insert in position. More efficient for partially sorted lists.

## CLP Basics

**Import:** `:-use_module(library('clp/bounds')).`

**Domain:** `Vars in Low..High`

**Constraint operators:** `#=`, `#<`, `#>`, `#=<`, `#>=`, `#\=`

**Solve:** `label(Vars)` assigns values satisfying constraints

## N-Queens (Lab 2)

```prolog
n_reines(Vars, N) :-
    length(Vars, N), Vars in 1..N, all_different(Vars),
    cont_diag(Vars, 1), label(Vars).

cont_diag([_], _) :- !.
cont_diag([XI|Vars], I) :-
    Iplus1 is I + 1, distribuer(XI, Vars, I, Iplus1),
    cont_diag(Vars, Iplus1).

distribuer(_, [], _, _).
distribuer(XI, [XJ|Vars], I, J) :-
    XI - XJ #\= I - J, XI - XJ #\= J - I,
    Jplus1 is J + 1, distribuer(XI, Vars, I, Jplus1).
```

**Constraints:**
- `all_different(Vars)` → no two queens same column
- `XI - XJ #\= I - J` → main diagonal constraint
- `XI - XJ #\= J - I` → anti-diagonal constraint
- `label(Vars)` → find solution

**Output format:** `[2, 4, 1, 3]` = queens at (1,2), (2,4), (3,1), (4,3)

## Sudoku (Lab 3)

```prolog
sudoku(Vars) :-
    length(Vars, 81), Vars in 1..9,
    cont_lignes(Vars), cont_colonnes(Vars, 1), cont_carres(Vars, 1),
    label(Vars), afficher(Vars).

cont_lignes([]).
cont_lignes(Vars) :- length(L1, 9), append(L1, L2, Vars),
    all_different(L1), cont_lignes(L2).

cont_colonnes(_, 10) :- !.
cont_colonnes(Vars, I) :- 
    extraire_colonne(Vars, I, Colonne), all_different(Colonne),
    Iplus1 is I + 1, cont_colonnes(Vars, Iplus1).

cont_carres(_, 10) :- !.
cont_carres(Vars, I) :- 
    extraire_carre(Vars, I, Carre), all_different(Carre),
    Iplus1 is I + 1, cont_carres(Vars, Iplus1).

afficher([]) :- !.
afficher(Vars) :- length(L1, 9), append(L1, L2, Vars),
    write(L1), nl, afficher(L2).
```

**Grid representation:** Flat list of 81 elements (row-major)

**Constraints:**
1. **Rows:** 9 groups of 9 consecutive elements, each `all_different`
2. **Columns:** Extract element at position `i + row*9` for all rows
3. **3x3 boxes:** Calculate starting index = `(I-1 div 3)*27 + (I-1 mod 3)*3`

**Index math:** position = `row*9 + col`

## Core Recursive Patterns

### Pattern 1: Accumulate (tail recursion)
```prolog
base([]).
process([H|T]) :- compute(H), process(T).
```

### Pattern 2: Build result
```prolog
base([], []).
process([H|T], [R|RList]) :- transform(H, R), process(T, RList).
```

### Pattern 3: Two clauses (condition + cut)
```prolog
rule(X, special) :- condition(X), !.
rule(X, default).
```

## Key Operators

| Op | Meaning |
|----|---------|
| `=` | Unification (structural) |
| `is` | Arithmetic evaluation |
| `>, <, =<, >=, =:=, =\=` | Comparison |
| `!` | Cut (prevent backtracking) |
| `#=, #<, #>, #=<, #>=, #\=` | CLP constraints |
| `in` | CLP domain declaration |
| `all_different` | CLP all-different constraint |
| `label` | CLP solver |

## Quick Notes

- **Base case first:** Termination condition prevents infinite recursion
- **List pattern `[H|T]`:** Most natural decomposition for recursion
- **Cut prevents:** Backtracking to alternative clauses, useful after condition checked
- **Arithmetic `is`:** Required for evaluation; `=` only unifies structurally
- **CLP workflow:** Declare domain → add constraints → label → solve
