# Prolog CSP Labs - Complete Revision Guide & Cheatsheet

## Quick Start
```bash
swipl -s file.pl           % Run file
?- predicate(Args).        % Query in interactive mode
;                          % Get next solution
.                          % Accept current solution
```

## Syntax

**Fact:** `pere(samir, lilia).` | **Rule:** `parent(X,Y) :- pere(X,Y).` | **Query:** `?- parent(X, lilia).`

**Head|Tail:** `[H|T]` | **Unify:** `=` | **Arithmetic:** `is`

---

# Lab 1: List Operations & Sorting

## Core Patterns

### Pattern 1: Recursive List Processing
```prolog
% Base case(s) + Recursive case
predicate([], base).
predicate([H|T], result) :- predicate(T, rec_result), combine(H, rec_result).
```

### Pattern 2: Two-Clause with Cut
Use when you want to match specific case then fallback:
```prolog
predicate(X, Y, special) :- condition(X), !, action1(Y).
predicate(X, Y, default) :- action2(X, Y).
```
The `!` prevents backtracking after the first clause succeeds.

### Pattern 3: Helper Predicates
Break complex logic into smaller predicates. Example: `tri_sel` uses `min` and `supp_occ_1`.

## List Predicates (Exo 1)

```prolog
% Check membership - traverses until found or reaches end
appartient(X, [X|_]).
appartient(X, [_,Y|L]) :- appartient(X, [Y|L]).

% First element
premier(X, [X|_]).

% Last element - recursively skip to end
dernier(X, [X]).
dernier(X, [_,Y|L]) :- dernier(X, [Y|L]).

% Length - count elements recursively
longueur([], 0).
longueur([_|L], K) :- longueur(L, K2), K is K2+1.

% Sum - add and recurse
somme([X], X).
somme([X,Y|L], S) :- somme([Y|L], S2), S is S2+X.

% Concatenate - build result left-to-right
concat([], L2, L2).
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).

% Substitute all X with Y - cut prevents multiple solutions
substitue(_, _, [], []).
substitue(X, Y, [X|In], [Y|Out]) :- !, substitue(X, Y, In, Out).
substitue(X, Y, [Z|In], [Z|Out]) :- substitue(X, Y, In, Out).

% Remove k-th element
supprimerK(1, [_|L], L).
supprimerK(K, [X,Y|L1], [X|L2]) :- K2 is K-1, supprimerK(K2, [Y|L1], L2).

% Check if even number of elements
npair([]).
npair([_,_|L]) :- npair(L).
```

**Exam Tips:**
- For `appartient`: Think of it as "match head OR recursively search tail"
- For `dernier`: Base case is `[X]` (single element), not `[]`
- For `longueur`/`somme`: Return value calculated in `is`, not directly in base case
- `substitue` with `!` avoids generating duplicate solutions

## Sorting Algorithms (Exo 3-5)

### Merge Sort (Exo 3)
```prolog
% Merge two sorted lists
fusion([], L, L) :- !.           % Empty first list
fusion(L, [], L).                 % Empty second list (no ! needed)
fusion([X|L1], [Y|L2], [X|L3]) :- X < Y, !, fusion(L1, [Y|L2], L3).
fusion(L1, [Y|L2], [Y|L3]) :- fusion(L1, L2, L3).  % X >= Y (implicit)
```
**Logic:** Compare heads, take smaller, recurse. Cuts optimize by preventing re-checking.

### Selection Sort (Exo 5)
```prolog
tri_sel([X], [X]).
tri_sel([X,Y|L1], [Z|L2]) :- 
    min([X,Y|L1], Z),             % Find minimum
    supp_occ_1(Z, [X,Y|L1], L3),  % Remove it
    tri_sel(L3, L2).              % Sort rest

min([X], X).
min([X, Y|L1], Z) :- min([Y|L1], Z), Z < X, !.
min([X, _|_], X).

supp_occ_1(X, [X|L], L) :- !.
supp_occ_1(X, [Y|L1], [Y|L2]) :- supp_occ_1(X, L1, L2).
```
**Logic:** Find min, remove first occurrence, recursively sort rest.

### Insertion Sort (Exo 5)
```prolog
tri_ins([X], [X]) :- !.
tri_ins([X|L1], L2) :- 
    tri_ins(L1, L3),              % Sort rest first
    inserer(X, L3, L2).           % Insert X in sorted position

inserer(X, [], [X]).
inserer(X, [Y|L1], [X, Y|L1]) :- X =< Y, !.
inserer(X, [Y|L1], [Y|L2]) :- inserer(X, L1, L2).
```
**Logic:** Sort recursively, then insert into correct position.

**Exam Tips:**
- Selection sort: Find min first, remove, recurse
- Insertion sort: Recurse first, then insert
- Both work but Insertion is often more efficient
- Remember `supp_occ_1` removes FIRST occurrence only

---

# Lab 2: Constraint Logic Programming - N-Queens

## CLP Essentials

**Import:** `:-use_module(library('clp/bounds')).`

**Declare domain:** `Vars in 1..N` (all variables in range 1 to N)

**Constraint operators:**
- `#>`, `#<`, `#>=`, `#=<`, `#=` : Relational constraints
- `#\=` : Not equal (constraint, not arithmetic)

**Solve:** `label(Vars)` assigns concrete values respecting all constraints

## N-Queens Problem (Lab 2)

```prolog
:-use_module(library('clp/bounds')).

% Place N queens on NxN board, no two attack each other
n_reines(Vars, N) :-
    length(Vars, N),              % N variables
    Vars in 1..N,                 % Each in range [1..N]
    all_different(Vars),          % Different columns (no duplicates)
    cont_diag(Vars, 1),           % Check diagonal constraints
    label(Vars).                  % Find solution

% Recursively check diagonals
cont_diag([_], _) :- !.
cont_diag([XI|Vars], I) :-
    Iplus1 is I + 1,
    distribuer(XI, Vars, I, Iplus1),
    cont_diag(Vars, Iplus1).

% XI at position I cannot be on same diagonal as other queens
distribuer(_, [], _, _).
distribuer(XI, [XJ|Vars], I, J) :-
    XI - XJ #\= I - J,            % Not on main diagonal
    XI - XJ #\= J - I,            % Not on anti-diagonal
    Jplus1 is J + 1,
    distribuer(XI, Vars, I, Jplus1).

% Usage: ?- n_reines(Vars, 4), write(Vars).
% Solution: [2, 4, 1, 3] means Q at (1,2), (2,4), (3,1), (4,3)
```

**Exam Tips:**
- `all_different(Vars)` ensures no two queens in same column
- Diagonal constraints: if queen at (i, xi) and (j, xj):
  - Not on same diagonal if `xi - xj ≠ i - j`
  - Not on same anti-diagonal if `xi - xj ≠ j - i`
- `label(Vars)` is the solver - it tries values until constraints satisfied
- `I` and `J` track row positions, values in `Vars` are column positions

---

# Lab 3: Constraint Logic Programming - Sudoku

## Sudoku Problem (Lab 3)

```prolog
:-use_module(library('clp/bounds')).

sudoku(Vars) :-
    length(Vars, 81),             % 81 cells total (9x9 grid as list)
    Vars in 1..9,                 % Each cell contains 1-9
    cont_lignes(Vars),            % All rows have different numbers
    cont_colonnes(Vars, 1),       % All columns have different numbers
    cont_carres(Vars, 1),         % All 3x3 boxes have different numbers
    label(Vars),                  % Find solution
    afficher(Vars).               % Display result

% Rows constraint - every 9 consecutive elements must be all_different
cont_lignes([]).
cont_lignes(Vars) :- 
    length(L1, 9), 
    append(L1, L2, Vars),         % Split into first row + rest
    all_different(L1),            % Row constraint
    cont_lignes(L2).              % Check remaining rows

% Columns constraint - extract column i from all 9 rows
cont_colonnes(_, 10) :- !.       % Done all 9 columns
cont_colonnes(Vars, I) :- 
    extraire_colonne(Vars, I, Colonne),
    all_different(Colonne),
    Iplus1 is I + 1,
    cont_colonnes(Vars, Iplus1).

extraire_colonne([], _, []) :- !.
extraire_colonne(Vars, I, [X|Colonne]) :- 
    length(L1, 9), 
    append(L1, L2, Vars),         % Get current row
    Imoins1 is I - 1,
    length(L3, Imoins1),
    append(L3, [X|_], L1),        % Extract column I from row
    extraire_colonne(L2, I, Colonne).

% 3x3 Box constraint - each of 9 boxes has all_different
cont_carres(_, 10) :- !.         % Done all 9 boxes
cont_carres(Vars, I) :- 
    extraire_carre(Vars, I, Carre),
    all_different(Carre),
    Iplus1 is I + 1,
    cont_carres(Vars, Iplus1).

extraire_carre(Vars, I, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    Row is (I - 1) div 3,         % Box row (0, 1, 2)
    Col is (I - 1) mod 3,         % Box column (0, 1, 2)
    Debut0 is Row * 27 + Col * 3, % Starting position
    length(L1, Debut0),
    append(L1, [X1, X2, X3|_], Vars),
    Debut1 is Debut0 + 9,
    length(L2, Debut1),
    append(L2, [X4, X5, X6|_], Vars),
    Debut2 is Debut1 + 9,
    length(L3, Debut2),
    append(L3, [X7, X8, X9|_], Vars).

% Display result as 9x9 grid
afficher([]) :- !.
afficher(Vars) :- 
    length(L1, 9),
    append(L1, L2, Vars),
    write(L1), nl, 
    afficher(L2).
```

**Exam Tips:**
- Grid represented as flat list of 81 elements (row-major)
- Rows: consecutive groups of 9
- Columns: every 9th element starting from position i
- 3x3 boxes: calculated using `Row = (I-1) div 3`, `Col = (I-1) mod 3`
- Index into flat list: `position = row*9 + col`

---

## General Exam Tips & Tricks

### 1. **Tail Recursion Pattern**
Always try to structure as:
- Base case: trivial condition (`[]`, `[X]`, or specific value)
- Recursive case: process head, recurse on tail
```prolog
process([]).                       % Base
process([H|T]) :- process(T).      % Recursive
```

### 2. **Cut (!) Usage**
Use `!` to:
- Prevent multiple solutions when you want exactly one: `max(X,Y,X) :- X >= Y, !.`
- Commit to a choice after condition verified: `substitue(X, Y, [X|In], [Y|Out]) :- !,`
- Optimize by avoiding re-checking: `fusion([], L, L) :- !.`

### 3. **Arithmetic vs Unification**
```prolog
X = 3 + 2.      % X unifies with term +(3,2), NOT 5
X is 3 + 2.     % X evaluates to 5
K is K2 + 1.    % Must use is for arithmetic
```

### 4. **List Matching**
- `[X|_]` - matches any list with at least one element, ignores rest
- `[X,Y|L]` - matches list with at least 2 elements
- `[X]` - matches list with exactly one element
- `[X|T]` - T is the tail (rest of list after X)

### 5. **Base Case Strategy**
- For operations on lists: base case on `[]` or single element `[X]`
- For operations on numbers: base case on 0 or 1
- For operations on structures: base case when structure is simplest

### 6. **CLP Pattern**
All constraint problems follow this structure:
```prolog
problem(Vars) :-
    declarations(Vars),   % length, domain (in)
    constraints(Vars),    % all_different, #\=, etc
    label(Vars),          % Solve
    display(Vars).        % Show result
```

### 7. **Debugging**
```prolog
?- trace.               % Step through execution
?- notrace.             % Turn off tracing
```
Watch variable bindings as Prolog searches through rules.

### 8. **Common Mistakes to Avoid**
- Forgetting base case → infinite recursion
- Using `=` instead of `is` → gets term instead of value
- Not preventing backtracking → get multiple solutions when you want one
- Wrong list pattern → doesn't match intended input
- Confusing row/column indices in grid problems

---

## Key Operators Summary

| Category | Operators |
|----------|-----------|
| **Unification** | `=` |
| **Arithmetic** | `is` |
| **Comparison** | `> < =< >= =:= =\=` |
| **CLP Domain** | `in`, `#=`, `#>`, `#<`, `#>=`, `#=<` |
| **CLP Constraint** | `all_different`, `#\=` |
| **CLP Solve** | `label` |
| **Control** | `!` (cut), `:- ` (rule), `,` (and), `;` (or in CLP) |


