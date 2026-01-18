# Prolog CSP Labs - Complete Study Guide

## 📚 Syntax & Basics

### Core Concepts
- **Fact:** `pere(samir, lilia).` → "Samir is father of Lilia" (database entry)
- **Rule:** `parent(X,Y) :- pere(X,Y).` → "X is parent of Y IF X is father of Y"
  - Read `:-` as "IS TRUE WHEN" or "IF"
  - Left side (head) is true when right side (body) is true
- **Query:** `?- parent(X, lilia).` → "Who is parent of Lilia?" → `X = samir`
- **Variables:** Uppercase (X, Y, List), unknowns to find
- **Atoms:** Lowercase (samir, lilia), concrete values

### List Notation
```prolog
[1, 2, 3]        % Normal list
[H|T]            % H=1, T=[2,3] (head and tail)
[X,Y|Rest]       % X=1, Y=2, Rest=[3]
[]               % Empty list (base case!)
[X]              % Single element, same as [X|[]]
```

**Mental Model:** Lists are chains: `[1|[2|[3|[]]]]`

### Key Operators
- **`=`** : Unification (pattern matching, BOTH ways)
  ```prolog
  [X, 2] = [1, Y]  % X=1, Y=2 (both constrained)
  ```
- **`is`** : Arithmetic evaluation (RIGHT side computed)
  ```prolog
  X is 2 + 3       % X = 5
  X = 2 + 3        % X = +(2,3) (structure, NOT 5!)
  ```
- **Comparison:** `<, >, =<, >=, =:=, =\=` (both sides evaluated)
- **`!`** : Cut - "Stop searching alternatives here"
  - Prevents backtracking to other clauses
  - Use after condition satisfied

### Execution Model
**Prolog searches for what makes statements TRUE:**
1. Try first matching clause
2. If fails, backtrack and try next clause
3. Recursion explores tree of possibilities
4. Cut `!` prunes branches

---

## 📝 List Predicates (Lab 1)

### Membership - Check if Element in List
```prolog
appartient(X, [X|_]).                    % Found in head
appartient(X, [_,Y|L]) :- appartient(X, [Y|L]).  % Check rest
```
**Logic:** "X belongs to list if X is head OR X belongs to tail"

**Example:** `appartient(2, [1,2,3])` → TRUE (finds 2 in tail)

---

### First & Last Element
```prolog
premier(X, [X|_]).     % First element is head

dernier(X, [X]).       % Last if single element
dernier(X, [_,Y|L]) :- dernier(X, [Y|L]).  % Skip head, recurse
```
**Why `[X]` not `[]`?** Empty list has no last element!

**Example:** `dernier(X, [1,2,3])` → `X = 3`

---

### Length - Count Elements
```prolog
longueur([], 0).                           % Empty = 0
longueur([_|L], K) :- longueur(L, K2), K is K2+1.
```
**Logic:** "Length = 1 + length of tail"

**Trace:**
```prolog
longueur([a,b,c], K)
→ longueur([b,c], K2), K is K2+1
  → longueur([c], K3), K2 is K3+1
    → longueur([], K4), K3 is K4+1
      → K4=0, so K3=1, K2=2, K=3
```

---

### Sum - Add All Numbers
```prolog
somme([X], X).                              % Single element
somme([X,Y|L], S) :- somme([Y|L], S2), S is S2+X.
```
**Logic:** "Sum = first element + sum of rest"

**Example:** `somme([1,2,3], S)` → `S = 6`

---

### Concatenate - Join Two Lists
```prolog
concat([], L2, L2).                         % Empty + L2 = L2
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).
```
**Logic:** "Move elements from L1 to result one by one"

**Trace:**
```prolog
concat([1,2], [3,4], R)
→ R = [1|R2], concat([2], [3,4], R2)
  → R2 = [2|R3], concat([], [3,4], R3)
    → R3 = [3,4]
  → R2 = [2,3,4]
→ R = [1,2,3,4]
```

---

### Substitute - Replace All Occurrences
```prolog
substitue(_, _, [], []).                    % Empty stays empty
substitue(X, Y, [X|In], [Y|Out]) :- !, substitue(X, Y, In, Out).  % Replace
substitue(X, Y, [Z|In], [Z|Out]) :- substitue(X, Y, In, Out).     % Keep
```
**Cut `!` crucial:** Without it, would also try third clause after second

**Example:** `substitue(a, x, [a,b,a], R)` → `R = [x,b,x]`

---

### Remove K-th Element
```prolog
supprimerK(1, [_|L], L).                    % Remove first (position 1)
supprimerK(K, [X,Y|L1], [X|L2]) :- K2 is K-1, supprimerK(K2, [Y|L1], L2).
```
**Logic:** Count down K while keeping non-removed elements

**Example:** `supprimerK(2, [a,b,c], R)` → `R = [a,c]`

---

### Check Even Length
```prolog
npair([]).                % 0 elements = even
npair([_,_|L]) :- npair(L).  % Remove 2, check rest
```
**Clever:** Removes pairs of elements, succeeds if exactly exhausted

**Example:** `npair([1,2,3,4])` → TRUE, `npair([1,2,3])` → FALSE

---

## 🔄 Sorting Algorithms (Lab 1)

### Selection Sort - Find Min, Remove, Recurse
```prolog
tri_sel([X], [X]).
tri_sel([X,Y|L1], [Z|L2]) :- 
    min([X,Y|L1], Z),           % Find minimum
    supp_occ_1(Z, [X,Y|L1], L3), % Remove it
    tri_sel(L3, L2).            % Sort rest

min([X], X).
min([X,Y|L1], Z) :- min([Y|L1], Z), Z < X, !.  % Z is min of tail AND < X
min([X,_|_], X).                                % Else X is min

supp_occ_1(X, [X|L], L) :- !.                   % Found, remove
supp_occ_1(X, [Y|L1], [Y|L2]) :- supp_occ_1(X, L1, L2).  % Skip
```

**Strategy:** 
1. Find smallest element
2. Put it first in result
3. Sort remaining elements

**Example:** `tri_sel([3,1,2], R)`
- min=1 → R=[1|R2], tri_sel([3,2], R2)
- min=2 → R2=[2|R3], tri_sel([3], R3)
- R3=[3] → R=[1,2,3]

---

### Insertion Sort - Sort Tail, Insert Head
```prolog
tri_ins([X], [X]) :- !.
tri_ins([X|L1], L2) :- 
    tri_ins(L1, L3),      % Sort tail FIRST
    inserer(X, L3, L2).   % Insert head into sorted tail

inserer(X, [], [X]).
inserer(X, [Y|L1], [X,Y|L1]) :- X =< Y, !.  % Insert before Y
inserer(X, [Y|L1], [Y|L2]) :- inserer(X, L1, L2).  % Insert later
```

**Strategy:**
1. Recursively sort tail
2. Insert head into correct position

**Example:** `tri_ins([3,1,2], R)`
- tri_ins([1,2], L3) → inserer(3, L3, R)
  - tri_ins([2], L4) → inserer(1, L4, L3)
    - L4=[2], inserer(1, [2], L3) → L3=[1,2]
  - inserer(3, [1,2], R) → R=[1,2,3]

---

### Merge Sort (Fusion)
```prolog
fusion([], L, L) :- !.
fusion(L, [], L).
fusion([X|L1], [Y|L2], [X|L3]) :- X < Y, !, fusion(L1, [Y|L2], L3).
fusion(L1, [Y|L2], [Y|L3]) :- fusion(L1, L2, L3).
```

**Strategy:** Take smaller head, recurse

**Example:** `fusion([1,3], [2,4], R)`
- 1<2 → R=[1|R2], fusion([3], [2,4], R2)
- 3>2 → R2=[2|R3], fusion([3], [4], R3)
- 3<4 → R3=[3|R4], fusion([], [4], R4)
- R4=[4] → R=[1,2,3,4]

---

## 🧩 CLP (Constraint Logic Programming)

### Setup & Core Concepts
```prolog
:-use_module(library('clp/bounds')).  % Import CLP library
```

**Philosophy:** DECLARE constraints, let solver FIND solutions

### Domain Declaration
```prolog
Vars in Low..High     % All variables in range
X in 1..10           % X can be 1,2,3,...,10
[A,B,C] in 0..5      % A,B,C each in {0,1,2,3,4,5}
```

### Constraint Operators
```prolog
X #= Y + 2          % X equals Y+2 (constraint, not assignment!)
X #\= Y             % X not equal Y
X #< Y              % X less than Y
X #> Y              % X greater than Y
X #=< Y             % X less or equal Y
X #>= Y             % X greater or equal Y
```

**Key Difference from `is`:**
- `X is Y+2` → Evaluates RIGHT, assigns to LEFT
- `X #= Y+2` → Constraint stored, both X and Y constrained

### All Different
```prolog
all_different([X,Y,Z])  % X≠Y, X≠Z, Y≠Z (all pairwise different)
```

### Labeling - Actually Find Values
```prolog
label(Vars)  % Search for concrete values satisfying all constraints
```

**CLP Workflow:**
1. Declare domains: `Vars in 1..N`
2. Add constraints: `X #\= Y`, `all_different(...)`
3. Solve: `label(Vars)`

---

## ♛ N-Queens Problem (Lab 2)

### Problem Definition
**Goal:** Place N queens on N×N chessboard so none attack each other

**Constraints:**
- One queen per row (implicit - list has N elements)
- One queen per column (all_different)
- No diagonal attacks (custom constraint)

### Solution Representation
```prolog
Vars = [C1, C2, C3, ..., CN]
```
- **Index i** = Row number (1 to N)
- **Ci** = Column where queen in row i is placed

**Example:** N=4, Vars=[2,4,1,3]
```
Row 1: . Q . .  (queen in col 2)
Row 2: . . . Q  (queen in col 4)
Row 3: Q . . .  (queen in col 1)
Row 4: . . Q .  (queen in col 3)
```

### Complete Code
```prolog
n_reines(Vars, N) :-
    length(Vars, N),        % N variables (rows)
    Vars in 1..N,           % Each in columns 1..N
    all_different(Vars),    % All different columns
    cont_diag(Vars, 1),     % No diagonal attacks
    label(Vars).            % Find solution

cont_diag([_], _) :- !.     % Base: single queen, no conflicts
cont_diag([XI|Vars], I) :-
    Iplus1 is I + 1,
    distribuer(XI, Vars, I, Iplus1),  % Check XI vs all following
    cont_diag(Vars, Iplus1).          % Recurse

distribuer(_, [], _, _).    % Base: no more queens
distribuer(XI, [XJ|Vars], I, J) :-
    XI - XJ #\= I - J,      % Not on / diagonal
    XI - XJ #\= J - I,      % Not on \ diagonal
    Jplus1 is J + 1,
    distribuer(XI, Vars, I, Jplus1).
```

### Diagonal Attack Logic
**Two queens at (I, CI) and (J, CJ) attack diagonally if:**

1. **`/` diagonal (↗):** Same slope +1
   - Slope = (CJ-CI)/(J-I) = 1
   - CI - I = CJ - J
   - **Rearranged:** CI - CJ = I - J ❌

2. **`\` diagonal (↘):** Same slope -1
   - Slope = (CJ-CI)/(J-I) = -1
   - CI + I = CJ + J
   - **Rearranged:** CI - CJ = J - I ❌

**We forbid both:** `XI - XJ #\= I - J` AND `XI - XJ #\= J - I`

### Example Execution
```prolog
?- n_reines(Vars, 4).
Vars = [2, 4, 1, 3]  % First solution
Vars = [3, 1, 4, 2]  % Backtracking finds more
```

**Trace (simplified):**
1. Setup: Vars=[C1,C2,C3,C4], each in {1,2,3,4}, all_different
2. Constraints: C1-C2≠-1, C1-C2≠1, C1-C3≠-2, ... (15 diagonal constraints)
3. label(Vars): Solver finds C1=2, C2=4, C3=1, C4=3

---

## 🔢 Sudoku (Lab 3)

### Problem Definition
**Goal:** Fill 9×9 grid so each row, column, and 3×3 box has 1-9 exactly once

### Grid Representation
**Flat list of 81 elements (row-major order):**
```
Index: 0  1  2  3  4  5  6  7  8  9  10 ...
Row 1: [X1,X2,X3,X4,X5,X6,X7,X8,X9,
Row 2:  X10,X11,X12,X13,X14,X15,X16,X17,X18,
...
```

**Index formula:** Position = `Row*9 + Col` (0-indexed)

### Complete Code
```prolog
sudoku(Vars) :-
    length(Vars, 81),           % 9×9 grid
    Vars in 1..9,               % Each cell 1-9
    cont_lignes(Vars),          % Row constraints
    cont_colonnes(Vars, 1),     % Column constraints
    cont_carres(Vars, 1),       % 3×3 box constraints
    label(Vars),                % Solve
    afficher(Vars).             % Display

% Row constraints: every 9 consecutive elements all_different
cont_lignes([]).
cont_lignes(Vars) :- 
    length(L1, 9),              % Take first 9
    append(L1, L2, Vars),       % Split: L1 + L2 = Vars
    all_different(L1),          % Row constraint
    cont_lignes(L2).            % Recurse on rest

% Column constraints: extract column I, check all_different
cont_colonnes(_, 10) :- !.      % Columns 1-9 done
cont_colonnes(Vars, I) :- 
    extraire_colonne(Vars, I, Colonne),
    all_different(Colonne),
    Iplus1 is I + 1,
    cont_colonnes(Vars, Iplus1).

% Box constraints: extract 3×3 box I, check all_different
cont_carres(_, 10) :- !.        % Boxes 1-9 done
cont_carres(Vars, I) :- 
    extraire_carre(Vars, I, Carre),
    all_different(Carre),
    Iplus1 is I + 1,
    cont_carres(Vars, Iplus1).

% Display: print 9 rows
afficher([]) :- !.
afficher(Vars) :- 
    length(L1, 9),
    append(L1, L2, Vars),
    write(L1), nl,
    afficher(L2).
```

### Extracting Columns
**Column I consists of:** elements at positions I-1, I+8, I+17, ..., I+71
```prolog
extraire_colonne(Vars, Col, [E1,E2,E3,E4,E5,E6,E7,E8,E9]) :-
    % Calculate indices: Col-1, Col+8, Col+17, ...
    % Extract those elements from Vars
```

### Extracting 3×3 Boxes
**Boxes numbered 1-9 (row-major):**
```
Box 1 | Box 2 | Box 3
Box 4 | Box 5 | Box 6
Box 7 | Box 8 | Box 9
```

**Starting index of box I:**
- Row of box: `(I-1) div 3` → multiply by 27
- Column of box: `(I-1) mod 3` → multiply by 3
- Formula: `Start = ((I-1) div 3)*27 + ((I-1) mod 3)*3`

**Elements in box:** Start, Start+1, Start+2, Start+9, Start+10, Start+11, Start+18, Start+19, Start+20

### Example
```prolog
?- sudoku(Vars).
% Vars with some values pre-filled (0 for unknown)
% Solver fills remaining cells
% Displays:
[5,3,4,6,7,8,9,1,2]
[6,7,2,1,9,5,3,4,8]
...
```

---

## 🎨 Graph Coloring (Exam 2024/2025)

### Problem Definition
**Goal:** Color graph vertices so adjacent vertices have different colors

**Input:**
- **N:** Number of vertices
- **P:** Number of colors available (1 to P)
- **M:** Adjacency matrix (flattened upper triangle)
- **Vars:** Output color assignment

### Adjacency Matrix Format
**Upper triangle only (since graph undirected):**
```
Graph with 4 vertices:
     1   2   3   4
1  [ -   a   b   c ]
2  [ -   -   d   e ]
3  [ -   -   -   f ]
4  [ -   -   -   - ]

Flattened M = [a, b, c, d, e, f]
              ↑  ↑  ↑  ↑  ↑  ↑
            (1,2)(1,3)(1,4)(2,3)(2,4)(3,4)
```

**Values:** `1` = edge exists, `0` = no edge

### Complete Code
```prolog
coloriage(N, P, M, Vars) :-
    length(Vars, N),            % N vertices
    Vars in 1..P,               % Colors 1 to P
    ext_cont(1, 2, N, Vars, M), % Extract edge constraints
    label(Vars),                % Find coloring
    write('Solution = '),
    writeln(Vars).

% Extract constraints from adjacency matrix
ext_cont(N, _, N, _, _) :- !.   % Done (I reached N)

ext_cont(I, J, N, Vars, M) :-   % J > N: move to next row
    J > N, !,
    Iplus1 is I + 1,
    Iplus2 is I + 2,            % Next row starts at I+2 (since I<J)
    ext_cont(Iplus1, Iplus2, N, Vars, M).

ext_cont(I, J, N, Vars, [0|M]) :- % No edge: skip
    !,
    Jplus1 is J + 1,
    ext_cont(I, Jplus1, N, Vars, M).

ext_cont(I, J, N, Vars, [_|M]) :- % Edge exists: add constraint
    % Extract variable at position I
    Imoins1 is I - 1,
    length(Vars1, Imoins1),
    append(Vars1, [XI|_], Vars),
    % Extract variable at position J
    Jmoins1 is J - 1,
    length(Vars2, Jmoins1),
    append(Vars2, [XJ|_], Vars),
    % Add constraint
    XI #\= XJ,                  % Different colors!
    Jplus1 is J + 1,
    ext_cont(I, Jplus1, N, Vars, M).
```

### How Variable Extraction Works
```prolog
% Get Vars[I] using append trick:
Imoins1 is I - 1,
length(Prefix, Imoins1),        % Create dummy list of I-1 elements
append(Prefix, [XI|_], Vars),   % Split: Prefix + [XI|Rest] = Vars
```

**Example:** Extract Vars[3] from [C1,C2,C3,C4]
```prolog
I = 3, Imoins1 = 2
length(Prefix, 2) → Prefix = [_,_]
append([_,_], [XI|_], [C1,C2,C3,C4])
→ [C1,C2] ++ [C3|[C4]] = [C1,C2,C3,C4]
→ XI = C3 ✓
```

### Example Execution
```prolog
% Square graph: 1-2, 1-3, 2-4, 3-4
?- coloriage(4, 3, [1,1,0,0,1,1], Vars).
%                   ↑ ↑ ↑ ↑ ↑ ↑
%                   edges: (1,2)(1,3)(1,4)(2,3)(2,4)(3,4)

Constraints extracted:
- (1,2): edge → C1 #\= C2
- (1,3): edge → C1 #\= C3
- (1,4): no edge → skip
- (2,3): no edge → skip
- (2,4): edge → C2 #\= C4
- (3,4): edge → C3 #\= C4

Solution = [1,2,2,1]  % Vertices 1,4 color 1; vertices 2,3 color 2
```

### Traversal Pattern
**Nested loop over pairs (I,J) where I < J:**
```
I=1: J=2,3,4
I=2: J=3,4
I=3: J=4
Done
```

Each iteration consumes one element from M (the flat list).

---

## 📐 TCSP bd-Arc-Consistency (Exam 2022/2023)

### Problem Definition
**Goal:** Verify if a Temporal CSP is binarized-domains arc-consistent

**bd-Arc-Consistency condition:**
```
For all i≠j (i,j≠0): C0i ⊆ C0j ∘ Cji
```

**Meaning:** Direct path from X0 to Xi must be contained in indirect path via Xj

### Interval Operations

#### Inclusion
```prolog
inclus(_, [0]).                 % Empty ⊆ anything
inclus([A,B], [C,D]) :- A >= C, B =< D.
```
**Check:** `[A,B] ⊆ [C,D]` iff `C ≤ A ≤ B ≤ D`

**Example:** `inclus([2,5], [1,7])` → TRUE

---

#### Transpose
```prolog
transposee([0], [0]).
transposee([A,B], [C,D]) :- C is -B, D is -A.
```
**Formula:** `[A,B]^t = [-B,-A]`

**Logic:** If `(Xj - Xi) ∈ [A,B]`, then `(Xi - Xj) ∈ [-B,-A]`

**Example:** `transposee([2,5], T)` → `T = [-5,-2]`

---

#### Intersection
```prolog
inters([0], A, A).
inters(A, [0], A).
inters([A,B], [C,D], [E,F]) :- max(A,C,E), min(B,D,F).
```
**Formula:** `[A,B] ∩ [C,D] = [max(A,C), min(B,D)]`

**Example:** `inters([2,7], [5,9], R)` → `R = [5,7]`

**Empty result:** If `max > min`, intersection is empty (e.g., `[1,3] ∩ [5,7] = [5,3]` invalid)

---

#### Composition
```prolog
comp([0], _, [0]).
comp(_, [0], [0]).
comp([A,B], [C,D], [E,F]) :- E is A+C, F is B+D.
```
**Formula:** `[A,B] ∘ [C,D] = [A+C, B+D]`

**Logic:** If `X0→Xj ∈ [A,B]` and `Xj→Xi ∈ [C,D]`, then `X0→Xi ∈ [A+C, B+D]`

**Example:** `comp([1,3], [5,7], R)` → `R = [6,10]`

---

### Matrix Representation
```prolog
Mp = [L0, L1, L2, ..., LN]
```

**L0 (Row 0):** Binarized domains `[C00, C01, C02, ..., C0N]`
- C0i = domain of Xi relative to X0

**Li (Row i):** Constraints from Xi `[Ci0, Ci1, Ci2, ..., CiN]`
- Cij represents `(Xj - Xi) ∈ Cij`

**Each entry:** `[a,b]` (interval) or `[0]` (no constraint/empty)

### Example Matrix
```prolog
TCSP: X={X0,X1,X2}, C={(X1-X0)∈[0,1], (X2-X0)∈[2,4], (X2-X1)∈[2,3]}

     X0        X1        X2
X0 [[0,0],   [0,1],    [2,4]]     ← L0 (binarized domains)
X1 [[-1,0],  [0,0],    [2,3]]     ← L1
X2 [[-4,-2], [-3,-2],  [0,0]]     ← L2

Reading:
- L0[1] = [0,1]: C01 (X1 is 0-1 after X0)
- L0[2] = [2,4]: C02 (X2 is 2-4 after X0)
- L1[2] = [2,3]: C12 (X2 is 2-3 after X1)
- L1[0] = [-1,0]: C10 = transpose([0,1])
- L2[1] = [-3,-2]: C21 = transpose([2,3])
```

---

### Main Verification Code
```prolog
bdAC([L0|M]) :- verifier(L0, M, 1).

verifier(_, [], _) :- 
    write('bd-consistance d arc verifiee'), !.

verifier(L0, [LJ|M], J) :-
    % Extract L0[J] (binarized domain of Xj)
    length(Gauche, J),
    append(Gauche, [ZeroJ|_], L0),
    % Check all i vs j
    verifier2(ZeroJ, L0, LJ, J, 0),
    Jplus1 is J + 1,
    verifier(L0, M, Jplus1).

verifier2(_, [], [], _, _) :- !.
verifier2(ZeroJ, [ZeroI|L0], [JI|LJ], J, I) :-
    comp(ZeroJ, JI, Comp),      % ZeroJ ∘ JI
    inclus(ZeroI, Comp),        % Check: ZeroI ⊆ Comp
    !,
    Iplus1 is I + 1,
    verifier2(ZeroJ, L0, LJ, J, Iplus1).

verifier2(ZeroJ, [ZeroI|_], [JI|_], J, I) :-
    writeln('bd-consistance d arc non verifiee...'),
    write('I = '), write(I), write(' ZeroI = '), writeln(ZeroI),
    write('J = '), write(J), write(' ZeroJ = '), writeln(ZeroJ),
    write(' JI = '), writeln(JI),
    fail.
```

### What It Checks
**For each pair (I, J) where I≠J and I,J≠0:**
```
C0i ⊆ C0j ∘ Cji
```

Where:
- **ZeroI = C0i:** Binarized domain of Xi
- **ZeroJ = C0j:** Binarized domain of Xj  
- **JI = Cji:** Constraint from Xj to Xi

**Intuition:** "Direct distance X0→Xi must fit within path X0→Xj→Xi"

---

### Execution Trace Example
```prolog
?- bdAC([[[0,0],[0,1],[2,4]],
         [[-1,0],[0,0],[2,3]],[[-4,-2],[-3,-2],[0,0]]]).
```

**Check J=1 (variable X1):**
- ZeroJ = L0[1] = [0,1]

**I=0:**
```prolog
ZeroI = L0[0] = [0,0]
JI = L1[0] = [-1,0]
Comp = [0,1] ∘ [-1,0] = [-1,1]
inclus([0,0], [-1,1])? → 0≥-1 ✓, 0≤1 ✓ → TRUE ✓
```

**I=2:**
```prolog
ZeroI = L0[2] = [2,4]
JI = L1[2] = [2,3]
Comp = [0,1] ∘ [2,3] = [2,4]
inclus([2,4], [2,4])? → 2≥2 ✓, 4≤4 ✓ → TRUE ✓
```

**Check J=2:** (similar process)

**Result:** All checks pass → "bd-consistance d arc verifiee"

---

### Why This Matters
**For Simple Temporal Problems (STPs):**
- bd-Arc-Consistency **decides consistency** (polynomial time!)
- Tightens domains to minimal values
- If bdAC passes, problem has solution
- If fails, no solution exists

---

## 🎯 Core Recursive Patterns

### Pattern 1: Process Each Element (Tail Recursion)
```prolog
base([]).
process([H|T]) :- 
    do_something(H),    % Process head
    process(T).         % Recurse on tail
```
**Example:** `cont_lignes`, `cont_colonnes` in Sudoku

---

### Pattern 2: Build Result List
```prolog
base([], []).
process([H|T], [R|RList]) :- 
    transform(H, R),         % Transform head
    process(T, RList).       % Build rest of result
```
**Example:** `substitue`, `concat`

---

### Pattern 3: Conditional with Cut
```prolog
rule(X, result1) :- condition(X), !.
rule(X, result2).
```
**Example:** `min`, `inserer`

**Why cut?** Prevents backtracking to second clause once condition satisfied

---

### Pattern 4: Extract & Recurse
```prolog
process(List) :-
    extract_part(List, Part, Rest),
    check(Part),
    process(Rest).
```
**Example:** `cont_lignes` extracts 9 elements, checks all_different, recurses

---

## 📊 Comparison Table

| Concept | Prolog | CLP |
|---------|--------|-----|
| **Variables** | Unknowns to unify | Unknowns with domains |
| **Constraints** | Logical rules | Arithmetic/logic constraints |
| **Solving** | Pattern matching + backtracking | Constraint propagation + search |
| **Example** | `X = 5` (unification) | `X #> 3, X #< 7` (constraints) |
| **When to use** | Logic puzzles, lists | Optimization, scheduling |

---

## 🔑 Key Operators Reference

| Operator | Type | Meaning | Example |
|----------|------|---------|---------|
| `=` | Unification | Pattern match (both ways) | `[X,2] = [1,Y]` → X=1, Y=2 |
| `is` | Arithmetic | Evaluate right, assign left | `X is 2+3` → X=5 |
| `<, >, =<, >=` | Comparison | Arithmetic comparison | `3 < 5` → TRUE |
| `=:=, =\=` | Comparison | Arithmetic equality/inequality | `2+3 =:= 5` → TRUE |
| `!` | Control | Cut (prevent backtracking) | After condition met |
| `#=, #\=` | CLP | Equality/inequality constraint | `X #\= Y` |
| `#<, #>, #=<, #>=` | CLP | Ordering constraint | `X #< Y+2` |
| `in` | CLP | Domain declaration | `X in 1..10` |
| `all_different` | CLP | All pairwise different | `all_different([X,Y,Z])` |
| `label` | CLP | Find concrete values | `label([X,Y])` |

---

## 💡 Common Pitfalls & Tips

### 1. `=` vs `is`
```prolog
X = 2 + 3      % X = +(2,3) structure, NOT 5!
X is 2 + 3     % X = 5 ✓
```

### 2. Base Case Must Come First
```prolog
% WRONG:
process([H|T]) :- process(T).
process([]).

% RIGHT:
process([]).
process([H|T]) :- process(T).
```

### 3. Cut Placement
```prolog
% Put cut AFTER condition checked, BEFORE recursive call
min([X,Y|L], Z) :- min([Y|L], Z), Z < X, !.  % ✓ Correct
min([X,Y|L], Z) :- !, min([Y|L], Z), Z < X.  % ✗ Wrong
```

### 4. Empty List vs Single Element
```prolog
dernier(X, [X]).      % ✓ Last of [X] is X
dernier(X, []).       % ✗ No last element in []
```

### 5. CLP: Declare Domain Before Constraints
```prolog
% WRONG:
X #< Y, X in 1..10.

% RIGHT:
X in 1..10, Y in 1..10, X #< Y.
```

### 6. Extracting List Elements
```prolog
% Extract element at position I (1-indexed):
Imoins1 is I - 1,
length(Prefix, Imoins1),
append(Prefix, [Element|_], List)
```







