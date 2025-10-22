# Lab 1

## Exo 1

### Q1
```
appartient(X, [X|_]).
appartient(X, [_ , Y|L]) :- appartient(X, [Y|L]).
```
![alt text](exec/image.png)