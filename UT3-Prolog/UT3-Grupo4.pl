:- use_module(library(assoc)).

% NOT
truthTable(not(true), false). 
truthTable(not(false), true).

% AND
truthTable(and(false,false), false). 
truthTable(and(true,false), false). 
truthTable(and(false,true), false). 
truthTable(and(true,true), true). 

% OR
truthTable(or(false,false), false).
truthTable(or(true,false), true).
truthTable(or(false,true), true).
truthTable(or(true,true), true).

% COND
truthTable(cond(true, false), false).
truthTable(cond(true, true), true).
truthTable(cond(false, false), true).
truthTable(cond(false, true), true).

% BICOND
truthTable(bicond(true, false), false).
truthTable(bicond(true, true), true).
truthTable(bicond(false, false), true).
truthTable(bicond(false, true), false).

% Ejercicio 2 

assign(A) :- list_to_assoc(["t" - true, "f" - false], A).
propEval(propVar(X), A, V) :- get_assoc(X, A, V).
propEval(not(X),A,R) :- propEval(X,A,R1), truthTable(not(R1),R). 
propEval(and(X,Y),A,R) :- propEval(X,A,R1), propEval(Y,A,R2), truthTable(and(R1,R2),R). 
propEval(or(X,Y),A,R) :- propEval(X,A,R1), propEval(Y,A,R2), truthTable(or(R1,R2),R). 
propEval(cond(X,Y),A,R) :- propEval(X,A,R1), propEval(Y,A,R2), truthTable(cond(R1,R2),R).
propEval(bicond(X,Y),A,R) :- propEval(X,A,R1), propEval(Y,A,R2), truthTable(bicond(R1,R2),R).



