:- use_module(library(assoc)).

language(ut1,rust).
language(ut2,haskell).
language(ut3,prolog).
language(ut4,ruby).
language(ut5,javascript).

rule3(A,B,C,X) :- var(X), X is B*C/A.
rule3(A,B,X,D) :- var(X), X is D*A/B.
rule3(A,X,C,D) :- var(X), X is D*A/C.
rule3(X,B,C,D) :- var(X), X is B*C/D.
/*rel(+A,?B,-C)*/

factorial(0,F) :- F is 1.
factorial(X,F) :-
    var(F),
    X>0,
    F2 is X-1,
    factorial(F2,A),
    F is X * A.

isBoolean(true).
isBoolean(false).

truthTable(not(false), true).
truthTable(not(true), false).

truthTable(and(false, V), false) :- isBoolean(V).
truthTable(and(true,true), true).
truthTable(and(true,false), false).

truthTable(or(true,_), true).
truthTable(or(_,true), true).
truthTable(or(_,_), false).

truthTable(cond(true,true), true).
truthTable(cond(false,true), true).
truthTable(cond(_,_), false).

truthTable(bicond(true,true), true).
truthTable(bicond(false,false), true).
truthTable(bicond(_,_), false).

testAssoc(A) :- list_to_assoc(["t" - true, "f" - false], A ).


propEval(true, _, true).
propEval(not(P), A, R) :- 
    propEval(P, A, R2),
    truthTable(not(R2), R).

propEval(propVar(V),A,R):-