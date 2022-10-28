
zip([], _ , []).
zip([_|_] , [], []).
zip([X|Xs],[Y|Ys], [pair(X,Y) | RS]) :-
    zip(Xs,Ys,RS).


unzip([], [], []).
unzip([pair(X,Y)|RS], [W|WS], [Z|ZS]):-
    W is X
    Z is Y,
    unzip(RS,WS,ZS).