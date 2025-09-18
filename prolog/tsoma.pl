soma(X,0):- X =< 0, !.
soma(X,R):-
        X > 0,
        X1 is X - 1,
        soma(X1,R1),
        R is R1 + X.