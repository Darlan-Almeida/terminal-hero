perfeito(X):- perfeito(X,1,0).

perfeito(X,X,X):-!.
perfeito(X,Y,R):-
            Y < X,
            X mod Y =:= 0,
            Y1 is Y + 1,
            R1 is R + Y,
            perfeito(X,Y1,R1).
perfeito(X,Y,R):-
            Y < X,
            X mod Y =\= 0,
            Y1 is Y + 1,
            perfeito(X,Y1,R).

perfect(X):-perfect(X,1,0).

