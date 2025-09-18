soma(N,0):- N =< 0, !.
soma(N,R):- 
        N1 is N - 1,
        soma(N1,R1),
        R is R1 + N.