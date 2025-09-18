perfeito(N):- perfeito(N,1,0).

perfeito(N,N,N):-!.
perfeito(N,D,S):-
            D < N,
            N mod D =:= 0,
            S1 is S + D,
            D1 is D + 1,
            perfeito(N,D1,S1).
perfeito(N,D,S):-
            D < N,
            N mod D =\= 0,
            D1 is D + 1,
            perfeito(N,D1,S).
