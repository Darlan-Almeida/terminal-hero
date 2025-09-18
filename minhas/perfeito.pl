perfeito(N , N , N):-!.

perfeito(N , X , R):-
    X < N,
    N mod X =:= 0,
    X1 is X + 1,
    R1 is R + X,
    perfeito(N , X1 , R1).
perfeito(N , X , R):-
    X < N,
    N mod X =\= 0,
    X1 is X + 1,
perfeito(N , X1 , R).


perfeito(N):- perfeito(N , 1 , 0).