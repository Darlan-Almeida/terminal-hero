contem(N , [H|T]):-
    N =:= H;
    contem(N , T).

repetidos([H|T]):-
    contem(H,T);
    repetidos(T).
