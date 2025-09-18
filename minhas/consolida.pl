consolida(L1 ,  L2 , L3, R):-
    append(L1 , L2 , R0),
    append(R0 , L3 , R).

gera_lista(X , X , [X]):- !.
gera_lista(X , Y , R):-
    X < Y,
    R = "N",!.

gera_lista(X , Y , [X|R]):-
    X1 is X - 1,
    gera_lista(X1 , Y , R).