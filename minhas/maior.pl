encontra_maior(H , [X|Y]):-
    H < X;
    encontra_maior(H , Y).

maior([H|T] , H):-
    \+ encontra_maior(H , T), !.

maior([_|T] , R):-
    maior(T , R).