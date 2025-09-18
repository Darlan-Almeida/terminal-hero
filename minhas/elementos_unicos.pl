elementos_unicos(A , [] , A).
elementos_unicos([] , A , A).
elementos_unicos([] , [] , []).

elementos_unicos([H|T] , L , [H|R]):-
    delete([H|T], H , R1),
    delete(L , H, R2),
    elementos_unicos(R1, R2 , R).
    