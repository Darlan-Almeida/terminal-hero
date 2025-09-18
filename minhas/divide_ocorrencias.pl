divide_ocor([] , _ , [] , []).

divide_ocor([N|T] , N , L , [N|R]):-
    divide_ocor(T , N , L , R).

divide_ocor([H|T] , N , [H|L] , R):-
    H \== N,
    divide_ocor(T , N , L , R).