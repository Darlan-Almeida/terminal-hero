divide([] , _ , [] , []).
divide([N|T] , N , [N], T):- !.

divide([H|T] , N , [H|L] , L2):-
    divide(T , N , L , L2).