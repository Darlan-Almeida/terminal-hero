eh_numero(N):-
    integer(N).



separa([] , [] , []).
separa([A] , [A] , []):-
    eh_numero(A).
separa([A] , [] , [A]):-
    \+ eh_numero(A).

separa([H|T] , [H|L1] , L2):-
    eh_numero(H),
    separa(T , L1 , L2).
separa([H|T] , L1 , [H|L2]):-
    \+ eh_numero(H),
    separa(T , L1 , L2).

