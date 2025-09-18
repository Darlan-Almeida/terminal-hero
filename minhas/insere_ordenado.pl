insere_ordenado([] , E , [E]).
insere_ordenado([H|T] , E , [H|R]):-
    H < E,
    insere_ordenado(T , E , R).

insere_ordenado([H|T] , E, [E,H|T]):-
    H >= E.