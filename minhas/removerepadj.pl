remover([] , []).
remover([A] , [A]).


remover([H,H|T] , R):-
    remover([H|T], R).
    
remover([H,X|T] , [H|R]):-
    H =\= X,
    remover([X|T], R).
