sublista(L , L).
sublista([H|T] , [X|Y]):-
    H =:= X,
    sublista([H|T] , Y), !.
sublista([H|T] , [X|Y]):-
    H =\= X,
    sublista([H|T] , Y).