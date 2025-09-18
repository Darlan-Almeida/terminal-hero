remove([] , E, []).
remove([E] , E , []).

remove([H|T] , E , [H|R]):-
    H =\= E,
    remove(T , E , R).

remove([H|T] , E , R):-
    H =:= E,
    remove(T , E , R).