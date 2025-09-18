ultimo([X] , X).
ultimo([X|Y] , R):-
    ultimo(Y , R).
