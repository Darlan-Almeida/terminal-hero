distintos([H|T] , R):-
    distintos([H|T], 0 ,R).

distintos([], N , N).

distintos([H|T], N ,R):-
    delete(T, H , L),
    N1 is N + 1,
    distintos(L , N1 , R).