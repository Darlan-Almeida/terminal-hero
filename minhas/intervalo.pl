intervalo(X , Y , R):-
    X =< Y + 1,
    writeln("impossivel"), !.

intervalo(X, X, []) :- !.
intervalo(X , Y , [X1]):-
    X > Y+1,
    X1 is X - 1,
    intervalo(X1 , Y , R).