imprimir(0):- halt.
imprimir(N):-
    N < 10,
    N > 0,
    writeln(N),
    N2 is N - 1,
    imprimir(N2). 