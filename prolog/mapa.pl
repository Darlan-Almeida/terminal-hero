cor(azul).
cor(verde).
cor(amarelo).
cor(vermelho).

mapa(A,B,C,D,E):-
    cor(A), cor(B), cor(C), cor(D), cor(E),
    A \= B, A \= C, A\= D,B \= C, B \= E, C \= D, C \= E, D \= E. 

main:- 
    mapa(A,B,C,D,E),
    write(A),write(' '),
    write(B),write(' '),
    write(C),write(' '),
    write(D),write(' '),
    writeln(E),
    fail.

:- main.