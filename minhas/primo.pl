% Caso em que encontrou divisor
n_primo(N, D) :-
    D > 1,
    N mod D =:= 0.

% Caso recursivo: ainda não encontrou divisor
n_primo(N, D) :-
    D > 1,
    D1 is D - 1,
    n_primo(N, D1).

primo(N):- \+ n_primo(N , N-1).