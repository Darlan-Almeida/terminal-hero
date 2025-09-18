converte([], []).
converte([X|Y], [AX|R]) :-
    AX is abs(X),
    converte(Y, R).