digito(0).
digito(1).

binario(N):-
    digito(X),
    digito(Y),
    digito(Z),
    atomic_list_concat([X, Y, Z],'',N).