digito(0).
digito(1).

binario(N):- 
       digito(A),
       digito(B),
       digito(C),
       atomic_list_concat([A,B,C],'',N).

main:-
    binario(N),
    writeln(N),
    fail.

:- main.