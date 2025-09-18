pai(carlos,joao).
pai(ana,marcos).
pai(jose,thiago).
mae(carlos,ana).
mae(ana,diana).
mae(jose,diana).
casal(thiago,diana).
casal(gabriel,ana).

padrasto(X,Y):-
        mae(X,M),
        casal(Y,M),
        pai(X,P),
        Y \= P.