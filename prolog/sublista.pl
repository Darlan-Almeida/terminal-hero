prefixo(X,Y):- append(X,_,Y).
sufixo(X,Y):- append(_,X,Y).

sublista(X,Y):- prefixo(Z,Y), sufixo(X,Z).