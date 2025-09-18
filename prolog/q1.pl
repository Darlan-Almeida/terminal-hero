repetidos([X|Y]):- member(X,Y), !.
repetidos([_|Y]):- repetidos(Y).

repetidos2([X|Y]):- member(X,Y),!; repetidos2(Y).