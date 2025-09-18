concat([],X,X).
concat([X|Y],L,[X|Z]):- concat(Y,L,Z).