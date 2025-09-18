divide(_,[],[],[]):- !.
divide(N,[X|Y],R1,R2):- 
        X =< N,
        divide(N,Y,R,R2),
        append([X],R,R1).
divide(N,[X|Y],R1,R2):- 
        X > N,
        divide(N,Y,R1,R),
        append([X],R,R2).
