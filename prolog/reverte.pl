reverte([X],[X]).
reverte([X|Y],R):-
    reverte(Y,R1),
    append(R1,[X],R).