reverte([],[]):-!.
reverte([X|Y],R):-
        reverte(Y,R1),
        append(R1,[X],R).