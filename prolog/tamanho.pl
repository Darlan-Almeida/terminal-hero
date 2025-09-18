len([],0):- !.
len([_|T],R):- 
            len(T,R1),
            R is R1 + 1.