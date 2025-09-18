ultimo([H|[]],H):- !.
ultimo([_|T],R):- 
            ultimo(T,R).