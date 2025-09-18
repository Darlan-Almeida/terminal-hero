limpa(_,[],[]):- !.
limpa(X,[X|T],R):- limpa(X,T,R).
limpa(X,[H|T],[H|R]):- 
            X \= H,
            limpa(X,T,R).