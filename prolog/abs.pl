abs(N,R):- (N < 0 -> R is -N; R = N).

converte([],[]):- !.
converte([H1|T1],[H2|T2]):-
            abs(H1,H2),
            converte(T1,T2).