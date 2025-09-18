media(N1,N2,N3,R):- M is (N1+N2+N3)/3, situacao(M,R).
situacao(M,'aprovado'):- M >= 7,!.
situacao(M,'reprovado'):- M < 4,!.
situacao(_,'final').