grades(N1 , N2 , N3 , R):- Mean is (N1+N2+N3)/3,
classify(Mean,R).

classify(M, "APROVADO"):- M >=7, !.
classify(M, "REPROVADO"):- M < 4, !.
classify(_, "Final").


grades_cond(N1 , N2 , N3 , R):- Mean is (N1+N2+N3)/3,
    (Mean >= 7 -> R = "APROVADO";
     Mean < 4 -> R = "REPROVADO";
     R = "FINAL"
    ).