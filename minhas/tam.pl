tam([], 0).
tam([_|Y] , R):- 
    tam(Y , R1),
    R is R1 + 1.