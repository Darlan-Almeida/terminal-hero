soma(N,N,N).

soma(N , Y , R):- 
    Y < N,
    Y1 is Y + 1,
    soma(N , Y1 , R1),
    R is R1 + Y.

somatorio(N , R ):- soma(N , 1 , R).