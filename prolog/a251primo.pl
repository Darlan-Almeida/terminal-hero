primo(N):- nextDiv(N,2,R), N =:= R.
nextDiv(N,I,I):- N mod I =:= 0,!.
nextDiv(N,I,R):- 
    I2 is I + 1,
    nextDiv(N,I2,R).

primo(N):- nextDiv(N,2,R), N =:= R.
