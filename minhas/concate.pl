concate(L1 , L2 , L3 , R):- 
    append(L1 , L2 , S),
    append(S , L3 , R).

concat([] , X , X).
concat([X|Y] , L , [X|Z]):- concat(Y , L , Z).