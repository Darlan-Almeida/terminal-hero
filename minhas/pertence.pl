pertence([X|Y] , X):- !.
pertence([X|Y] , Z):- pertence(Y , Z).