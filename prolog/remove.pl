remove(N,[N|Y],Y):- !.
remove(N,[X|Y],[X|Y1]):- 
        remove(N,Y,Y1).