pertence(X,[X|_]):-!.
pertence(X,[_|T]):- pertence(X,T).