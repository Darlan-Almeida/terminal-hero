substitui([] , _ , _ , []).
substitui([A] , A , B ,[B]).

substitui([H|T], A , B , [B|R]):-
    H == A,
    substitui(T , A , B , R).

substitui([H|T], A , B , [H|R]):-
    H \== A,
    substitui(T , A , B , R).