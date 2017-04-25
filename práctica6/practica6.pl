longitud([],0).
longitud([_|H], N) :- longitud(H, N1), N is N1+1.


ultimo([X|[]],X). 
ultimo([_|H],X):- ultimo(H,X).

recorrei(X,X,0).
recorrei([X|Y], L, N):-N1 is N-1, append(Y,[X],Y1), recorrei(Y1, L, N1).

rotatelist([H|T], R) :- une(T, [H], R).

une([],L,L).
une([H|T],L2,[H,L3]) :- une(T,L2,L3).