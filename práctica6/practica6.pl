longitud([],0).
longitud([_|H], N) :- longitud(H, N1), N is N1+1.


ultimo([X|[]],X). 
ultimo([_|H],X):- ultimo(H,X).
