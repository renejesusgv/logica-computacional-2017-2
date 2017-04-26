longitud([],0).
longitud([_|H], N) :- longitud(H, N1), N is N1+1.


ultimo([X|[]],X). 
ultimo([_|H],X):- ultimo(H,X).

/**recorrei(X,X,0).
recorrei([X|Y], L, N):-N1 is N-1, append(Y,[X],Y1), recorrei(Y1, L, N1).
*/

recorrei([H|T],Z) :- une(T, [H],Z).

une([],X,X).                            
une([X|Y],Z,[X|W]) :- une(Y,Z,W). 
