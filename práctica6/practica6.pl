longitud([],0).
longitud([_|H], N) :- longitud(H, N1), N is N1+1.

ultimo([X|[]],X). 
ultimo([_|H],X):- ultimo(H,X).

transicion(estado(1),b,estado(2)).
transicion(estado(2),a,final(3)).
transicion(estado(2),a,estado(2)).
transicion(final(3),b,estado(2)).
final(3).

aceptaAux(final(3),[],final(3)).
aceptaAux(E,[X|H],_):- transicion(E,X,Y), aceptaAux(Y,H,_).

acepta(Y):- aceptaAux(estado(1),Y,_), final(3).



