/*Ejercicio 02*/
/*	a)	*/
ultimo([X|[]],X). 
ultimo([_|H],X):- ultimo(H,X).

/*	b)	*/
longitud([],0).
longitud([_|H], N) :- longitud(H, N1), N is N1+1.

/*	c)	*/
recorrei([H|T],Z) :- une(T, [H],Z).
recorred(L, Z) :- recorrei(Z, L).

une([],X,X).                            
une([X|Y],Z,[X|W]) :- une(Y,Z,W). 

/*Ejercicio 03*/

/*	a)	*/
transicion(q1,b,q2).
transicion(q2,a,q2).
transicion(q2,a,q3).
transicion(q3,b,q2).
inicial(q1).
final(q3).

/*	b)	*/
acepta([], q3).
acepta([H|T], estado) :- transicion(estado, H, X), acepta(T,X), final(X).



