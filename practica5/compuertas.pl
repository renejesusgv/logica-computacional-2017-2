and(0,0,0).
and(0,1,0).
and(1,0,0).
and(1,1,1).

not(0,1).
not(1,0).

circuitoa(X,Y,Z) :-
	and(X,Y,W),
	not(W,Z).

circuitob(V,Z,Y,X,W) :-
	and(V,Z,A),
	and(Y,X,B),
	and(A,B,C),
	not(C,W).