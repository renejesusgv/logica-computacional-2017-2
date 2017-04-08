bt(void).
bt(node(A,T1,T2)):- integer(A),bt(T1),bt(T2).

elem(A,bt(node(X,_T1,_T2))):- A==X,!.
elem(A,bt(node(_X,T1,T2))):- elem(A,T1);elem(A,T2),!.
