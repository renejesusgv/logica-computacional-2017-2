bt(void).
bt(node(A,T1,T2)):- integer(A),bt(T1),bt(T2).

elem(A,bt(node(X,_T1,_T2))):- A==X,!.
elem(A,bt(node(_X,T1,T2))):- elem(A,T1);elem(A,T2),!.

maxtree(A,bt(node(X,T1,T2))):- A>X, maxtree(A,T1),maxtree(A,T2).
maxtree(A,bt(node(X,bt(void),T2))):- A>X, maxtree(A,T2),!.
maxtree(A,bt(node(X,T1,bt(void)))):- A>X, maxtree(A,T1),!.
maxtree(A,bt(node(X,bt(void),bt(void)))):- A>X,!.

mintree(A,bt(node(X,T1,T2))):- A<X, mintree(A,T1),mintree(A,T2).
mintree(A,bt(node(X,bt(void),T2))):- A<X, mintree(A,T2),!.
mintree(A,bt(node(X,T1,bt(void)))):- A<X, mintree(A,T1),!.
mintree(A,bt(node(X,bt(void),bt(void)))):- A<X,!.

elembst(A,bt(node(X,_,_))):- A==X,!.
elembst(A,bt(node(X,T1,_))):- A<X,elembst(A,T1),!.
elembst(A,bt(node(X,_,T2))):- A>X,elembst(A,T2),!.
