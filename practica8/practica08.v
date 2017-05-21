Inductive booleano : Type : = 
|btrue: booleano
|bfalse: booleano

Definition and (b1 b2 : booleno) : booleano:=
	match b1 with
		|bfalse => bfalse
		|_ => b2
		end.

Definition or (b1 b2 : booleano) : booleano:=
	match b1 with
		|btrue = btrue
		|_ => b2
		end.

Definition impl (b1 b2 : booleano) : booleano:=
	match b1 with
		|bfalse => b2
		|_ => btrue
		end.

Definition neg (b1 : booleano) : booleano:=
	match b1 with
		|btrue => bfalse
		|bfalse => btrue
		end.


("PARTE I")
Theorem andI: forall (p q :booleano), p = btrue -> q = btrue -> and p q = btrue.

Theorem andE1: forall (p q:booleano), and p q = btrue -> p = btrue.

Theorem andE2: forall (p q:booleano), and p q = btrue -> q = btrue.

("PARTE II")
("PARTE III")
("PARTE IV")