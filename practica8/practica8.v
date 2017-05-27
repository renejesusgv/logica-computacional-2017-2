Inductive booleano : Type :=
| btrue : booleano
| bfalse : booleano.

Definition and ( b1 b2 : booleano ) : booleano :=
match b1 with
| bfalse => bfalse
| _ => b2
end.

Definition or ( b1 b2 : booleano ) : booleano :=
match b1 with
| btrue => btrue
| _ => b2
end .

Definition impl ( b1 b2 : booleano ) : booleano :=
match b1 with
| bfalse => btrue
| _ => b2
end .
Definition neg ( b1 : booleano ) : booleano :=
match b1 with
| btrue => bfalse
| bfalse => btrue
end .

(*Definición de variables*)
Variable A: Type.
Variable x: A.

(*Reglas de conjunción*)
Theorem andI: forall (p q :booleano), p = btrue -> q = btrue -> and p q = btrue.
Proof.
induction p.
simpl.
trivial.
intros.
contradict H.
discriminate.
Qed.

(*Regla de la doble negación*)
Theorem dobleNeg: forall (p:booleano), neg(neg(p)) = p.
Proof.
induction p.
unfold neg.
trivial.
unfold neg.
trivial.
Qed.

(*Eliminación de la implicación*)
Theorem elimImpl: forall(f g:booleano), f=btrue -> impl f g=btrue -> g=btrue.
Proof.
destruct f.
simpl.
trivial.
intros.
contradict H.
discriminate.
Qed.

(*Modus Tollens*)
Theorem modusTollens: forall(f g: booleano), impl f g = btrue -> neg g = btrue ->
neg f = btrue.
Proof.
induction f.
simpl.
intros.
destruct g.
trivial.
contradict H.
discriminate.
destruct g.
simpl.
intros.
trivial.
simpl.
reflexivity.
Qed.

(*5.-*)
Theorem introImpl: forall (p q:booleano), p=btrue-> q=btrue-> impl p q=btrue.
Proof.
induction p.
simpl.
trivial.
simpl.
intros.
contradict H.
discriminate.
Qed.

(*5. alternativo-*)
Theorem introImpl: forall (p q:booleano), p=btrue-> q=btrue-> impl p q=btrue.
Proof.
intros.
rewrite H.
rewrite H0.
simpl.
trivial.
Qed.

(*PARTE 2*)
Example ejemplo2: forall (p q r: booleano), p = btrue-> neg (neg(and q r))=btrue->
and (neg (neg p)) r= btrue.
Proof.
induction r.
intros.
destruct H0.
destruct H.
destruct and.
destruct and.
unfold neg.
trivial.
unfold neg.





(*PARTE 3*)
Theorem elimForall: forall (p:A->Prop), (forall y:A, p y) -> p x.

