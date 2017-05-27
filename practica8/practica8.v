Inductive booleano : Type :=
| btrue : booleano
| bfalse : booleano.

Fixpoint and ( b1 b2 : booleano ) : booleano :=
  match b1 with
  | bfalse => bfalse
  | _ => b2
  end.

Fixpoint or ( b1 b2 : booleano ) : booleano :=
  match b1 with
  | btrue => btrue
  | _ => b2
  end.

Fixpoint impl ( b1 b2 : booleano ) : booleano :=
  match b1 with
  | bfalse => btrue
  | _ => b2
  end.

Fixpoint neg ( b1 : booleano ) : booleano :=
  match b1 with
  | btrue => bfalse
  | bfalse => btrue
  end.

Theorem andI: forall (p q :booleano), p = btrue -> q = btrue -> and p q = btrue.

Theorem andE1: forall (p q:booleano), and p q = btrue -> p = btrue.
Proof.
induction p.
intros.
trivial.
intros.
contradict H.
discriminate.
Qed.

Theorem andE2: forall (p q:booleano), and p q = btrue -> q = btrue.
Proof.
induction q.
intros.
trivial.
intros.
contradict H.
destruct p.
discriminate.
discriminate.
Qed.

Example ejemplo1: forall (p q r:booleano), and p q = btrue-> r = btrue -> and q r = btrue.
Proof.
induction q.
intros.
rewrite H0.
trivial.
intros.
rewrite <- H.
rewrite H.
rewrite H0.
simpl.
Qed.

Example ejemplo2: forall (p q r: booleano), p = btrue-> neg (neg(and q r))=btrue->
and (neg (neg p)) r= btrue.
Proof.
induction p.
Qed.
