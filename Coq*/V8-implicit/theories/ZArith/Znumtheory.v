(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i $Id: Znumtheory.v 9245 2006-10-17 12:53:34Z notin $ i*)

Require Import ZArith_base.
Require Import ZArithRing.
Require Import Zcomplements.
Require Import Zdiv.
Require Import Ndigits.
Require Import Wf_nat.
Open Local Scope Z_scope.

Require Import ImplicitAxioms.

(** This file contains some notions of number theory upon Z numbers: 
     - a divisibility predicate [Zdivide]
     - a gcd predicate [gcd]
     - Euclid algorithm [euclid]
     - a relatively prime predicate [rel_prime]
     - a prime predicate [prime]
     - an efficient [Zgcd] function 
*)

(** * Divisibility *)

Inductive Zdivide (a b:Z) : Prop :=
    Zdivide_intro : forall q:Z, b = q * a -> Zdivide a b.

(** Syntax for divisibility *)

Notation "( a | b )" := (Zdivide a b) (at level 0) : Z_scope.

(** Results concerning divisibility*)

Lemma Zdivide_refl : forall a:Z, (a | a).
Proof.
  intros; apply Zdivide_intro with 1; ring.
Qed.

Lemma Zone_divide : forall a:Z, (1 | a).
Proof.
  intros; apply Zdivide_intro with a; ring.
Qed.

Lemma Zdivide_0 : forall a:Z, (a | 0).
Proof.
  intros; apply Zdivide_intro with 0; ring.
Qed.

Hint Resolve Zdivide_refl Zone_divide Zdivide_0: zarith.

Lemma Zmult_divide_compat_l : forall a b c:Z, (a | b) -> (c * a | c * b).
Proof.
  simple induction 1; intros; apply Zdivide_intro with q.
  rewrite H0; ring.
Qed.

Lemma Zmult_divide_compat_r : forall a b c:Z, (a | b) -> (a * c | b * c).
Proof.
  intros a b c; rewrite (Zmult_comm a c); rewrite (Zmult_comm b c).
  apply Zmult_divide_compat_l; trivial.
Qed.

Hint Resolve Zmult_divide_compat_l Zmult_divide_compat_r: zarith.

Lemma Zdivide_plus_r : forall a b c:Z, (a | b) -> (a | c) -> (a | b + c).
Proof.
  simple induction 1; intros q Hq; simple induction 1; intros q' Hq'.
  apply Zdivide_intro with (q + q').
  rewrite Hq; rewrite Hq'; ring.
Qed.

Lemma Zdivide_opp_r : forall a b:Z, (a | b) -> (a | - b).
Proof.
  simple induction 1; intros; apply Zdivide_intro with (- q).
  rewrite H0; ring.
Qed.

Lemma Zdivide_opp_r_rev : forall a b:Z, (a | - b) -> (a | b).
Proof.
  intros; replace b with (- - b). apply Zdivide_opp_r; trivial. ring.
Qed.

Lemma Zdivide_opp_l : forall a b:Z, (a | b) -> (- a | b).
Proof.
  simple induction 1; intros; apply Zdivide_intro with (- q).
  rewrite H0; ring.
Qed.

Lemma Zdivide_opp_l_rev : forall a b:Z, (- a | b) -> (a | b).
Proof.
  intros; replace a with (- - a). apply Zdivide_opp_l; trivial. ring.
Qed.

Lemma Zdivide_minus_l : forall a b c:Z, (a | b) -> (a | c) -> (a | b - c).
Proof.
  simple induction 1; intros q Hq; simple induction 1; intros q' Hq'.
  apply Zdivide_intro with (q - q').
  rewrite Hq; rewrite Hq'; ring.
Qed.

Lemma Zdivide_mult_l : forall a b c:Z, (a | b) -> (a | b * c).
Proof.
  simple induction 1; intros q Hq; apply Zdivide_intro with (q * c).
  rewrite Hq; ring.
Qed.

Lemma Zdivide_mult_r : forall a b c:Z, (a | c) -> (a | b * c).
Proof.
  simple induction 1; intros q Hq; apply Zdivide_intro with (q * b).
  rewrite Hq; ring.
Qed.

Lemma Zdivide_factor_r : forall a b:Z, (a | a * b).
Proof.
  intros; apply Zdivide_intro with b; ring.
Qed.

Lemma Zdivide_factor_l : forall a b:Z, (a | b * a).
Proof.
  intros; apply Zdivide_intro with b; ring.
Qed.

Hint Resolve Zdivide_plus_r Zdivide_opp_r Zdivide_opp_r_rev Zdivide_opp_l
  Zdivide_opp_l_rev Zdivide_minus_l Zdivide_mult_l Zdivide_mult_r
  Zdivide_factor_r Zdivide_factor_l: zarith.

(** Auxiliary result. *)

Lemma Zmult_one : forall x y:Z, x >= 0 -> x * y = 1 -> x = 1.
Proof.
  intros x y H H0; destruct (Zmult_1_inversion_l _ _ H0) as [Hpos| Hneg].
  assumption.
  rewrite Hneg in H; simpl in H.
  contradiction (Zle_not_lt 0 (-1)).
    apply Zge_le; assumption.
    apply Zorder.Zlt_neg_0.
Qed.

(** Only [1] and [-1] divide [1]. *)

Lemma Zdivide_1 : forall x:Z, (x | 1) -> x = 1 \/ x = -1.
Proof.
  simple induction 1; intros.
  elim (Z_lt_ge_dec 0 x); [ left | right ].
  apply impl_PI; apply Zmult_one with q; auto with zarith; rewrite H0; ring.
  assert (- x = 1); auto with zarith.
  apply impl_PI; apply Zmult_one with (- q); auto with zarith; rewrite H0; ring.
Qed.

(** If [a] divides [b] and [b] divides [a] then [a] is [b] or [-b]. *)

Lemma Zdivide_antisym : forall a b:Z, (a | b) -> (b | a) -> a = b \/ a = - b.
Proof.
simple induction 1; intros.
inversion H1.
rewrite H0 in H2; clear H H1.
case (Z_zerop a); intro.
apply impl_PI; left; rewrite H0; rewrite e; ring.
assert (Hqq0 : q0 * q = 1).
apply Zmult_reg_l with a.
apply impl_PI; assumption.
ring_simplify.
pattern a at 2 in |- *; rewrite H2; ring.
assert (q | 1).
rewrite <- Hqq0; auto with zarith.
elim (Zdivide_1 q H); intros.
rewrite H1 in H0; left; omega.
rewrite H1 in H0; right; omega.
Qed.

(** If [a] divides [b] and [b<>0] then [|a| <= |b|]. *)

Lemma Zdivide_bounds : forall a b:Z, (a | b) -> b <> 0 -> Zabs a <= Zabs b.
Proof.
  simple induction 1; intros.
  assert (Zabs b = Zabs q * Zabs a).
  subst; apply Zabs_Zmult.
  rewrite H2.
  assert (H3 := Zabs_pos q).
  assert (H4 := Zabs_pos a).
  assert (Zabs q * Zabs a >= 1 * Zabs a); auto with zarith.
  apply Zmult_ge_compat; auto with zarith.
  elim (Z_lt_ge_dec (Zabs q) 1); [ intros | apply impl_PI; auto with zarith ].
  assert (Zabs q = 0).
  apply impl_PI; omega.
  assert (q = 0).
  rewrite <- (Zabs_Zsgn q).
  rewrite H5; auto with zarith.
  subst q; omega.
Qed.

(** * Greatest common divisor (gcd). *)
   
(** There is no unicity of the gcd; hence we define the predicate [gcd a b d] 
     expressing that [d] is a gcd of [a] and [b]. 
     (We show later that the [gcd] is actually unique if we discard its sign.) *)

Inductive Zis_gcd (a b d:Z) : Prop :=
  Zis_gcd_intro :
  (d | a) ->
  (d | b) -> (forall x:Z, (x | a) -> (x | b) -> (x | d)) -> Zis_gcd a b d.

(** Trivial properties of [gcd] *)

Lemma Zis_gcd_sym : forall a b d:Z, Zis_gcd a b d -> Zis_gcd b a d.
Proof.
  simple induction 1; constructor; intuition.
Qed.

Lemma Zis_gcd_0 : forall a:Z, Zis_gcd a 0 a.
Proof.
  constructor; auto with zarith.
Qed.

Lemma Zis_gcd_1 : forall a, Zis_gcd a 1 1.
Proof.
  constructor; auto with zarith.
Qed.

Lemma Zis_gcd_refl : forall a, Zis_gcd a a a.
Proof.
  constructor; auto with zarith.
Qed.

Lemma Zis_gcd_minus : forall a b d:Z, Zis_gcd a (- b) d -> Zis_gcd b a d.
Proof.
  simple induction 1; constructor; intuition.
Qed.

Lemma Zis_gcd_opp : forall a b d:Z, Zis_gcd a b d -> Zis_gcd b a (- d).
Proof.
  simple induction 1; constructor; intuition.
Qed.

Lemma Zis_gcd_0_abs : forall a:Z, Zis_gcd 0 a (Zabs a).
Proof.
  intros a.
  apply Zabs_ind.
  intros; apply Zis_gcd_sym; apply Zis_gcd_0; auto.
  intros; apply Zis_gcd_opp; apply Zis_gcd_0; auto.
Qed.

Hint Resolve Zis_gcd_sym Zis_gcd_0 Zis_gcd_minus Zis_gcd_opp: zarith.

(** * Extended Euclid algorithm. *)

(** Euclid's algorithm to compute the [gcd] mainly relies on
    the following property. *)

Lemma Zis_gcd_for_euclid :
  forall a b d q:Z, Zis_gcd b (a - q * b) d -> Zis_gcd a b d.
Proof.
  simple induction 1; constructor; intuition.
  replace a with (a - q * b + q * b). auto with zarith. ring.
Qed.

Lemma Zis_gcd_for_euclid2 :
  forall b d q r:Z, Zis_gcd r b d -> Zis_gcd b (b * q + r) d.
Proof.
  simple induction 1; constructor; intuition.
  apply H2; auto.
  replace r with (b * q + r - b * q). auto with zarith. ring.
Qed.

(** We implement the extended version of Euclid's algorithm,
    i.e. the one computing Bezout's coefficients as it computes
    the [gcd]. We follow the algorithm given in Knuth's
    "Art of Computer Programming", vol 2, page 325. *)

Section extended_euclid_algorithm.

  Variables a b : Z.

  (** The specification of Euclid's algorithm is the existence of
      [u], [v] and [d] such that [ua+vb=d] and [(gcd a b d)]. *)

  Inductive Euclid : Set :=
    Euclid_intro :
    forall u v d:Z, [u * a + v * b = d] -> [Zis_gcd a b d] -> Euclid.

  (** The recursive part of Euclid's algorithm uses well-founded
      recursion of non-negative integers. It maintains 6 integers
      [u1,u2,u3,v1,v2,v3] such that the following invariant holds:
      [u1*a+u2*b=u3] and [v1*a+v2*b=v3] and [gcd(u2,v3)=gcd(a,b)]. 
      *)

  Lemma euclid_rec :
    forall v3:Z,
      [0 <= v3] ->
      forall u1 u2 u3 v1 v2:Z,
	[u1 * a + u2 * b = u3] ->
	[v1 * a + v2 * b = v3] ->
	[forall d:Z, Zis_gcd u3 v3 d -> Zis_gcd a b d] -> Euclid.
  Proof.
    intros v3 Hv3; generalize [Hv3]; pattern v3 in |- *.
    apply Zlt_0_rec.
    clear v3 Hv3; intros.
    elim (Z_zerop x); intro.
    apply Euclid_intro with (u := u1) (v := u2) (d := u3).
    assumption.
    apply H3.
    rewrite a0; auto with zarith.
    set (q := u3 / x) in *.
cut [0 <= u3 - q * x < x].
    intro Hq.

    apply (H (u3 - q * x) (impl_PI Hq) (proj1 Hq) v1 v2 x (u1 - q * v1) (u2 - q * v2)).
    tauto.
    replace ((u1 - q * v1) * a + (u2 - q * v2) * b) with
      (u1 * a + u2 * b - q * (v1 * a + v2 * b)).
    rewrite H1; rewrite H2; trivial.
    ring.
    intros; apply H3.
    apply Zis_gcd_for_euclid with q; assumption.

replace (u3 - q * x) with (u3 mod x).
apply Z_mod_lt; omega.
assert (xpos : x > 0). omega.
generalize (Z_div_mod_eq u3 x xpos). 
unfold q in |- *. 
intro eq; pattern u3 at 2 in |- *; rewrite eq; ring.

    apply impl_PI; assumption.
  Qed.

  (** We get Euclid's algorithm by applying [euclid_rec] on
      [1,0,a,0,1,b] when [b>=0] and [1,0,a,0,-1,-b] when [b<0]. *)

  Lemma euclid : Euclid.
  Proof.
    case (Z_le_gt_dec 0 b); intro.
    intros;
      apply euclid_rec with
	(u1 := 1) (u2 := 0) (u3 := a) (v1 := 0) (v2 := 1) (v3 := b);
	auto with zarith; ring.
    intros;
      apply euclid_rec with
	(u1 := 1) (u2 := 0) (u3 := a) (v1 := 0) (v2 := -1) (v3 := - b);
	auto with zarith; try ring.
  Qed.

End extended_euclid_algorithm.

Theorem Zis_gcd_uniqueness_apart_sign :
  forall a b d d':Z, Zis_gcd a b d -> Zis_gcd a b d' -> d = d' \/ d = - d'.
Proof.
  simple induction 1.
  intros H1 H2 H3; simple induction 1; intros.
  generalize (H3 d' H4 H5); intro Hd'd.
  generalize (H6 d H1 H2); intro Hdd'.
  exact (Zdivide_antisym d d' Hdd' Hd'd).
Qed.

(** * Bezout's coefficients *)

Inductive Bezout (a b d:Z) : Prop :=
  Bezout_intro : forall u v:Z, u * a + v * b = d -> Bezout a b d.

(** Existence of Bezout's coefficients for the [gcd] of [a] and [b] *)

Lemma Zis_gcd_bezout : forall a b d:Z, Zis_gcd a b d -> Bezout a b d.
Proof.
  intros a b d Hgcd.
  elim (euclid a b); intros u v d0 e g.
  apply impl_PI.
  generalize (Zis_gcd_uniqueness_apart_sign a b d d0 Hgcd g).
  intro H; elim H; clear H; intros.
  apply Bezout_intro with u v.
  rewrite H; assumption.
  apply Bezout_intro with (- u) (- v).
  rewrite H; rewrite <- e; ring.
Qed.

(** gcd of [ca] and [cb] is [c gcd(a,b)]. *)

Lemma Zis_gcd_mult :
  forall a b c d:Z, Zis_gcd a b d -> Zis_gcd (c * a) (c * b) (c * d).
Proof.
  intros a b c d; simple induction 1; constructor; intuition.
  elim (Zis_gcd_bezout a b d H); intros.
  elim H3; intros.
  elim H4; intros.
  apply Zdivide_intro with (u * q + v * q0).
  rewrite <- H5.
  replace (c * (u * a + v * b)) with (u * (c * a) + v * (c * b)).
  rewrite H6; rewrite H7; ring.
  ring.
Qed.
  

(** * Relative primality *)

Definition rel_prime (a b:Z) : Prop := Zis_gcd a b 1.

(** Bezout's theorem: [a] and [b] are relatively prime if and
    only if there exist [u] and [v] such that [ua+vb = 1]. *)

Lemma rel_prime_bezout : forall a b:Z, rel_prime a b -> Bezout a b 1.
Proof.
  intros a b; exact (Zis_gcd_bezout a b 1).
Qed.

Lemma bezout_rel_prime : forall a b:Z, Bezout a b 1 -> rel_prime a b.
Proof.
  simple induction 1; constructor; auto with zarith.
  intros. rewrite <- H0; auto with zarith.
Qed.

(** Gauss's theorem: if [a] divides [bc] and if [a] and [b] are
    relatively prime, then [a] divides [c]. *)

Theorem Gauss : forall a b c:Z, (a | b * c) -> rel_prime a b -> (a | c).
Proof.
  intros. elim (rel_prime_bezout a b H0); intros.
  replace c with (c * 1); [ idtac | ring ].
  rewrite <- H1.
  replace (c * (u * a + v * b)) with (c * u * a + v * (b * c));
    [ eauto with zarith | ring ].
Qed.

(** If [a] is relatively prime to [b] and [c], then it is to [bc] *)

Lemma rel_prime_mult :
  forall a b c:Z, rel_prime a b -> rel_prime a c -> rel_prime a (b * c).
Proof.
  intros a b c Hb Hc.
  elim (rel_prime_bezout a b Hb); intros.
  elim (rel_prime_bezout a c Hc); intros.
  apply bezout_rel_prime.
  apply Bezout_intro with
    (u := u * u0 * a + v0 * c * u + u0 * v * b) (v := v * v0).
  rewrite <- H.
  replace (u * a + v * b) with ((u * a + v * b) * 1); [ idtac | ring ].
  rewrite <- H0.
  ring.
Qed.

Lemma rel_prime_cross_prod :
  forall a b c d:Z,
    rel_prime a b ->
    rel_prime c d -> b > 0 -> d > 0 -> a * d = b * c -> a = c /\ b = d.
Proof.
  intros a b c d; intros.
  elim (Zdivide_antisym b d).
  split; auto with zarith.
  rewrite H4 in H3.
  rewrite Zmult_comm in H3.
  apply Zmult_reg_l with d; auto with zarith.
  intros; omega.
  apply Gauss with a.
  rewrite H3.
  auto with zarith.
  red in |- *; auto with zarith.
  apply Gauss with c.
  rewrite Zmult_comm.
  rewrite <- H3.
  auto with zarith.
  red in |- *; auto with zarith.
Qed.

(** After factorization by a gcd, the original numbers are relatively prime. *)

Lemma Zis_gcd_rel_prime :
  forall a b g:Z,
    b > 0 -> g >= 0 -> Zis_gcd a b g -> rel_prime (a / g) (b / g).
  intros a b g; intros.
  assert (g <> 0).
  intro.
  elim H1; intros. 
  elim H4; intros.
  rewrite H2 in H6; subst b; omega.
  unfold rel_prime in |- *.
  destruct H1.
  destruct H1 as (a',H1).
  destruct H3 as (b',H3).
  replace (a/g) with a'; 
    [|rewrite H1; rewrite Z_div_mult; auto with zarith].
  replace (b/g) with b'; 
    [|rewrite H3; rewrite Z_div_mult; auto with zarith].
  constructor.
  exists a'; auto with zarith.
  exists b'; auto with zarith.
  intros x (xa,H5) (xb,H6).
  destruct (H4 (x*g)).
  exists xa; rewrite Zmult_assoc; rewrite <- H5; auto.
  exists xb; rewrite Zmult_assoc; rewrite <- H6; auto.
  replace g with (1*g) in H7; auto with zarith.
  do 2 rewrite Zmult_assoc in H7.
  generalize (Zmult_reg_r _ _ _ H2 H7); clear H7; intros.
  rewrite Zmult_1_r in H7.
  exists q; auto with zarith.
Qed.

(** * Primality *)

Inductive prime (p:Z) : Prop :=
  prime_intro :
    1 < p -> (forall n:Z, 1 <= n < p -> rel_prime n p) -> prime p.

(** The sole divisors of a prime number [p] are [-1], [1], [p] and [-p]. *)

Lemma prime_divisors :
  forall p:Z,
    prime p -> forall a:Z, (a | p) -> a = -1 \/ a = 1 \/ a = p \/ a = - p.
Proof.
  simple induction 1; intros.
  assert
    (a = - p \/ - p < a < -1 \/ a = -1 \/ a = 0 \/ a = 1 \/ 1 < a < p \/ a = p).
  assert (Zabs a <= Zabs p). apply Zdivide_bounds; [ assumption | omega ].
  generalize H3. 
  pattern (Zabs a) in |- *; apply Zabs_ind; pattern (Zabs p) in |- *;
    apply Zabs_ind; intros; omega.
  intuition idtac.
  (* -p < a < -1 *)
  absurd (rel_prime (- a) p); intuition.
  inversion H3.
  assert (- a | - a); auto with zarith.
  assert (- a | p); auto with zarith.
  generalize (H8 (- a) H9 H10); intuition idtac.
  generalize (Zdivide_1 (- a) H11); intuition.
  (* a = 0 *)
  inversion H2. subst a; omega.
  (* 1 < a < p *)
  absurd (rel_prime a p); intuition.
  inversion H3.
  assert (a | a); auto with zarith.
  assert (a | p); auto with zarith.
  generalize (H8 a H9 H10); intuition idtac.
  generalize (Zdivide_1 a H11); intuition.
Qed.

(** A prime number is relatively prime with any number it does not divide *)

Lemma prime_rel_prime :
  forall p:Z, prime p -> forall a:Z, ~ (p | a) -> rel_prime p a.
Proof.
  simple induction 1; intros.
  constructor; intuition.
  elim (prime_divisors p H x H3); intuition; subst; auto with zarith.
  absurd (p | a); auto with zarith.
  absurd (p | a); intuition.
Qed.

Hint Resolve prime_rel_prime: zarith.

(** [Zdivide] can be expressed using [Zmod]. *)

Lemma Zmod_divide : forall a b:Z, b > 0 -> a mod b = 0 -> (b | a).
Proof.
  intros a b H H0.
  apply Zdivide_intro with (a / b).
  pattern a at 1 in |- *; rewrite (Z_div_mod_eq a b H).
  rewrite H0; ring.
Qed.

Lemma Zdivide_mod : forall a b:Z, b > 0 -> (b | a) -> a mod b = 0.
Proof.
  intros a b; simple destruct 2; intros; subst.
  change (q * b) with (0 + q * b) in |- *.
  rewrite Z_mod_plus; auto.
Qed.

(** [Zdivide] is hence decidable *)

Lemma Zdivide_dec : forall a b:Z, {(a | b)} + {~ (a | b)}.
Proof.
  intros a b; elim (Ztrichotomy_inf a 0).
  (* a<0 *)
  intros H; elim H; intros. 
  case (Z_eq_dec (b mod - a) 0).
  left; apply Zdivide_opp_l_rev; apply Zmod_divide; auto with zarith.
  intro H1; right; intro; elim H1; apply Zdivide_mod; auto with zarith.
  (* a=0 *)
  case (Z_eq_dec b 0); intro.
  left; subst; auto with zarith.
  right; subst; intro H0; inversion H0; omega.
  (* a>0 *)
  intro H; case (Z_eq_dec (b mod a) 0).
  left; apply Zmod_divide; auto with zarith.
  intro H1; right; intro; elim H1; apply Zdivide_mod; auto with zarith.
Qed.

(** If a prime [p] divides [ab] then it divides either [a] or [b] *)

Lemma prime_mult :
  forall p:Z, prime p -> forall a b:Z, (p | a * b) -> (p | a) \/ (p | b).
Proof.
  intro p; simple induction 1; intros.
  destruct (Zdivide_dec p a); apply impl_PI; intuition.
  right; apply Gauss with a; auto with zarith.
Qed.


(** We could obtain a [Zgcd] function via Euclid algorithm. But we propose 
  here a binary version of [Zgcd], faster and executable within Coq.

   Algorithm: 

   gcd 0 b = b
   gcd a 0 = a
   gcd (2a) (2b) = 2(gcd a b)
   gcd (2a+1) (2b) = gcd (2a+1) b
   gcd (2a) (2b+1) = gcd a (2b+1)
   gcd (2a+1) (2b+1) = gcd (b-a) (2*a+1)
                    or gcd (a-b) (2*b+1), depending on whether a<b 
*)   

Open Scope positive_scope.

Fixpoint Pgcdn (n: nat) (a b : positive) { struct n } : positive := 
  match n with 
    | O => 1
    | S n => 
      match a,b with 
	| xH, _ => 1 
	| _, xH => 1
	| xO a, xO b => xO (Pgcdn n a b)
	| a, xO b => Pgcdn n a b
	| xO a, b => Pgcdn n a b
	| xI a', xI b' => match Pcompare a' b' Eq with 
			    | Eq => a
			    | Lt => Pgcdn  n (b'-a') a
			    | Gt => Pgcdn n (a'-b') b
			  end
      end
  end.

Fixpoint Pggcdn (n: nat) (a b : positive) { struct n } : (positive*(positive*positive)) := 
  match n with 
    | O => (1,(a,b))
    | S n => 
      match a,b with 
	| xH, b => (1,(1,b)) 
	| a, xH => (1,(a,1))
	| xO a, xO b => 
          let (g,p) := Pggcdn n a b in 
            (xO g,p)
	| a, xO b => 
          let (g,p) := Pggcdn n a b in 
            let (aa,bb) := p in 
              (g,(aa, xO bb))
	| xO a, b => 
          let (g,p) := Pggcdn n a b in 
            let (aa,bb) := p in 
              (g,(xO aa, bb))
	| xI a', xI b' => match Pcompare a' b' Eq with 
			    | Eq => (a,(1,1))
			    | Lt => 
			      let (g,p) := Pggcdn n (b'-a') a in 
				let (ba,aa) := p in 
				  (g,(aa, aa + xO ba))
			    | Gt => 
			      let (g,p) := Pggcdn n (a'-b') b in 
				let (ab,bb) := p in 
				  (g,(bb+xO ab, bb))
			  end
      end
  end.

Definition Pgcd (a b: positive) := Pgcdn (Psize a + Psize b)%nat a b.
Definition Pggcd (a b: positive) := Pggcdn (Psize a + Psize b)%nat a b.

Open Scope Z_scope.

Definition Zgcd (a b : Z) : Z := match a,b with 
				   | Z0, _ => Zabs b 
				   | _, Z0 => Zabs a
				   | Zpos a, Zpos b => Zpos (Pgcd a b)
				   | Zpos a, Zneg b => Zpos (Pgcd a b)
				   | Zneg a, Zpos b => Zpos (Pgcd a b)
				   | Zneg a, Zneg b => Zpos (Pgcd a b)
				 end.

Definition Zggcd (a b : Z) : Z*(Z*Z) := match a,b with 
					  | Z0, _ => (Zabs b,(0, Zsgn b)) 
					  | _, Z0 => (Zabs a,(Zsgn a, 0))
					  | Zpos a, Zpos b => 
					    let (g,p) := Pggcd a b in 
					      let (aa,bb) := p in 
						(Zpos g, (Zpos aa, Zpos bb))
					  | Zpos a, Zneg b => 
					    let (g,p) := Pggcd a b in 
					      let (aa,bb) := p in 
						(Zpos g, (Zpos aa, Zneg bb))
					  | Zneg a, Zpos b => 
					    let (g,p) := Pggcd a b in 
					      let (aa,bb) := p in 
						(Zpos g, (Zneg aa, Zpos bb))
					  | Zneg a, Zneg b =>
					    let (g,p) := Pggcd a b in 
					      let (aa,bb) := p in 
						(Zpos g, (Zneg aa, Zneg bb))
					end.

Lemma Zgcd_is_pos : forall a b, 0 <= Zgcd a b.
Proof.
  unfold Zgcd; destruct a; destruct b; auto with zarith.
Qed.

Lemma Psize_monotone : forall p q, Pcompare p q Eq = Lt -> (Psize p <= Psize q)%nat.
Proof.
  induction p; destruct q; simpl; auto with arith; intros; try discriminate.
  intros; generalize (Pcompare_Gt_Lt _ _ H); auto with arith.
  intros; destruct (Pcompare_Lt_Lt _ _ H); auto with arith; subst; auto.
Qed.

Lemma Pminus_Zminus : forall a b, Pcompare a b Eq = Lt -> 
  Zpos (b-a) = Zpos b - Zpos a.
Proof.
  intros.
  repeat rewrite Zpos_eq_Z_of_nat_o_nat_of_P.
  rewrite nat_of_P_minus_morphism.
  apply inj_minus1.
  apply lt_le_weak.
  apply nat_of_P_lt_Lt_compare_morphism; auto.
  rewrite ZC4; rewrite H; auto.
Qed.

Lemma Zis_gcd_even_odd : forall a b g, Zis_gcd (Zpos a) (Zpos (xI b)) g -> 
  Zis_gcd (Zpos (xO a)) (Zpos (xI b)) g. 
Proof.
  intros.
  destruct H.
  constructor; auto.
  destruct H as (e,H2); exists (2*e); auto with zarith.
  rewrite Zpos_xO; rewrite H2; ring.
  intros.
  apply H1; auto.
  rewrite Zpos_xO in H2.
  rewrite Zpos_xI in H3.
  apply Gauss with 2; auto.
  apply bezout_rel_prime.
  destruct H3 as (bb, H3).
  apply Bezout_intro with bb (-Zpos b).
  omega.
Qed.

Lemma Pgcdn_correct : forall n a b, (Psize a + Psize b<=n)%nat -> 
  Zis_gcd (Zpos a) (Zpos b) (Zpos (Pgcdn n a b)).
Proof.
  intro n; pattern n; apply lt_wf_ind; clear n; intros.
  destruct n.
  simpl.
  destruct a; simpl in *; try inversion H0.
  destruct a.
  destruct b; simpl.
  case_eq (Pcompare a b Eq); intros.
  (* a = xI, b = xI, compare = Eq *)
  rewrite (Pcompare_Eq_eq _ _ H1); apply Zis_gcd_refl.
  (* a = xI, b = xI, compare = Lt *)
  apply Zis_gcd_sym.
  apply Zis_gcd_for_euclid with 1.
  apply Zis_gcd_sym.
  replace (Zpos (xI b) - 1 * Zpos (xI a)) with (Zpos(xO (b - a))).
  apply Zis_gcd_even_odd.
  apply H; auto.
  simpl in *.
  assert (Psize (b-a) <= Psize b)%nat.
  apply Psize_monotone.
  change (Zpos (b-a) < Zpos b).
  rewrite (Pminus_Zminus _ _ H1).
  assert (0 < Zpos a) by (compute; auto).
  omega.
  omega.  
  rewrite Zpos_xO; do 2 rewrite Zpos_xI.
  rewrite Pminus_Zminus; auto.
  omega.
  (* a = xI, b = xI, compare = Gt *)
  apply Zis_gcd_for_euclid with 1.
  replace (Zpos (xI a) - 1 * Zpos (xI b)) with (Zpos(xO (a - b))).
  apply Zis_gcd_sym.
  apply Zis_gcd_even_odd.
  apply H; auto.
  simpl in *.
  assert (Psize (a-b) <= Psize a)%nat.
  apply Psize_monotone.
  change (Zpos (a-b) < Zpos a).
  rewrite (Pminus_Zminus b a).
  assert (0 < Zpos b) by (compute; auto).
  omega.
  rewrite ZC4; rewrite H1; auto.
  omega.  
  rewrite Zpos_xO; do 2 rewrite Zpos_xI.
  rewrite Pminus_Zminus; auto.
  omega.
  rewrite ZC4; rewrite H1; auto.
  (* a = xI, b = xO *)
  apply Zis_gcd_sym.
  apply Zis_gcd_even_odd.
  apply Zis_gcd_sym.
  apply H; auto.
  simpl in *; omega.
  (* a = xI, b = xH *)
  apply Zis_gcd_1.
  destruct b; simpl.
  (* a = xO, b = xI *)
  apply Zis_gcd_even_odd.
  apply H; auto.
  simpl in *; omega.
  (* a = xO, b = xO *)
  rewrite (Zpos_xO a); rewrite (Zpos_xO b); rewrite (Zpos_xO (Pgcdn n a b)).
  apply Zis_gcd_mult.
  apply H; auto.
  simpl in *; omega.
  (* a = xO, b = xH *)
  apply Zis_gcd_1.
  (* a = xH *)
  simpl; apply Zis_gcd_sym; apply Zis_gcd_1.
Qed.

Lemma Pgcd_correct : forall a b, Zis_gcd (Zpos a) (Zpos b) (Zpos (Pgcd a b)).
Proof.
  unfold Pgcd; intros.
  apply Pgcdn_correct; auto.
Qed.

Lemma Zgcd_is_gcd : forall a b, Zis_gcd a b (Zgcd a b).
Proof.
  destruct a.
  intros.
  simpl.
  apply Zis_gcd_0_abs.
  destruct b; simpl.
  apply Zis_gcd_0.
  apply Pgcd_correct.
  apply Zis_gcd_sym.
  apply Zis_gcd_minus; simpl.
  apply Pgcd_correct.
  destruct b; simpl.
  apply Zis_gcd_minus; simpl.
  apply Zis_gcd_sym.
  apply Zis_gcd_0.
  apply Zis_gcd_minus; simpl.
  apply Zis_gcd_sym.
  apply Pgcd_correct.
  apply Zis_gcd_sym.
  apply Zis_gcd_minus; simpl.
  apply Zis_gcd_minus; simpl.
  apply Zis_gcd_sym.
  apply Pgcd_correct.
Qed.


Lemma Pggcdn_gcdn : forall n a b, 
  fst (Pggcdn n a b) = Pgcdn n a b.
Proof.
  induction n.
  simpl; auto.
  destruct a; destruct b; simpl; auto.
  destruct (Pcompare a b Eq); simpl; auto.
  rewrite <- IHn; destruct (Pggcdn n (b-a) (xI a)) as (g,(aa,bb)); simpl; auto.
  rewrite <- IHn; destruct (Pggcdn n (a-b) (xI b)) as (g,(aa,bb)); simpl; auto.
  rewrite <- IHn; destruct (Pggcdn n (xI a) b) as (g,(aa,bb)); simpl; auto.
  rewrite <- IHn; destruct (Pggcdn n a (xI b)) as (g,(aa,bb)); simpl; auto.
  rewrite <- IHn; destruct (Pggcdn n a b) as (g,(aa,bb)); simpl; auto.
Qed.

Lemma Pggcd_gcd : forall a b, fst (Pggcd a b) = Pgcd a b.
Proof.
  intros; exact (Pggcdn_gcdn (Psize a+Psize b)%nat a b).
Qed.

Lemma Zggcd_gcd : forall a b, fst (Zggcd a b) = Zgcd a b.
Proof.
  destruct a; destruct b; simpl; auto; rewrite <- Pggcd_gcd; 
    destruct (Pggcd p p0) as (g,(aa,bb)); simpl; auto.
Qed.

Open Scope positive_scope.

Lemma Pggcdn_correct_divisors : forall n a b, 
  let (g,p) := Pggcdn n a b in 
    let (aa,bb):=p in 
      (a=g*aa) /\ (b=g*bb).
Proof.
  induction n.
  simpl; auto.
  destruct a; destruct b; simpl; auto.
  case_eq (Pcompare a b Eq); intros.
  (* Eq *)
  rewrite Pmult_comm; simpl; auto.
  rewrite (Pcompare_Eq_eq _ _ H); auto.
  (* Lt *)
  generalize (IHn (b-a) (xI a)); destruct (Pggcdn n (b-a) (xI a)) as (g,(ba,aa)); simpl.
  intros (H0,H1); split; auto.
  rewrite Pmult_plus_distr_l.
  rewrite Pmult_xO_permute_r.
  rewrite <- H1; rewrite <- H0.
  simpl; f_equal; symmetry.
  apply Pplus_minus; auto.
  rewrite ZC4; rewrite H; auto.
  (* Gt *)
  generalize (IHn (a-b) (xI b)); destruct (Pggcdn n (a-b) (xI b)) as (g,(ab,bb)); simpl.
  intros (H0,H1); split; auto.
  rewrite Pmult_plus_distr_l.
  rewrite Pmult_xO_permute_r.
  rewrite <- H1; rewrite <- H0.
  simpl; f_equal; symmetry.
  apply Pplus_minus; auto.
  (* Then... *) 
  generalize (IHn (xI a) b); destruct (Pggcdn n (xI a) b) as (g,(ab,bb)); simpl.
  intros (H0,H1); split; auto.
  rewrite Pmult_xO_permute_r; rewrite H1; auto.
  generalize (IHn a (xI b)); destruct (Pggcdn n a (xI b)) as (g,(ab,bb)); simpl.
  intros (H0,H1); split; auto.
  rewrite Pmult_xO_permute_r; rewrite H0; auto.
  generalize (IHn a b); destruct (Pggcdn n a b) as (g,(ab,bb)); simpl.
  intros (H0,H1); split; subst; auto.
Qed.

Lemma Pggcd_correct_divisors : forall a b, 
  let (g,p) := Pggcd a b in 
    let (aa,bb):=p in 
      (a=g*aa) /\ (b=g*bb).
Proof.
  intros a b; exact (Pggcdn_correct_divisors (Psize a + Psize b)%nat a b).
Qed.

Open Scope Z_scope.

Lemma Zggcd_correct_divisors : forall a b, 
  let (g,p) := Zggcd a b in 
    let (aa,bb):=p in 
      (a=g*aa) /\ (b=g*bb).
Proof.
  destruct a; destruct b; simpl; auto; try solve [rewrite Pmult_comm; simpl; auto]; 
    generalize (Pggcd_correct_divisors p p0); destruct (Pggcd p p0) as (g,(aa,bb)); 
      destruct 1; subst; auto.
Qed.

Theorem Zgcd_spec : forall x y : Z, {z : Z | Zis_gcd x y z /\ 0 <= z}.
Proof.
  intros x y; exists (Zgcd x y).
  split; [apply Zgcd_is_gcd  | apply Zgcd_is_pos].
Qed.



