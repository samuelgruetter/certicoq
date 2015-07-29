Add LoadPath "../common" as Common.
Add LoadPath "../L1_MalechaQuoted" as L1.
Add LoadPath "." as L2.
Require Import Lists.List.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Arith.EqNat.
Require Import Arith.Compare_dec.
Require Import L2.term.
Require Import L2.program.
Require Import L2.wndEval.
Require Import L2.wcbvEval.

Local Open Scope string_scope.
Local Open Scope bool.
Local Open Scope list.
Local Open Scope program_scope.
Set Implicit Arguments.

(** Weak typed normal form: normal form of wndEval:
*** no wndEval steps possible (including no steps in type fields.
**)
Inductive WNorm: Term -> Prop :=
| WNRel: forall n, WNorm (TRel n)
| WNLam: forall nm bod, WNorm (TLambda nm bod)
| WNProd: forall nm bod, WNorm (TProd nm bod)
| WNFix: forall ds br, WNorm (TFix ds br)
| WNCase: forall mch n brs,
            WNorm mch -> WNorms brs -> ~ isCanonical mch ->
            WNorm (TCase n mch brs)
| WNConstruct: forall i n, WNorm (TConstruct i n)
| WNInd: forall i, WNorm (TInd i)
| WNSort: forall s, WNorm (TSort s)
| WNApp: forall fn t ts,
           WNorm fn -> WNorm t -> WNorms ts ->
           ~ (isLambda fn) -> ~ (isFix fn) -> ~ (isApp fn) ->
           WNorm (TApp fn t ts)
with WNorms: Terms -> Prop :=
| WNtnil: WNorms tnil
| WNtcons: forall t ts, WNorm t -> WNorms ts -> WNorms (tcons t ts).
Hint Constructors WNorm WNorms.
Scheme WNorm_ind' := Induction for WNorm Sort Prop
  with WNorms_ind' := Induction for WNorms Sort Prop.
Combined Scheme WNormWNorms_ind from WNorm_ind', WNorms_ind'.


(** WNorm is decidable **)
Lemma WNorm_dec: 
  (forall t, WNorm t \/ ~ WNorm t) /\
  (forall ts, WNorms ts \/ ~ WNorms ts) /\
  (forall (ds:Defs), True).
Ltac rght := right; intros h; inversion_Clear h; contradiction.
Ltac lft := solve [left; constructor; assumption].
apply TrmTrmsDefs_ind; intros; auto.
- right. intros h. inversion h.
- destruct H; rght.
- destruct (isLambda_dec t). rght.
  destruct (isFix_dec t). rght.
  destruct (isApp_dec t). rght.
  destruct H, H0, H1; try rght.
  + left. apply WNApp; auto.
- rght.
- destruct H; destruct H0; try rght.
  + destruct (isCanonical_dec t).
    * right. inversion H1; intros h; inversion h; subst; contradiction.
    * left. constructor; auto.
- destruct H; destruct H0;
  try (solve [right; intros h; inversion_Clear h; contradiction]).
  + left; constructor; auto.
Qed.

Lemma WNorms_tappendl:
  forall ts us, WNorms (tappend ts us) -> WNorms ts.
Proof.
  induction ts; intros us h.
  - constructor.
  - simpl in h. apply WNtcons; inversion_Clear h.
    + assumption.
    + eapply IHts. eassumption.
Qed.


Lemma Wcbv_WNorm:
  forall p, WFaEnv p ->
    (forall t s, WcbvEval p t s -> WFapp t -> WNorm s) /\
    (forall ts ss, WcbvEvals p ts ss -> WFapps ts -> WNorms ss).
Proof.
intros p hp. apply WcbvEvalEvals_ind; simpl; intros; auto.
- inversion_Clear H0. apply H. assumption.
- inversion_Clear H0. apply H.
  assert (j:= Lookup_pres_WFapp hp l). inversion j. assumption.
- inversion_Clear H2. apply H1. 
  assert (j:= proj1 (wcbvEval_pres_WFapp hp) _ _ w H7). inversion_Clear j.
  apply whBetaStep_pres_WFapp; try assumption.
  eapply (proj1 (wcbvEval_pres_WFapp hp)); eassumption. 
- inversion_Clear H1. apply H0. apply instantiate_pres_WFapp. assumption. 
  apply (proj1 (wcbvEval_pres_WFapp hp) _ _ w). assumption.
- inversion_Clear H1. apply H0. 
  refine (whFixStep_pres_WFapp _ _ _ e). 
  + assert (j:= proj1 (wcbvEval_pres_WFapp hp) _ _ w H6).
    inversion j. assumption.
  + constructor; assumption.
- inversion_Clear H2. constructor; intuition.
  + destruct H2 as [x0 [x1 j]]. discriminate.
  + destruct H2 as [x0 [x1 j]]. discriminate.
  + destruct H2 as [x0 [x1 [x2 j]]]. discriminate.
- inversion_Clear H2. constructor; intuition.
  + destruct H2 as [x0 [x1 j]]. discriminate.
  + destruct H2 as [x0 [x1 j]]. discriminate.
  + destruct H2 as [x0 [x1 [x2 j]]]. discriminate.
- inversion_Clear H1. apply H0.
  refine (whCaseStep_pres_WFapp _ _ _ e); auto.
- inversion_Clear H1. apply H0.
  refine (whCaseStep_pres_WFapp _ _ _ e0). auto.
  refine (tskipn_pres_WFapp _ _ e).
  assert (j:= proj1 (wcbvEval_pres_WFapp hp) _ _ w H4). inversion j.
  constructor; assumption.
- inversion_Clear H1. constructor; intuition.
Qed.


Lemma wcbvEval_no_further:
  forall p, 
    (forall t s, WcbvEval p t s -> WcbvEval p s s) /\
    (forall ts ss, WcbvEvals p ts ss -> WcbvEvals p ss ss).
Proof.
  intros p. apply WcbvEvalEvals_ind; simpl; intros; auto.
Qed.

(** If a program is in weak normal form, it has no wndEval step **)
Lemma wNorm_no_wndStep_lem:
  forall (p:environ),
  (forall t s, wndEval p t s -> ~ WNorm t) /\
  (forall ts ss, wndEvals p ts ss -> ~ WNorms ts).
intros p.
apply wndEvalEvals_ind; intros; intros h;
try (solve[inversion h]);
try (solve[inversion h; subst; contradiction]).
- inversion h. subst. elim H5. exists nm, bod. reflexivity.
- inversion h. subst. elim H4. constructor.
- inversion h. subst. elim H4. constructor.
- inversion h. subst. elim H6. exists dts, m. reflexivity.
- destruct t; simpl in h; try (solve [elim H; constructor]).
  + inversion w. subst.
    * inversion h. subst. inversion H3.
    * inversion h. subst. inversion H6.
  + inversion h. subst. contradiction.
  + elim H. inversion h. subst. assert (j:= WNorms_tappendl _ _ H5).
    constructor; try assumption.
  + inversion h. subst. contradiction.
  + inversion h. subst. inversion H3. contradiction.
Qed.

Lemma wNorm_no_wndStep:
  forall t, WNorm t -> forall p, no_wnd_step p t.
unfold no_wnd_step, no_wnds_step, no_step. intros t h0 p b h1.
elim (proj1 (wNorm_no_wndStep_lem p) _ _ h1). assumption.
Qed.


(***************
Lemma wNorm_no_wndStep:
  forall p,
    (forall t, WNorm t -> no_wnd_step p t) /\
    (forall ts, WNorms ts -> no_wnds_step p ts).
intros p; apply WNormWNorms_ind; intros;
unfold no_wnd_step, no_wnds_step;
intros x h; inversion_Clear h; auto;
try (solve [elim (H _ H4)]);
try (solve [elim (H _ H6)]);
try (solve [elim (H0 _ H6)]).
- eelim H. eassumption.
- eelim H0. eassumption.
- eelim H1. eassumption. 
- eelim H0. eassumption.
Qed.


(** If a program is in weak normal form, it WcbvEval to itself **)
Lemma pre_wNorm_WcbvEval_rfl:
  forall p,
    (forall t, WNorm t -> forall s, WcbvEval p t s -> t = s) /\
    (forall ts, WNorms ts -> forall ss, WcbvEvals p ts ss -> ts = ss).
intros p; apply WNormWNorms_ind; intros; auto; 
try (solve [inversion H; reflexivity]).
- inversion_Clear H1; rewrite (H _ H5) in n0; elim n0; constructor.
- inversion_Clear H2.
  + rewrite (H _ H6) in n. elim n. exists nm, bod. reflexivity.
  + rewrite (H _ H6) in n0. elim n0. exists dts, m. reflexivity.
  + rewrite (H _ H6).  in H12. rewrite (H1 _ H9). rewrite (H0 _ H8). reflexivity.
- inversion_Clear H1. rewrite (H _ H4). rewrite (H0 _ H6). reflexivity.
Qed.
***)

(***
Lemma wNorm_WcbvEval_rfl:
  forall p,
    (forall t, WNorm t -> WcbvEval p t t) /\
    (forall ts, WNorms ts -> WcbvEvals p ts ts).
intros p.
assert (j:= pre_wNorm_WcbvEval_rfl p).
split; intros.
Check (proj1 j _ H).
***)

(*** HERE; use Crct premise
(** If WcbvEval p t t, then t is in weak normal form **)
Goal
  forall (p:environ),
    (forall t s, WcbvEval p t s -> t = s -> WNorm t) /\
    (forall ts ss, WcbvEvals p ts ss -> ts = ss -> WNorms ts).
intros p. apply WcbvEvalEvals_ind; intros; try (solve [constructor]).
- subst. inversion_Clear w.
***)


(** If wndEval cannot step a correct program, then it is in weak normal form:
*** This claim is false now; e.g. whFixStep or whCaseStep may fail.
*** So we comment out this lemma.
**
Lemma no_wndStep_wNorm:
  (forall p n t, Crct p n t -> n = 0 -> no_wnd_step p t -> WNorm t) /\
  (forall p n ts, Crcts p n ts -> n = 0 ->
                  no_wnds_step p ts -> WNorms ts) /\
  (forall (p:environ) (n:nat) (ds:Defs), CrctDs p n ds -> True).
apply (CrctCrctsCrctDs_ind); intros; try auto; try (solve [constructor]).
- apply H0. assumption. intros sx h. elim (H5 sx). destruct (wndEval_weaken p).
  apply H6; assumption.
- rewrite H2 in H. omega.
- apply WNCast.
  + apply H0. trivial. intros s j. 
    elim (H4 (TCast s ck ty)). apply (sCastTrm _ _ j).
  + apply H2. trivial. intros s j.
    elim (H4 (TCast t ck s)). apply (sCastTy _ _ j).
- apply WNProd. apply H2. trivial. intros s j. elim (H4 (TProd nm s bod)).
  apply sProdTy. trivial.
- apply WNLam. apply H2. trivial. intros s j. elim (H4 (TLambda nm s bod)).
  apply sLamTy. trivial.
- elim (H6 (instantiate dfn 0 bod)). apply sLetIn.
- apply WNApp. apply H0. trivial. intros s j. elim (H6 (TApp s a args)).
  + apply sAppFn. assumption.
  + apply (H2 H5). intros s j. elim (H6 (TApp fn s args)).
    apply sAppArg. assumption.
  + induction args. auto. apply (H4 H5). intros sx j. inversion_clear j.
    * elim (H6 (TApp fn a (tcons r args))). apply sAppArgs. apply saHd. auto.
    * elim (H6 (TApp fn a (tcons t ss))). apply sAppArgs. apply saTl. auto.
  + intros h. destruct h. destruct H7. destruct H7.
    elim (H6 (whBetaStep x1 a args)). rewrite H7. apply sBeta.
  + intros h. destruct h. destruct H7. rewrite H7 in H6.
    unfold no_wnd_step in H6. elim H6. admit.
(**    eelim (H6). rewrite H7. apply sFix. unfold whFixStep. destruct (whFixStep x x0 (tcons a args)). ereflexivity. rewrite H7. apply sFix.
**)
- eelim (H3). apply sConst. eassumption. 
- subst. apply WNCase.
  + apply H0. reflexivity. unfold no_wnd_step. intros s h. eelim H6. 
    eapply wndEvalRTC_Case_mch.

unfold no_wnd_step in H6.
- subst. destruct (isCanonical_dec mch).
  + inversion H5. edestruct H6. rewrite <- H7. apply sCase0. unfold whCaseStep. simpl. [apply sCase0 | apply sCasen].
  + constructor; intuition.
    * apply H7. intros s h. elim (H6 (TCase m ty s brs)). apply sCaseArg. 
      assumption.
    * apply H2. intros s h. elim (H6 (TCase m s mch brs)). apply sCaseTy. 
      assumption.
    * apply H0. intros s h. elim (H6 (TCase m ty mch s)). apply sCaseBrs.
      assumption.
- subst. intuition. constructor.
  + apply H3. intros s h. elim (H4 (tcons s ts)). apply saHd. assumption.
  + apply H0. intros s h. elim (H4 (tcons t s)). apply saTl. assumption.
Qed.
***)

(**********************
(** if WcbvEval p t1 t2 then t2 is in weak normal form ***)
Goal 
  (forall p n t, Crct p n t -> n = 0 -> forall s, WcbvEval p t s -> WNorm s) /\
  (forall p n ts, Crcts p n ts -> n = 0 ->
                     forall ss, WcbvEvals p ts ss -> WNorms ss) /\
  (forall (p:environ) (n:nat) (ds:Defs), CrctDs p n ds -> True).
apply (CrctCrctsCrctDs_ind); intros; subst; try auto.
- inversion_Clear H0. constructor.
- apply H0; auto.



forall p n t1 t2, Crct p n t1 -> n = 0 -> WcbvEval p t1 t2 -> WNorm t2.
intros.


 apply (proj1 (no_wndStep_wNorm) p n t1). 

***********************)