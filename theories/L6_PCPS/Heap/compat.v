(* Compatibility lemmas for logical relations.
 * Part of the CertiCoq project.
 * Author: Zoe Paraskevopoulou, 2017
 *)

From Coq Require Import NArith.BinNat Relations.Relations MSets.MSets
                        MSets.MSetRBT Lists.List omega.Omega Sets.Ensembles.
From L6 Require Import functions cps ctx eval cps_util identifiers ctx Ensembles_util
     List_util Heap.heap Heap.heap_defs Heap.space_sem Heap.closure_conversion
     Heap.cc_log_rel tactics set_util.
From compcert Require Import lib.Coqlib.

Import ListNotations.

Module Compat (H : Heap).

  Module LR := CC_log_rel H.
  
  Import H LR LR.Sem LR.Sem.Equiv LR.Sem.Equiv.Defs LR.Sem.Equiv.Defs.HL.
  
  Section CompatDefs.
    
    Variable (clo_tag : cTag).

    Context (IG : GInv) (* Final global *)
            (IL1 IL2: Inv) (* Final local *)

            (IIG : GIInv) (* Global initial *)
            (IIL1 IIL2 : IInv) (* Local initial *)

            (M : nat) (* memory cost factor *)
            (F : nat) (* time cost factor *).


    (** * Base case and timout compatibility *)

    Definition InvCostBase (H1 H2 : heap block) (rho1 rho2 : env) (e1 e2 : exp) :=
      forall (H1' H2' : heap block) (rho1' rho2' : env) (c : nat) β1 β2,
        (occurs_free e1) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free e1))) β1 ->
           
        (occurs_free e2) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free e2))) β2 ->
                           
        IIL1 (H1', rho1', e1) (H2', rho2', e2) ->
                           
        IL1 (H1', rho1', e1, c, size_heap H1') (H2', rho2', e2, c, size_heap H2').

    (** * Constructor application compatibility *) 

    Definition InvCtxCompat (H1 H2 : heap block) (rho1 rho2 : env) (C1 C2 : exp_ctx) (e1 e2 : exp)  :=
      forall (H1' H2' H1'' H2'' : heap block) (rho1' rho2' rho1'' rho2'' : env) c1 c2 c1' c2' m1 m2  β1 β2,
        (occurs_free (C1 |[ e1 ]|)) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (C1 |[ e1 ]|)))) β1 ->
           
        (occurs_free (C2 |[ e2 ]|)) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (C2 |[ e2 ]|)))) β2 ->

        IL2 (H1'', rho1'', e1, c1, m1) (H2'', rho2'', e2, c2, m2) ->
        
        ctx_to_heap_env C1 H1' rho1' H1'' rho1'' c1' ->
        ctx_to_heap_env_CC C2 H2' rho2' H2'' rho2'' c2' ->
        
        IL1 (H1', rho1', C1 |[ e1 ]|, c1 + c1', m1) (H2', rho2', C2 |[ e2 ]|, c2 + c2', m2).
    
    Definition IInvCtxCompat (H1 H2 : heap block) (rho1 rho2 : env) (C1 C2 : exp_ctx) (e1 e2 : exp)  :=
      forall (H1' H2' H1'' H2'' : heap block) (rho1' rho2' rho1'' rho2'' : env) c1' c2' β1 β2,
        (occurs_free (C1 |[ e1 ]|)) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (C1 |[ e1 ]|)))) β1 ->
           
        (occurs_free (C2 |[ e2 ]|)) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (C2 |[ e2 ]|)))) β2 ->                           

        IIL1 (H1', rho1', C1 |[ e1 ]|) (H2', rho2', C2 |[ e2 ]|) ->
            
        ctx_to_heap_env C1 H1' rho1' H1'' rho1'' c1' ->
        ctx_to_heap_env_CC C2 H2' rho2' H2'' rho2'' c2' ->

        IIL2 (H1'', rho1'', e1) (H2'', rho2'', e2).

    Definition InvCtxCompat_r (H1 H2 : heap block) (rho1 rho2 : env) (C : exp_ctx) (e1 e2 : exp) :=
      forall (H1' H2' H1'' H2'' : heap block) (rho1' rho2' rho1'' rho2'' : env) c' c1 c2 m1 m2 β1 β2,
        (occurs_free e1) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free e1))) β1 ->
           
        (occurs_free (C |[ e2 ]|)) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (C |[ e2 ]|)))) β2 ->
                                      
        IL2 (H1'', rho1'', e1, c1, m1) (H2'', rho2'', e2, c2, m2) ->
        
        ctx_to_heap_env_CC C H2' rho2' H2'' rho2'' c' ->
        
        IL1 (H1', rho1', e1, c1, m1) (H2', rho2', C |[ e2 ]|, c2 + c', m2).

    Definition IInvCtxCompat_r (H1 H2 : heap block) (rho1 rho2 : env) (C : exp_ctx) (e1 e2 : exp) :=
      forall (H1' H2' H1'' H2'' : heap block) (rho1' rho2' rho1'' rho2'' : env) c' β1 β2,
        (occurs_free e1) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free e1))) β1 ->
           
        (occurs_free (C |[ e2 ]|)) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (C |[ e2 ]|)))) β2 ->
                                   
        IIL1 (H1', rho1', e1) (H2', rho2', C |[ e2 ]|) ->
            
        ctx_to_heap_env_CC C H2' rho2' H2'' rho2'' c' ->

        IIL2 (H1'', rho1'', e1) (H2'', rho2'', e2).


    (** * Case compatibility *)
    
    Definition IInvCaseHdCompat (H1 H2 : heap block) (rho1 rho2 : env) x1 x2 t Pats1 Pats2 (e1 e2 : exp) :=
      (forall H1' rho1' H2' rho2' β1 β2,
         (occurs_free (Ecase x1 ((t, e1) :: Pats1))) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
         injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (Ecase x1 ((t, e1) :: Pats1))))) β1 ->
           
         (occurs_free (Ecase x2 ((t, e2) :: Pats2))) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
         injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (Ecase x2 ((t, e2) :: Pats2))))) β2 ->

         IIL1 (H1', rho1', Ecase x1 ((t, e1) :: Pats1)) (H2', rho2', Ecase x2 ((t, e2) :: Pats2)) ->
         IIL2 (H1', rho1', e1) (H2', rho2', e2)).

      Definition IInvCaseTlCompat (H1 H2 : heap block) (rho1 rho2 : env)  x1 x2 t Pats1 Pats2 (e1 e2 : exp) :=
        (forall H1' rho1' H2' rho2' β1 β2,
         (occurs_free (Ecase x1 ((t, e1) :: Pats1))) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
         injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (Ecase x1 ((t, e1) :: Pats1))))) β1 ->
           
         (occurs_free (Ecase x2 ((t, e2) :: Pats2))) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
         injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (Ecase x2 ((t, e2) :: Pats2))))) β2 ->
                                                       
         IIL1 (H1', rho1', Ecase x1 ((t, e1) :: Pats1)) (H2', rho2', Ecase x2 ((t, e2) :: Pats2)) ->
         IIL1 (H1', rho1', Ecase x1 Pats1) (H2', rho2', Ecase x2 Pats2)).

      Definition InvCaseHdCompat (H1 H2 : heap block) (rho1 rho2 : env) x1 x2 t Pats1 Pats2 (e1 e2 : exp) :=
        (forall H1' rho1' H2' rho2' β1 β2 c1 c2 m1 m2,
           (occurs_free (Ecase x1 ((t, e1) :: Pats1))) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
           injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (Ecase x1 ((t, e1) :: Pats1))))) β1 ->
           
           (occurs_free (Ecase x2 ((t, e2) :: Pats2))) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
           injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (Ecase x2 ((t, e2) :: Pats2))))) β2 ->

           IL2 (H1', rho1', e1, c1, m1) (H2', rho2', e2, c2, m2)  ->
           IL1 (H1', rho1', Ecase x1 ((t, e1) :: Pats1), c1 + cost (Ecase x1 ((t, e1) :: Pats1)), m1)
               (H2', rho2', Ecase x2 ((t, e2) :: Pats2), c2 + cost (Ecase x2 ((t, e2) :: Pats2)), m2)).

      Definition InvCaseTlCompat (H1 H2 : heap block) (rho1 rho2 : env) x1 x2 t Pats1 Pats2 (e1 e2 : exp) :=
        (forall H1' rho1' H2' rho2' β1 β2 c1 c2 m1 m2,
           (occurs_free (Ecase x1 ((t, e1) :: Pats1))) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
           injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (Ecase x1 ((t, e1) :: Pats1))))) β1 ->
           
           (occurs_free (Ecase x2 ((t, e2) :: Pats2))) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
           injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (Ecase x2 ((t, e2) :: Pats2))))) β2 ->

           IL1 (H1', rho1', Ecase x1 Pats1, c1, m1) (H2', rho2', Ecase x2 Pats2, c2, m2) ->
           IL1 (H1', rho1', Ecase x1 ((t, e1) :: Pats1), c1, m1) (H2', rho2', Ecase x2 ((t, e2) :: Pats2), c2, m2)).
     
    (** * App compatibility *)

    Definition IInvGC :=
      forall (H1 H1' H2 H2' : heap block) (rho1 rho2 : env) (e1 e2 : exp),
        IIG (H1, rho1, e1) (H2, rho2, e2) ->
        live (env_locs rho1 (occurs_free e1)) H1 H1' ->
        live' (env_locs rho2 (occurs_free e2)) H2 H2' ->
        IIG (H1', rho1, e1) (H2', rho2, e2).

    Definition InvGC :=
      forall (H1 H1' H2 H2' : heap block) (rho1 rho2 : env) (e1 e2 : exp) c1 c2 m1 m2 k,
        IG k (H1', rho1, e1, c1, m1) (H2', rho2, e2, c2, m2) ->
        live (env_locs rho1 (occurs_free e1)) H1 H1' ->
        live' (env_locs rho2 (occurs_free e2)) H2 H2' ->
        IG k (H1, rho1, e1, c1, m1) (H2, rho2, e2, c2, m2).

    Definition IInvAppCompat (H1 H2 : heap block) (rho1 rho2 : env) f1 t xs1 f2 xs2 f2' Γ :=   
      forall (i  : nat) (H1' H1'' H2' : heap block)
        (rho_clo rho_clo1 rho_clo2 rho1' rho2' rho2'' : env) β1 β2 
        (B1 : fundefs) (f1' : var) (ct1 : cTag)
        (xs1' : list var) (e1 : exp) (l1 env_loc1 : loc)
        (vs1 : list value) 
        (B2 : fundefs) (f3 : var) (c ct2 : cTag) (xs2' : list var) 
        (e2 : exp) (l2 env_loc2 : loc) (vs2 : list value) c1 c2 m1 m2,
        (occurs_free (Eapp f1 t xs1)) |- (H1, rho1) ⩪_(id, β1) (H1', rho1') ->
        injective_subdomain (reach' H1' (env_locs rho1' (occurs_free (Eapp f1 t xs1)))) β1 ->
           
        (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ)) |- (H2, rho2) ⩪_(β2, id) (H2', rho2') ->
        injective_subdomain (reach' H2 (env_locs rho2 (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ)))) β2 ->

        
        IG i (H1'', rho_clo2, e1, c1, m1) (H2', rho2'', e2, c2, m2) ->
        IIL1 (H1', rho1', Eapp f1 t xs1) (H2', rho2', AppClo clo_tag f2 t xs2 f2' Γ) ->
        
        M.get f1 rho1' = Some (Loc l1) ->
        get l1 H1' = Some (Clos (FunPtr B1 f1') (Loc env_loc1)) ->
        get env_loc1 H1' = Some (Env rho_clo) ->
        find_def f1' B1 = Some (ct1, xs1', e1) ->
        getlist xs1 rho1' = Some vs1 ->
        def_closures B1 B1 rho_clo H1' env_loc1 = (H1'', rho_clo1) ->
        setlist xs1' vs1 rho_clo1 = Some rho_clo2 ->
        
        
        M.get f2 rho2' = Some (Loc l2) ->
        getlist xs2 rho2' = Some vs2 ->
        get l2 H2' = Some (Constr c [FunPtr B2 f3; Loc env_loc2]) ->
        Some rho2'' =
        setlist xs2' (Loc env_loc2 :: vs2) (def_funs B2 B2 (M.empty value)) ->
        find_def f3 B2 = Some (ct2, xs2', e2) ->
        
        IL1 (H1', rho1', Eapp f1 t xs1, c1 + cost (Eapp f1 t xs1), max m1 (size_heap H1''))
           (H2', rho2', AppClo clo_tag f2 t xs2 f2' Γ, c2 + 1 + 1 + cost (Eapp f2' t (Γ :: xs2)), max m2 (size_heap H2')).
    
  End CompatDefs.

  Section CompatLemmas.
    
    Variable (clo_tag : cTag).

    Context (IG : GInv) (* Final global *)
            (IL1 IL2: Inv) (* Final local *)

            (IIG : GIInv) (* Global initial *)
            (IIL1 IIL2 : IInv). (* Local initial *)

    (* TODO move *)
    Lemma live_deterministic S (_ : set_util.ToMSet S) H1 H2 H2' :
      live S H1 H2 ->
      live S H1 H2' ->
      (exists b1 b2, S |- H2 ≃_(b1, b2) H2' /\
      injective_subdomain (reach' H2 S) b1 /\
      injective_subdomain (reach' H2' S) b2).
    Proof.
      intros [Hyp1 [b1 [Hyp2 Hinj1]]] [Hyp1' [b2 [Hyp2' Hinj2]]].
      eexists b1, b2. symmetry in Hyp2. 
      rewrite <- (Combinators.compose_id_right _ _ id) in Hyp2.
      rewrite <- (Combinators.compose_id_left _ _ b2).
      split; [| split ]; eauto.
      eapply heap_equiv_f_compose; [| eassumption ]. eassumption.
    Qed.


        
    (** Application compatibility *)
    Lemma cc_approx_exp_app_compat (k j : nat) (b : Inj) (H1 H2 : heap block)
          (rho1 rho2 : env) (f1 : var) (xs1 : list var)
          (f2 f2' Γ : var) (xs2 : list var) (t : fTag) :
      IInvAppCompat clo_tag IG IL1 IIL1 H1 H2 rho1 rho2 f1 t xs1 f2 xs2 f2' Γ ->
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Eapp f1 t xs1) (AppClo clo_tag f2 t xs2 f2' Γ) ->
      IInvGC IIG -> InvGC IG ->

      well_formed (reach' H1 (env_locs rho1 (occurs_free (Eapp f1 t xs1)))) H1 ->
      well_formed (reach' H2 (env_locs rho2 (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ)))) H2 ->
      (env_locs rho1 (occurs_free (Eapp f1 t xs1))) \subset dom H1 ->
      (env_locs rho2 (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ))) \subset dom H2 ->
      
      ~ Γ \in (f2 |: FromList xs2) ->
      ~ f2' \in (f2 |: FromList xs2) ->
      Γ <> f2' ->
                
      cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2 f1 f2 ->
      Forall2 (cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2) xs1 xs2 ->

      (Eapp f1 t xs1, rho1, H1) ⪯ ^ (k ; j; IIL1 ; IIG
                                     ; IL1
                                     ; IG)
      (AppClo clo_tag f2 t xs2 f2' Γ, rho2, H2).
    Proof with now eauto with Ensembles_DB.
      intros Hiinv Hbase Hiigc Higc Hwf1 Hwf2 Hs1 Hs2 Hnin1 Hnin2 Hneq  Hvar Hall
             b1 b2 H1' H2' rho1' rho2' v1 c1 m1 Heq1 Hinj1 Heq2 Hinj2
             HII Hleq1 Hstep1 Hstuck1.
      eapply (cc_approx_var_env_heap_env_equiv
                _ _ 
                (occurs_free (Eapp f1 t xs1))
                (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ))) in Hvar;
          [| eassumption | eassumption | eassumption | eassumption 
           | normalize_occurs_free; now eauto with Ensembles_DB
           | unfold AppClo; normalize_occurs_free; now eauto with Ensembles_DB ].
      inv Hstep1. 
      (* Timeout! *)
      - { edestruct (Hstuck1 (cost (Eapp f1 t xs1))) as [v1 [m1 Hstep1]].
          inv Hstep1; [ omega | ].
          exists OOT, c1. destruct (lt_dec c1 1).
          - eexists. eexists id. repeat split. now constructor; eauto.
            now eapply injective_subdomain_Empty_set. rewrite <- plus_n_O.
            eapply Hbase; try eassumption.
            now rewrite cc_approx_val_eq.
          - edestruct Hvar as [l2 [Hget' Hcc]]; eauto.
            simpl in Hcc. rewrite Hgetl in Hcc. destruct l2 as [l2 | ]; [| contradiction ].
            destruct (get l2 H2') as [v |] eqn:Hget2; try contradiction.
            destruct Hcc as [Hbeq Hcc]; subst; simpl in Hcc.
            destruct v as [ ? [| [| B2 f3 ] [| [ env_loc' |] [|] ]] | | ]; try contradiction.
            destruct Hcc as [Him Hcc].
            edestruct Hcc with (vs2 := vs) as (xs2' & e2 & rho2'' & Hfind' & Hset' & Hi'); try eassumption.
            reflexivity. now firstorder. symmetry.
            reflexivity. now firstorder.
            destruct (lt_dec (c1 - 1) 1).
            + eexists. eexists id. repeat split. unfold AppClo.
              eapply Eval_proj_per_cc; eauto.
              simpl. omega. reflexivity.
              now econstructor; simpl; eauto.
              eapply injective_subdomain_Empty_set.
              rewrite <- !plus_n_O.
              eapply Hbase; try eassumption.
              rewrite cc_approx_val_eq. now eauto.
            + eexists. eexists id. repeat split. unfold AppClo.
              eapply Eval_proj_per_cc; eauto.
              simpl. omega. reflexivity.
              eapply Eval_proj_per_cc; eauto.
              simpl. omega.
              rewrite M.gso. eassumption.
              intros Heq; subst. now eauto with Ensembles_DB.
              reflexivity. 
              econstructor; try (now simpl; eauto). simpl.
              erewrite <- Forall2_length; [| eassumption ]. simpl in *. omega.
              now eapply injective_subdomain_Empty_set.
              rewrite <- plus_n_O.
              eapply Hbase; try eassumption.
              now rewrite cc_approx_val_eq. }
      (* Termination *) 
      - { simpl in Hcost.
          eapply Forall2_monotonic_strong in Hall; (* yiiiiiikes *)
            [| intros x1 x2 Hin1 Hin2 Hyp;
               eapply (cc_approx_var_env_heap_env_equiv
                         _ _
                         (occurs_free (Eapp f1 t xs1))
                         (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ))) in Hyp;
               [ now eapply Hyp | eassumption | eassumption | eassumption | eassumption
                 | normalize_occurs_free; now eauto with Ensembles_DB
                 | unfold AppClo; repeat normalize_occurs_free; rewrite FromList_cons ];
               right; constructor;
               [ right; constructor;
               [ now eauto with Ensembles_DB
               | now intros Hc; inv Hc; eapply Hnin1; eauto ]
               | now intros Hc; inv Hc; eapply Hnin2; eauto ] ].
          edestruct (cc_approx_var_env_getlist IIG IG  k j rho1' rho2') as [vs2 [Hgetl' Hcc']];
            [ eassumption | now eauto |].
          edestruct Hvar as [l2 [Hget' Hcc]]; eauto.
          simpl in Hcc. rewrite Hgetl in Hcc. destruct l2 as [l2 | ]; [| contradiction ]. 
          destruct (get l2 H2') as [v |] eqn:Hget2; try contradiction.
          destruct Hcc as [Hbeq Hcc]. simpl in Hcc.
          destruct v as [ ? [| [| B2 f3 ] [| [ env_loc' |] [|] ]] | | ]; try contradiction.
          edestruct Hcc as (Him & xs2' & e2 & rho2'' & Hfind' & Hset' & Hi'); try eassumption.
          reflexivity. now firstorder. reflexivity. now firstorder.
          edestruct (live_exists' (env_locs rho2'' (occurs_free e2)) H2') as [H2'' Hgc'].
          assert (Hgc1 := Hgc); assert (Hgc2 := Hgc').
          destruct Hgc as [Hseq [bgc [Heqgc Hinjgc]]].
          destruct Hgc' as [Hseq' [bgc' [Heqgc' Hinjgc']]].
          edestruct Hi' with (i := k - cost  (Eapp f1 t xs1))
                               (j' := j) as [HG [r2 [c2 [m2 [b2' [Hbs2 [Hinj [HIG Hcc2]]]]]]]];
            [ | omega | | | | | | | | | eassumption | | ].  
          + simpl. omega.
          + rewrite compose_id_neut_r. rewrite compose_id_neut_l. reflexivity.
          + eapply Forall2_monotonic_strong; [| eassumption ].
            intros v v' Hinv1 Hinv2 Heq. rewrite cc_approx_val_eq.
            eapply cc_approx_val_monotonic with (k := k).
            assert (Hrv : val_loc v \subset env_locs rho1' (occurs_free (Eapp f1 t xs1))).
            { normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. rewrite env_locs_FromList.
              simpl. eapply In_Union_list.
              eapply in_map. eassumption. eassumption. }
            assert (Hrv' : val_loc v' \subset
                                   env_locs rho2' (occurs_free (AppClo clo_tag f2 t xs2 f2' Γ))).
            { unfold AppClo. repeat normalize_occurs_free.
              rewrite FromList_cons, !Setminus_Union_distr, !env_locs_Union.
              do 2 eapply Included_Union_preserv_r.
              eapply Included_Union_preserv_l. eapply Included_Union_preserv_r.
              rewrite !Setminus_Disjoint.
              rewrite env_locs_FromList.
              simpl. eapply In_Union_list.
              eapply in_map. eassumption. eassumption. 
              now eapply Disjoint_Singleton_r; intros Hc; eapply Hnin1; eauto.
              now eapply Disjoint_Singleton_r; intros Hc; inv Hc; eapply Hnin2; eauto. }
            eapply cc_approx_val_heap_monotonic;
              [ | | | | | now eapply HL.subheap_refl | ].
            * eapply well_formed_respects_heap_env_equiv in Hwf1; [| eassumption ].
              eapply well_formed_antimon; [| now apply Hwf1 ].
              now eapply reach'_set_monotonic.
            * eapply well_formed_respects_heap_env_equiv in Hwf2; [| eassumption ].
              eapply well_formed_antimon; [| now apply Hwf2 ].
              now eapply reach'_set_monotonic.
            * eapply Included_trans; [| eapply reachable_in_dom; try eassumption ].
              eapply Included_trans; [| now eapply reach'_extensive ].
              eassumption.
              eapply well_formed_respects_heap_env_equiv in Hwf1; [| eassumption ].
              eassumption.
              eapply env_locs_in_dom; eassumption.
            * eapply Included_trans; [| eapply reachable_in_dom; try eassumption ].
              eapply Included_trans; [| now eapply reach'_extensive ].
              eassumption.
              eapply well_formed_respects_heap_env_equiv in Hwf2; [| eassumption ].
              eassumption.
              eapply env_locs_in_dom; eassumption.
            * eapply def_funs_subheap. eauto.
            * eassumption.
            * simpl; omega.
          + eapply heap_env_equiv_heap_equiv. eassumption.
          + eassumption.
          + eapply heap_env_equiv_heap_equiv. eassumption.
          + eassumption.
          + eapply Hiigc; eassumption.
          + simpl; omega.
          + intros i.
            edestruct (Hstuck1 (i + cost (Eapp f1 t xs1))) as [r' [m' Hstep']].
            inv Hstep'.
            * omega.
            * rewrite NPeano.Nat.add_sub in Hbs0.
              repeat subst_exp. 
              edestruct live_deterministic as [b1' [b2' [Heq [Hinj1' Hinj2']]]].
              now apply Hgc1. now apply Hgc. try eassumption.
              edestruct big_step_gc_heap_env_equiv_r
                as [r1 [m1 [b1'' [b2''[Hgc'' _]]]]].
              eassumption.
              eapply heap_env_equiv_heap_equiv. symmetry. eassumption.
              eassumption. eassumption. do 2 eexists. eassumption.
          + do 3 eexists. exists b2'. repeat split.
            * eapply Eval_proj_per_cc with
              (c := c2 + 1 + 1 + cost (Eapp f2' t (Γ :: xs2))).
              simpl; omega.
              eassumption. eassumption. reflexivity.
              eapply Eval_proj_per_cc. simpl; omega.
              rewrite M.gso. eassumption.
              intros Hc. subst. eapply Hnin2. now left; eauto.
              eassumption. reflexivity. simpl.
              eapply Eval_app_per_cc.
              simpl. omega.
              rewrite M.gso. rewrite M.gss. reflexivity.
              now intros Hc; subst; eauto.
              eassumption.
              simpl. rewrite M.gss.
              rewrite !getlist_set_neq. now rewrite Hgetl'.
              intros Hc. eapply Hnin2. now eauto.
              intros Hc. eapply Hnin1. now eauto.
              now eauto. eassumption. reflexivity. simpl.
              replace (c2 + 1 + 1 + S (S (length xs2)) - 1 - 1 - S (S (length xs2)))  with c2.
              eassumption. omega.
            * eassumption.
            * replace c1 with (c1 - cost (Eapp f1 t xs1) + cost (Eapp f1 t xs1)) by (simpl in *; omega).
              eapply Hiinv; try eassumption.
              eapply Higc; eassumption.
            * rewrite cc_approx_val_eq in *. eapply cc_approx_val_monotonic.
              eassumption. simpl. omega. }        
    Qed.
    
    
    (** * Heap allocation and environment extension properties *)

    

      
    (** Constructor compatibility *)
    Lemma cc_approx_exp_constr_compat (k j : nat)
          (b : Inj) (H1 H2 : heap block)  (rho1 rho2 : env)
          (x1 x2 : var) (t : cTag) (ys1 ys2 : list var) (e1 e2 : exp)  : 
      InvCtxCompat IL1 IL2 H1 H2 rho1 rho2 (Econstr_c x1 t ys1 Hole_c) (Econstr_c x2 t ys2 Hole_c) e1 e2 ->
      IInvCtxCompat IIL1 IIL2 H1 H2 rho1 rho2 (Econstr_c x1 t ys1 Hole_c) (Econstr_c x2 t ys2 Hole_c) e1 e2 ->
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Econstr x1 t ys1 e1) (Econstr x2 t ys2 e2)  ->
      
      well_formed (reach' H1 (env_locs rho1 (occurs_free (Econstr x1 t ys1 e1)))) H1 ->
      well_formed (reach' H2 (env_locs rho2 (occurs_free (Econstr x2 t ys2 e2)))) H2 ->
      (env_locs rho1 (occurs_free (Econstr x1 t ys1 e1))) \subset dom H1 ->
      (env_locs rho2 (occurs_free (Econstr x2 t ys2 e2))) \subset dom H2 ->

      (forall j, Forall2 (cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2) ys1 ys2) ->

      (forall vs1 vs2 l1 l2 H1' H2',
         k >= cost (Econstr x1 t ys1 e1) ->
         (* allocate a new location for the constructed value *)
         alloc (Constr t vs1) H1 = (l1, H1') ->
         alloc (Constr t vs2) H2 = (l2, H2') ->
         (* new values already reachable *)
         locs (Constr t vs1) \subset env_locs rho1 (occurs_free (Econstr x1 t ys1 e1)) ->
         locs (Constr t vs2) \subset env_locs rho2 (occurs_free (Econstr x2 t ys2 e2)) ->
         (* and related *)
         (forall j, Forall2 (fun l1 l2 => (Res (l1, H1)) ≺ ^ (k - cost (Econstr x1 t ys1 e1) ; j ; IIG ; IG ; b) (Res (l2, H2))) vs1 vs2) ->
         (forall j, (e1, M.set x1 (Loc l1) rho1, H1') ⪯ ^ (k - cost (Econstr x1 t ys1 e1) ; j ; IIL2 ; IIG ; IL2 ; IG)
               (e2, M.set x2 (Loc l2) rho2, H2'))) ->
      
      (Econstr x1 t ys1 e1, rho1, H1) ⪯ ^ (k ; j ; IIL1; IIG ; IL1 ; IG) (Econstr x2 t ys2 e2, rho2, H2).
    Proof with now eauto with Ensembles_DB.
      intros Hinv Hiinv Hbase Hwf1 Hwf2 Hdom1 Hdom2 Hall Hpre b1 b2 H1' H2' rho1' rho2' v1 c1 m1
             Heq1 Hinj1 Heq2 Hinj2 HII Hleq1 Hstep1 Hstuck1.
      assert (Hall' := Hall).
      inv Hstep1.
      (* Timeout! *)
      - { exists OOT, c1. eexists. exists id. repeat split. 
          - econstructor. simpl. specialize (Hall 0). erewrite <- Forall2_length; [| eassumption ].
            simpl in Hcost. omega. reflexivity.
          - simpl. eapply injective_subdomain_Empty_set. 
          - eapply Hbase; try eassumption.
          - now rewrite cc_approx_val_eq. }
      (* Termination *)
      - { edestruct (cc_approx_var_env_getlist IIG IG k j rho1' rho2') as [vs2 [Hget' Hpre']];
          [| eauto |]; eauto.
          specialize (Hall j).
          eapply Forall2_monotonic_strong; [| eassumption ].
          intros x1' x2' Hin1 Hin2 Hvar.
          
          eapply cc_approx_var_env_heap_env_equiv; try eassumption.
          
          normalize_occurs_free... normalize_occurs_free...
          edestruct heap_env_equiv_env_getlist as [vs1' [Hget1' Hall1]];
            [| symmetry; now apply Heq1 | |]; try eassumption.
          normalize_occurs_free...
          edestruct heap_env_equiv_env_getlist as [vs2' [Hget2' Hall2]];
            [| symmetry; now apply Heq2 | |]; try eassumption.
          normalize_occurs_free...
          destruct (alloc (Constr t vs1') H1) as [l1 H1''] eqn:Hal1.
          destruct (alloc (Constr t vs2) H2') as [l2 H''] eqn:Hal2'.
          destruct (alloc (Constr t vs2') H2) as [l2' H2''] eqn:Hal2.
          assert (Halli := Hall). specialize (Hall j). eapply Forall2_length in Hall.
          assert (Hlen : @length M.elt ys1 = @length M.elt ys2).
          { erewrite (@getlist_length_eq value ys1 vs); [| eassumption ].
            erewrite (@getlist_length_eq value ys2 vs2); [| eassumption ].
            eapply Forall2_length. eassumption. }

          edestruct Hpre with (b1 := extend b1 l l1)
                              (b2 := extend b2 l2' l2)
            as [v2 [c2 [m2 [b' [Hstep [HS [Hinj Hval]]]]]]]; 
            [ | eassumption | eassumption | | | | | | | | | |  eassumption | | ].
          - simpl in *. omega.
          - normalize_occurs_free. rewrite env_locs_Union.
            eapply Included_Union_preserv_l. simpl.
            rewrite env_locs_FromList; eauto. reflexivity.
          - normalize_occurs_free. rewrite env_locs_Union.
            eapply Included_Union_preserv_l. simpl.
            rewrite env_locs_FromList; eauto. reflexivity.
          - intros j'.
            edestruct (cc_approx_var_env_getlist IIG IG k j' rho1 rho2) as [vs2'' [Hget'' Hall'']];
              [| eauto |]; eauto. subst_exp.
            eapply Forall2_monotonic; [| eassumption ]. intros ? ? H.
            eapply cc_approx_val_monotonic.
            now eapply H. omega.
          - eapply heap_env_equiv_alloc; [| | | | | | | eassumption | eassumption | | ].
            + eapply reach'_closed. eassumption. eassumption.
            + eapply reach'_closed.
              eapply well_formed_respects_heap_env_equiv.
              now apply Hwf1. eassumption.
              eapply env_locs_in_dom; eassumption.
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              eapply env_locs_monotonic. normalize_occurs_free...
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              eapply env_locs_monotonic. normalize_occurs_free...
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. simpl.
              rewrite env_locs_FromList; eauto. reflexivity.
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. simpl.
              rewrite env_locs_FromList; eauto. reflexivity.
            + eapply heap_env_equiv_antimon. eapply heap_env_equiv_rename_ext. 
              eassumption.
              reflexivity.

              eapply f_eq_subdomain_extend_not_In_S_r.
              intros Hr. eapply reachable_in_dom in Hr.
              eapply alloc_fresh in Halloc. destruct Hr as [s Hgetl]. congruence.
              eapply well_formed_respects_heap_env_equiv.
              now apply Hwf1. eassumption.
              eapply env_locs_in_dom; eassumption.
              reflexivity.
              normalize_occurs_free...
            + rewrite extend_gss. reflexivity.
            + simpl. split. reflexivity.

              eapply Forall2_symm_strong; [| eassumption ].
              intros l3 l4 Hin1 Hin2 Hin. simpl in Hin. symmetry in Hin.
              eapply res_equiv_rename_ext. eassumption.
              reflexivity.

              eapply f_eq_subdomain_extend_not_In_S_r.

              intros Hr. eapply reachable_in_dom in Hr.
              eapply alloc_fresh in Halloc. destruct Hr as [s Hgetl]. congruence. 

              eapply well_formed_antimon;
                [| eapply well_formed_respects_heap_env_equiv; (try now apply Hwf1); try eassumption ].
              eapply reach'_set_monotonic. simpl.
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l.

              rewrite env_locs_FromList; try eassumption. 
              eapply In_Union_list. eapply in_map. eassumption.
              eapply Included_trans; [| eapply env_locs_in_dom; eassumption ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l.
              rewrite env_locs_FromList; try eassumption. 
              eapply In_Union_list. eapply in_map. eassumption.
              
              reflexivity.
          - eapply injective_subdomain_antimon.
            eapply injective_subdomain_extend. eassumption.
            
            intros Hc. eapply image_monotonic in Hc; [| now eapply Setminus_Included ].  
            eapply heap_env_equiv_image_reach in Hc; try eassumption.
            eapply (image_id (reach' H1 (env_locs rho1 (occurs_free (Econstr x1 t ys1 e1)))))
              in Hc.
            eapply reachable_in_dom in Hc; try eassumption. destruct Hc as [v1' Hgetv1'].
            erewrite alloc_fresh in Hgetv1'; try eassumption. congruence.

            eapply Included_trans. eapply reach'_set_monotonic. eapply env_locs_monotonic.
            eapply occurs_free_Econstr_Included.
            eapply reach'_alloc_set; [| eassumption ]. 
            eapply Included_trans; [| eapply reach'_extensive ].
            normalize_occurs_free. rewrite env_locs_Union.
            eapply Included_Union_preserv_l. 
            rewrite env_locs_FromList; eauto. reflexivity.
          - eapply heap_env_equiv_alloc; [| | | | | | | now apply Hal2 | now apply Hal2' | | ].
            + eapply reach'_closed. eassumption. eassumption.
            + eapply reach'_closed.
              eapply well_formed_respects_heap_env_equiv.
              now apply Hwf2. eassumption.
              eapply env_locs_in_dom; eassumption.
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              eapply env_locs_monotonic. normalize_occurs_free...
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              eapply env_locs_monotonic. normalize_occurs_free...
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. simpl.
              rewrite env_locs_FromList; eauto. reflexivity.
            + eapply Included_trans; [ | now eapply reach'_extensive ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. simpl.
              rewrite env_locs_FromList; eauto. reflexivity.
            + eapply heap_env_equiv_antimon. eapply heap_env_equiv_rename_ext. 
              eassumption.

              eapply f_eq_subdomain_extend_not_In_S_r.
              intros Hr. eapply reachable_in_dom in Hr. 
              eapply alloc_fresh in Hal2. destruct Hr as [s Hgetl]. congruence.
              now apply Hwf2. eassumption. reflexivity. reflexivity.
              normalize_occurs_free...

            + rewrite extend_gss. reflexivity.  
            + symmetry. eapply block_equiv_rename_ext.
              split; eauto. reflexivity.

              eapply f_eq_subdomain_extend_not_In_S_r.
              intros Hr. eapply reachable_in_dom in Hr. 
              eapply alloc_fresh in Hal2. destruct Hr as [s Hgetl]. congruence.
              eapply well_formed_antimon; try eassumption.
              eapply reach'_set_monotonic.
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. 
              rewrite env_locs_FromList; eauto. reflexivity.

              eapply Included_trans; [| eassumption ].
              normalize_occurs_free. rewrite env_locs_Union.
              eapply Included_Union_preserv_l. 
              rewrite env_locs_FromList; eauto. reflexivity.

              reflexivity.

          - eapply injective_subdomain_antimon.
            eapply injective_subdomain_extend. eassumption.
            
            intros Hc. eapply image_monotonic in Hc; [| now eapply Setminus_Included ].  
            eapply heap_env_equiv_image_reach in Hc; try (symmetry; eassumption).
            eapply (image_id (reach' H2' (env_locs rho2' (occurs_free (Econstr x2 t ys2 e2)))))
              in Hc.
            eapply reachable_in_dom in Hc; try eassumption. destruct Hc as [v1' Hgetv1'].
            erewrite alloc_fresh in Hgetv1'; try eassumption. congruence.

            eapply well_formed_respects_heap_env_equiv. eassumption. eassumption.

            eapply Included_trans; [| eapply env_locs_in_dom; eassumption ].
            reflexivity.

            eapply Included_trans. eapply reach'_set_monotonic. eapply env_locs_monotonic.
            eapply occurs_free_Econstr_Included.
            eapply reach'_alloc_set; [| eassumption ]. 
            eapply Included_trans; [| eapply reach'_extensive ].
            normalize_occurs_free. rewrite env_locs_Union.
            eapply Included_Union_preserv_l. 
            rewrite env_locs_FromList; eauto. reflexivity.
              
          - eapply Hiinv; try eassumption.
            econstructor; eauto.
            now econstructor; eauto.
            econstructor; eauto.
            now econstructor; eauto.
          - simpl. simpl in Hcost. omega.
          - intros i. edestruct (Hstuck1 (i + cost (Econstr x1 t ys1 e1))) as [r' [m' Hstep']].
            inv Hstep'.
            * omega.
            * rewrite NPeano.Nat.add_sub in Hbs0. repeat subst_exp.
              repeat eexists; eauto.  
          - repeat eexists; eauto.
            + eapply Eval_constr_per_cc with (c := c2 + cost (Econstr x2 t ys2 e2))
              ; [ | | | rewrite NPeano.Nat.add_sub ]; try eassumption.
              simpl. omega. 
            + replace c1 with (c1 - cost (Econstr x1 t ys1 e1) + cost (Econstr x1 t ys1 e1))
                by ( simpl in *; omega). 
              eapply Hinv; try eassumption.
              replace (cost (Econstr x1 t ys1 e1)) with (0 + cost_ctx (Econstr_c x1 t ys1 Hole_c)) by (simpl; omega).
              econstructor; eauto. now econstructor; eauto.
              replace (cost (Econstr x2 t ys2 e2)) with (0 + cost_ctx (Econstr_c x2 t ys2  Hole_c)) by (simpl; omega).
              econstructor; eauto. now econstructor; eauto.
            + rewrite cc_approx_val_eq. eapply cc_approx_val_monotonic.
              rewrite <- cc_approx_val_eq. eassumption. simpl in *. omega. }
    Qed.
    
    (** Projection compatibility *)
    Lemma cc_approx_exp_proj_compat (k : nat) (H1 H2 : heap block) (rho1 rho2 : env) (b : Inj)
          (x1 x2 : var) (t : cTag) (n : N) (y1 y2 : var) (e1 e2 : exp) :

      InvCtxCompat IL1 IL2 H1 H2 rho1 rho2 (Eproj_c x1 t n y1 Hole_c) (Eproj_c x2 t n y2 Hole_c) e1 e2 ->
      IInvCtxCompat IIL1 IIL2 H1 H2 rho1 rho2 (Eproj_c x1 t n y1 Hole_c) (Eproj_c x2 t n y2 Hole_c) e1 e2 ->
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Eproj x1 t n y1 e1) (Eproj x2 t n y2 e2) ->
      
      (forall j, cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2 y1 y2) ->

      
      (forall v1 v2,
         (forall j, (Res (v1, H1)) ≺ ^ (k - cost (Eproj x1 t n y1 e1) ; j ; IIG ; IG; b) (Res (v2, H2))) ->
         (forall j, (e1, M.set x1 v1 rho1, H1) ⪯ ^ (k - cost (Eproj x1 t n y1 e1) ; j ; IIL2 ; IIG ; IL2 ; IG) (e2, M.set x2 v2 rho2, H2))) ->
      
      (forall j, (Eproj x1 t n y1 e1, rho1, H1) ⪯ ^ (k ; j ; IIL1 ; IIG ; IL1 ; IG) (Eproj x2 t n y2 e2, rho2, H2)).
    Proof with (now eauto with Ensembles_DB).
      intros Hinv Hiinv Hbase Hall Hpre j b1 b2 H1' H2' rho1' rho2' v1 c1 m1
             Heq1' Hinj1 Heq2' Hinj2 HII Hleq1 Hstep1 Hstuck. inv Hstep1.
      (* Timeout! *)
      - { simpl in Hcost. exists OOT, c1. eexists. exists id. repeat split. 
          - econstructor. simpl; omega. reflexivity.
          - eapply injective_subdomain_Empty_set.
          - eapply Hbase; eauto.
          - now rewrite cc_approx_val_eq. }
      (* Termination *)
      - { pose (cost1 := cost (Eproj x1 t n y1 e1)).
          pose (cost2 := cost (Eproj x2 t n y2 e2)).
          assert (Hall' := Hall).
          
          eapply (cc_approx_var_env_heap_env_equiv
                    _ _
                    (occurs_free (Eproj x1 t n y1 e1))
                    (occurs_free (Eproj x2 t n y2 e2)) _ (S j)) in Hall;
          [| eassumption | eassumption | eassumption | eassumption
           | normalize_occurs_free; now eauto with Ensembles_DB
           | normalize_occurs_free; now eauto with Ensembles_DB ].
          edestruct Hall as [l2 [Hget' Hcc']]; eauto.
          destruct l2 as [l' | l' f]; [| contradiction ].
          simpl in Hcc'. rewrite Hgetl in Hcc'.
          destruct (get l' H2') as [[ t2 vs' | | ] |] eqn:Hgetl';
            (try destruct Hcc' as [Hteq Hcc']); try contradiction.
          
          edestruct heap_env_equiv_env_get as [l1 [Hgetl1 Hres1]]; [ now apply Hgety | | | ].
          symmetry. eassumption. now eauto.
          edestruct heap_env_equiv_env_get as [l2 [Hgetl2 Hres2]]; [ now apply Hget' | | | ].
          symmetry. eassumption. now eauto.

          edestruct (Hall' (S j))  as [l2' [Hgetl2'' Hcc]]; eauto. repeat subst_exp. 
          
          assert (Hres1' := Hres1). assert (Hres2' := Hres2). rewrite res_equiv_eq in Hres1, Hres2.
          destruct l1 as [l1 |]; try contradiction.
          destruct l2' as [l2 |]; try contradiction.
          
          simpl in Hres1, Hres2. rewrite Hgetl in Hres1. rewrite Hgetl' in Hres2.
          destruct (get l1 H1) as [bl1 |] eqn:Hgetl1'; (try destruct Hres1 as [Hb1 Hres1]); try contradiction.
          destruct (get l2 H2) as [bl2 |] eqn:Hgetl2'; (try destruct Hres2 as [Hb2 Hres2]); try contradiction.
          destruct bl1 as [t1 vs1 | | ]; try contradiction.
          destruct bl2 as [t2' vs2 | | ]; try contradiction.
          destruct Hres1 as [Hteq Hallvs1]; subst. destruct Hres2 as [Hteq' Hallvs2]; subst.
          
          simpl in Hcc. rewrite Hgetl1' in Hcc. rewrite Hgetl2' in Hcc.
          destruct Hcc as [Hbeq [Hteq Hcc]]. subst.
          
          edestruct (Forall2_nthN _ _ _ _ _ Hallvs1 Hnth) as [v1' [Hnth' Hv1]].
          edestruct (Forall2_nthN
                       (fun l1 l2 => cc_approx_val k j IIG IG b (Res (l1, H1)) (Res (l2, H2))) vs1)
            as [l3' [Hnth'' Hval']]; eauto.
          (* eapply Hcc. unfold cost1. simpl. simpl in Hcost. omega. *)
          
          edestruct (Forall2_nthN (fun v1 v2 : value => (v1, H2) ≈_( b2, id) (v2, H2'))) as [v2' [Hnth2' Hv2]].
          eapply Forall2_symm_strong; [| eassumption ]. intros. now symmetry; eauto. eassumption.
          
          edestruct Hpre with (c1 := c1 - cost1) as [v2 [c2 [m2 [b' [Hstep [Hinj' [HS Hres]]]]]]];
            [ | | | | | | | eassumption | | ].  
          - intros j'.
            
            edestruct (Hall' (S j'))  as [l2'' [Hgetl2'' Hcc'']]; eauto. repeat subst_exp. 

            simpl in Hcc''. rewrite Hgetl1' in Hcc''. rewrite Hgetl2' in Hcc''.
            destruct Hcc'' as [_ [_ Hcc'']].
            
            edestruct (Forall2_nthN
                         (fun l1 l2 => cc_approx_val k j' IIG IG b (Res (l1, H1)) (Res (l2, H2))) vs1)
              as [v2'' [Hnth2 Hv2']]; eauto.

            eapply cc_approx_val_monotonic.
            rewrite <- cc_approx_val_eq. 
            repeat subst_exp. eassumption. omega.
            
          - eapply heap_env_equiv_set.
            eapply heap_env_equiv_antimon. eassumption.
            normalize_occurs_free... symmetry. eassumption.
          - eapply injective_subdomain_antimon. eassumption.

            rewrite (reach'_idempotent H1' (env_locs rho1' (occurs_free (Eproj x1 t n y1 e1)))).
            eapply reach'_set_monotonic.
            eapply Included_trans.
            eapply env_locs_set_Inlcuded'.
            eapply Union_Included.

            eapply Included_trans; [| eapply Included_post_reach' ].
            normalize_occurs_free. rewrite env_locs_Union, post_Union. eapply Included_Union_preserv_l.
            rewrite env_locs_Singleton; eauto. simpl. erewrite post_Singleton; eauto.
            simpl. eapply In_Union_list. eapply in_map. eapply nthN_In. eassumption.

            eapply Included_trans; [| eapply reach'_extensive ].
            eapply env_locs_monotonic. normalize_occurs_free...
          - eapply heap_env_equiv_set.
            eapply heap_env_equiv_antimon. eassumption.
            normalize_occurs_free...
            repeat subst_exp. eassumption.
          - eapply injective_subdomain_antimon. eassumption.

            rewrite (reach'_idempotent H2 (env_locs rho2 (occurs_free (Eproj x2 t n y2 e2)))).
            eapply reach'_set_monotonic.
            eapply Included_trans.
            eapply env_locs_set_Inlcuded'.
            eapply Union_Included.
            
            eapply Included_trans; [| eapply Included_post_reach' ].
            normalize_occurs_free. rewrite env_locs_Union, post_Union. eapply Included_Union_preserv_l.
            rewrite env_locs_Singleton; eauto. simpl. erewrite post_Singleton; eauto.
            simpl. eapply In_Union_list. eapply in_map. eapply nthN_In. eassumption.

            eapply Included_trans; [| eapply reach'_extensive ].
            eapply env_locs_monotonic. normalize_occurs_free...
          - eapply Hiinv; try eassumption.
            econstructor; eauto.
            now econstructor; eauto.
            econstructor; eauto.
            now econstructor; eauto.
          - unfold cost1. simpl. omega. 
          - intros i. edestruct (Hstuck (i + cost1)) as [r' [m' Hstep']].
            inv Hstep'.
            * unfold cost1 in Hcost0. omega.
            * simpl in Hbs0. rewrite NPeano.Nat.add_sub in Hbs0.
              repeat subst_exp.
              do 2 eexists. eassumption.
          - repeat eexists; eauto. eapply Eval_proj_per_cc with (c := c2 + cost2); try eassumption.
            unfold cost2. simpl; omega. simpl. rewrite NPeano.Nat.add_sub.
            eassumption.
            replace c1 with (c1 - cost1 + cost1) by (unfold cost1; simpl in *; omega).
            eapply Hinv; try eassumption.
            replace cost1 with (0 + cost_ctx (Eproj_c x1 t n y1 Hole_c)) by (unfold cost1; simpl; omega).
            econstructor; eauto. now econstructor; eauto.
            replace cost2 with (0 + cost_ctx (Eproj_c x2 t n y2 Hole_c)) by (unfold cost2; simpl; omega).
            econstructor; eauto. now econstructor; eauto.
            rewrite cc_approx_val_eq in *.
            eapply cc_approx_val_monotonic. eassumption.
            unfold cost1, cost2. simpl. simpl in Hcost. omega. }
    Qed.
    
    (** Case compatibility *)
    Lemma cc_approx_exp_case_nil_compat (k j : nat) (H1 H2 : heap block) (rho1 rho2 : env) (x1 x2 : var) :
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Ecase x1 []) (Ecase x2 []) ->
      (Ecase x1 [], rho1, H1) ⪯ ^ (k ; j ; IIL1; IIG ; IL1 ; IG) (Ecase x2 [], rho2, H2).
    Proof.
      intros Hbase b1 b2 H1' H2' rho1' rho2' v1 c1 m1 Heq1 Hinj1 Heq2 Hinj2 HII Hleq1 Hstep1 Hns. inv Hstep1.
      (* Timeout! *)
      - { simpl in Hcost. exists OOT, c1. eexists. eexists id. repeat split. 
          - econstructor. simpl; omega. reflexivity. 
          - now firstorder.
          - eapply Hbase; eassumption.
          - now rewrite cc_approx_val_eq. }
      (* Termination *) 
      - { simpl in Htag. discriminate. }
    Qed.
    
    Lemma cc_approx_exp_case_compat (k j : nat) (b : Inj) (H1 H2 : heap block) (rho1 rho2 : env) (x1 x2 : var) (t : cTag)
          (e1 e2 : exp) (Pats1 Pats2 : list (cTag * exp)) :
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Ecase x1 ((t, e1) :: Pats1)) (Ecase x2 ((t, e2) :: Pats2)) -> 
      IInvCaseHdCompat IIL1 IIL2 H1 H2 rho1 rho2 x1 x2 t Pats1 Pats2 e1 e2 ->
      InvCaseHdCompat IL1 IL2 H1 H2 rho1 rho2 x1 x2 t Pats1 Pats2 e1 e2 ->
      IInvCaseTlCompat IIL1 H1 H2 rho1 rho2 x1 x2 t Pats1 Pats2 e1 e2 -> 
      InvCaseTlCompat IL1 H1 H2 rho1 rho2 x1 x2 t Pats1 Pats2 e1 e2  ->
      
      cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2 x1 x2 ->

      (e1, rho1, H1) ⪯ ^ (k - cost (Ecase x1 Pats1); j; IIL2 ; IIG ; IL2 ; IG) (e2, rho2, H2) ->

      (Ecase x1 Pats1, rho1, H1) ⪯ ^ (k ; j ; IIL1 ; IIG ; IL1 ; IG) (Ecase x2 Pats2, rho2, H2) ->

      (Ecase x1 ((t, e1) :: Pats1), rho1, H1) ⪯ ^ (k ; j ; IIL1 ; IIG ; IL1 ; IG) (Ecase x2 ((t, e2) :: Pats2), rho2, H2).
    Proof with now eauto with Ensembles_DB.
      intros Hbase Hiinvh Hinvh Hiinvt Hinvt Hvar Hexp_hd Hexp_tl b1 b2 H1' H2' rho1' rho2'
             v1 c1 m1 Heq1 Hinj1 Heq2 Hinj2 HII Hleq1 Hstep1 Hstuck1.
      inv Hstep1.
      (* Timeout! *)
      - { simpl in Hcost. exists OOT, c1. eexists. exists id. repeat split. 
          - econstructor. simpl; omega. reflexivity. 
          - eapply injective_subdomain_Empty_set.
          - eapply Hbase; eassumption.
          - now rewrite cc_approx_val_eq. }
      - { pose (cost1 := cost (Ecase x1 ((t, e1) :: Pats1))).
          pose (cost2 := cost (Ecase x2 ((t, e2) :: Pats2))).
          eapply (cc_approx_var_env_heap_env_equiv
                    _ _
                    (occurs_free (Ecase x1 ((t, e1) :: Pats1)))
                    (occurs_free (Ecase x2 ((t, e2) :: Pats2)))) in Hvar;
          [| eassumption | eassumption | eassumption | eassumption
           | normalize_occurs_free; now eauto with Ensembles_DB | normalize_occurs_free; now eauto with Ensembles_DB ].
          edestruct Hvar as [l' [Hgety' Hcc]]; eauto.
          destruct l' as [l' |l' f ]; [| contradiction ].
          simpl in Hcc. rewrite Hgetl in Hcc. 
          destruct (get l' H2') as [[ t' vs' | | ] |] eqn:Hgetl';
            (try destruct Hcc as [Hbeq Hcc]); try contradiction.
          destruct Hcc as [Heq Hall']; subst. simpl in Hall', Hcost.
          simpl in Htag. destruct (M.elt_eq t t') eqn:Heq; subst.
          - inv Htag.
            edestruct Hexp_hd with (c1 := c1 - cost1) as [v2 [c2 [m2 [b' [Hstep [Hinj' [HS Hres]]]]]]].
            + eapply heap_env_equiv_antimon. now eapply Heq1.
              normalize_occurs_free...
            + eapply injective_subdomain_antimon. eassumption.
              eapply reach'_set_monotonic. eapply env_locs_monotonic.
              normalize_occurs_free...
            + eapply heap_env_equiv_antimon. now eapply Heq2.
              normalize_occurs_free...
            + eapply injective_subdomain_antimon. eassumption.
              eapply reach'_set_monotonic. eapply env_locs_monotonic.
              normalize_occurs_free...
            + eapply Hiinvh; try eassumption.
            + unfold cost1. simpl; omega.
            + eassumption.
            + intros i. edestruct (Hstuck1 (i + cost1)) as [r' [m'' Hstep']].
              inv Hstep'.
              * exists OOT. eexists. econstructor; eauto. unfold cost1 in Hcost0.
               omega. 
              * repeat subst_exp.
                simpl in Htag; rewrite Heq in Htag; inv Htag.
                simpl in Hbs0. rewrite NPeano.Nat.add_sub in Hbs0.
                do 2 eexists. eassumption.
            + repeat eexists; eauto. 
              * eapply Eval_case_per_cc with (c := c2 + cost2)
                ; [ | | | | rewrite NPeano.Nat.add_sub ]; try eassumption.
                unfold cost2. omega. now simpl; rewrite Heq. 
              * replace c1 with (c1 - cost1 + cost1) by (unfold cost1; simpl in *; omega).
                eapply Hinvh; try eassumption.
              * rewrite cc_approx_val_eq. eapply cc_approx_val_monotonic.
                rewrite <- cc_approx_val_eq. eassumption. unfold cost1. simpl. omega.
          - edestruct Hexp_tl as [v2 [c2 [m2 [b' [Hstep2 [Hinj' [HS Hpre2]]]]]]];
               [ | | | | | | now econstructor; eauto | | ].
            + eapply heap_env_equiv_antimon; [ eassumption |].
              normalize_occurs_free...
            + eapply injective_subdomain_antimon. eassumption.
              eapply reach'_set_monotonic. eapply env_locs_monotonic.
              normalize_occurs_free...
            + eapply heap_env_equiv_antimon; [ eassumption |].
              normalize_occurs_free...
            + eapply injective_subdomain_antimon. eassumption.
              eapply reach'_set_monotonic. eapply env_locs_monotonic.
              normalize_occurs_free...
            + eapply Hiinvt; try eassumption. 
            + simpl in Hcost. omega.
            + intros i. edestruct (Hstuck1 i) as [r' [m'' Hstep']].
              inv Hstep'.
              * exists OOT. eexists. econstructor; eauto.
              * repeat subst_exp.
                simpl in Htag0; rewrite Heq in Htag0. repeat subst_exp.
                simpl in Hbs0.
                do 2 eexists. eapply Eval_case_gc.
                simpl in Hcost0. simpl. omega.
                eassumption. eassumption. eassumption. eassumption.
            + inv Hstep2.
              * (* Timeout! *)
                { simpl in Hcost0. exists OOT, c2. eexists. exists b'. repeat split.
                  - econstructor. simpl. eassumption. reflexivity. 
                  - eassumption.
                  - eapply Hinvt; try eassumption.
                    
                  - eassumption. }
              * (* termination *)
                { repeat eexists; eauto.
                  - eapply Eval_case_per_cc with (c := c2); eauto.
                    simpl. repeat subst_exp.
                    rewrite Heq. eassumption.
                } }
    Qed.
    
    (** Halt compatibility *)
    Lemma cc_approx_exp_halt_compat (k j : nat) (H1 H2 : heap block) (rho1 rho2 : env) (b : Inj) (x1 x2 : var) :
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Ehalt x1) (Ehalt x2) ->
      
      cc_approx_var_env k j IIG IG b H1 rho1 H2 rho2 x1 x2 ->
      injective_subdomain (reach_n H1 j (env_locs rho1 [set x1])) b ->
      
      (Ehalt x1, rho1, H1) ⪯ ^ (k ; j ; IIL1 ; IIG ; IL1; IG) (Ehalt x2, rho2, H2).
    Proof.
      intros Hbase Hvar Hinj b1 b2 H1' H2' rho1' rho2' v1 c1 m1 Heq1 Hinj1
             Heq2 Hinj2 Hleq1 HII Hstep1 Hstuck1.
      assert (Hvar' := Hvar).
      inv Hstep1.
      - (* Timeout! *)
        { simpl in Hcost. exists OOT, c1. eexists. exists id. repeat split. 
          - econstructor; eauto.
          - eapply injective_subdomain_Empty_set.
          - rewrite <- plus_n_O. eapply Hbase; try eassumption.
          - now rewrite cc_approx_val_eq. }
      - pose (cost1 := cost (Ehalt x1)).
        pose (cost2 := cost (Ehalt x2)).
        eapply (cc_approx_var_env_heap_env_equiv
                  _ _
                  (occurs_free (Ehalt x1))
                  (occurs_free (Ehalt x2))) in Hvar;
          [| eassumption | eassumption | eassumption | eassumption | now constructor | now constructor ]. 
        edestruct Hvar as [l' [Hgety' Hcc]]; eauto.
        eexists. exists c1. eexists. exists (b2 ∘ b ∘ b1). repeat eexists.
        * eapply Eval_halt_per_cc. simpl. simpl in Hcost. omega. eassumption.
          reflexivity.
        * eapply injective_subdomain_compose. simpl.
          
          eapply injective_subdomain_antimon. eassumption.
          eapply Included_trans. eapply reach_n_in_reach.
          eapply reach'_set_monotonic. eapply get_In_env_locs; try eassumption.
          now eauto.
          
          destruct Heq1 as [Hl1 Hr1]. simpl in Hl1.
          edestruct Hr1 as [l1 [Hget1 Hres]]; eauto. 
          simpl. rewrite res_equiv_image_reach_n; try eassumption.
          rewrite image_id.
          
          eapply injective_subdomain_compose.
          
         
          eapply injective_subdomain_antimon. eassumption.
          rewrite env_locs_Singleton; eauto. reflexivity.

          eapply cc_approx_var_env_image_reach_n_eq in Hvar'; eauto.

          rewrite env_locs_Singleton in Hvar'; eauto.
          
          rewrite Hvar'.
          eapply injective_subdomain_antimon. eassumption.
          simpl. rewrite occurs_free_Ehalt. eapply reach_n_in_reach.
        * eapply Hbase; try eassumption.
        * rewrite cc_approx_val_eq in *.
          eapply cc_approx_val_monotonic. eassumption.
          omega.
    Qed.
   
    (** Abstraction compatibility *)
    Lemma cc_approx_exp_fun_compat (k j : nat) rho1 rho2 H1 H2 B1 e1 B2 e2 :
      InvCtxCompat IL1 IL2 H1 H2 rho1 rho2 (Efun1_c B1 Hole_c) (Efun1_c B2 Hole_c) e1 e2 ->
      IInvCtxCompat IIL1 IIL2 H1 H2 rho1 rho2 (Efun1_c B1 Hole_c) (Efun1_c B2 Hole_c) e1 e2 ->
      InvCostBase IL1 IIL1 H1 H2 rho1 rho2 (Efun B1 e1) (Efun B2 e2) ->

      well_formed (reach' H1 (env_locs rho1 (occurs_free (Efun B1 e1)))) H1 ->
      (env_locs rho1 (occurs_free (Efun B1 e1))) \subset dom H1 ->
      
      (forall H1 H1' H1'' rho1' rho_clo env_loc,

         restrict_env (fundefs_fv B1) rho1 = rho_clo ->
         alloc (Env rho_clo) H1 = (env_loc, H1') ->

         def_closures B1 B1 rho1 H1' env_loc = (H1'', rho1') ->

         (e1, rho1', H1'') ⪯ ^ (k - cost (Efun B1 e1) ; j ; IIL2 ; IIG ; IL2 ; IG)
         (e2, def_funs B2 B2 rho2, H2)) ->
      
      (Efun B1 e1, rho1, H1) ⪯ ^ (k ; j ; IIL1 ; IIG ; IL1 ; IG) (Efun B2 e2, rho2, H2).
    Proof with now eauto with Ensembles_DB.
      intros Hinv Hiinv Hbase Hwf Hdom Hpre b1 b2 H1' H2' rho1' rho2' v1 c1
             m1 Heq1 Hinj1 Heq2 Hinj2 HII Hleq1 Hstep1 Hstuck1.
      inv Hstep1.
      (* Timeout! *)
      - { simpl in Hcost. exists OOT, c1.
          - eexists. eexists id. repeat split. econstructor. simpl.
            omega. reflexivity.
            now eapply injective_subdomain_Empty_set.
            eapply Hbase; eassumption.
            now rewrite cc_approx_val_eq. }
      (* Termination *)
      - { destruct (alloc (Env (restrict_env (fundefs_fv B1) rho1)) H1)
            as [env_loc3 H3] eqn:Ha3.
          destruct (def_closures B1 B1 rho1 H3 env_loc3)
            as [H3' rho3] eqn:Hdef3.
          assert (Hlocs : env_locs rho1' (occurs_free (Efun B1 e1)) \subset dom H1').
          { eapply env_locs_in_dom. eassumption. eassumption. }
          assert (Hwf1' : well_formed (reach' H3 (env_locs rho1 (occurs_free (Efun B1 e1)))) H3).
          { eapply well_formed_alloc; try eassumption.
            rewrite <- well_formed_reach_alloc_same; eassumption.
            eapply Included_trans. apply restrict_env_env_locs.
            eapply restrict_env_correct.
            replace (FromList (set_util.PS.elements (fundefs_fv B1)))
            with (set_util.FromSet (fundefs_fv B1)) by reflexivity.
            rewrite <- fundefs_fv_correct. reflexivity.
            eapply Included_trans; try eassumption.
            normalize_occurs_free. eapply env_locs_monotonic... }
          assert (Hwf2' : well_formed (reach' H' (env_locs rho1' (occurs_free (Efun B1 e1)))) H').
          { eapply well_formed_respects_heap_env_equiv. eassumption.
            eapply heap_env_equiv_weaking_cor with (H1 := H1) (H2 := H1'); try eassumption.
            eapply well_formed_respects_heap_env_equiv; try now apply Hwf.
            eassumption.
            eapply HL.alloc_subheap. eassumption.
            eapply HL.alloc_subheap. eassumption. }
          edestruct heap_env_equiv_def_funs_strong
          with (H1 := H3)
                 (H2 := H')
                 (S := occurs_free (Efun B1 e1) :|: name_in_fundefs B1) (B := B1) 
                 (B0 := B1)
                 (β1 := b1 {env_loc ~> env_loc3})
            as [b1' Hs]; try eassumption.
          - eapply well_formed_antimon; try eassumption.
            eapply reach'_set_monotonic. eapply env_locs_monotonic...
          - eapply well_formed_antimon; try eassumption.
            eapply reach'_set_monotonic. eapply env_locs_monotonic...
          - eapply Included_trans; [| now eapply HL.dom_subheap; eapply HL.alloc_subheap; eauto].
            eapply Included_trans; try eassumption. 
            eapply env_locs_monotonic...
          - eapply Included_trans; [| now eapply HL.dom_subheap; eapply HL.alloc_subheap; eauto].
            rewrite Setminus_Union_distr, Setminus_Same_set_Empty_set, Union_Empty_set_neut_r.
            eapply Included_trans; try eassumption.
            eapply env_locs_monotonic... 
          - erewrite gas; eauto.
          - erewrite gas; eauto.
          - eapply Included_trans.
            eapply restrict_env_env_locs. eapply restrict_env_correct.
            now eapply fundefs_fv_correct.
            eapply Included_trans; [| now apply reach'_extensive ].
            normalize_occurs_free. eapply env_locs_monotonic.
            rewrite !Setminus_Union_distr. eapply Included_Union_preserv_l.
            eapply Included_Union_preserv_l. rewrite Setminus_Disjoint. reflexivity.
            eapply Disjoint_sym.
            eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
          - eapply Included_trans.
            eapply restrict_env_env_locs. eapply restrict_env_correct.
            now eapply fundefs_fv_correct.
            eapply Included_trans; [| now apply reach'_extensive ].
            normalize_occurs_free. eapply env_locs_monotonic.
            rewrite !Setminus_Union_distr. eapply Included_Union_preserv_l.
            eapply Included_Union_preserv_l. rewrite Setminus_Disjoint. reflexivity.
            eapply Disjoint_sym.
            eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
          - normalize_occurs_free. rewrite !Setminus_Union_distr.
            do 2 eapply Included_Union_preserv_l.
            rewrite Setminus_Disjoint. reflexivity. eapply Disjoint_sym.
            eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
          - rewrite res_equiv_eq. simpl. split.
            rewrite extend_gss. reflexivity.
            erewrite !gas; try eassumption. simpl. 
            eapply heap_env_equiv_restrict_env with (S' := occurs_free_fundefs B1).
            * eapply heap_env_equiv_weaking_cor with (H1 := H1) (H2 := H1'); try eassumption.
              eapply well_formed_respects_heap_env_equiv; try now apply Hwf.
              eassumption.
              eapply heap_env_equiv_rename_ext. eassumption.
              reflexivity. eapply f_eq_subdomain_extend_not_In_S_r.
              intros Hc. eapply reachable_in_dom in Hc.  destruct Hc as [x1 Hx1].
              erewrite alloc_fresh in Hx1; try eassumption. congruence.
              eapply well_formed_respects_heap_env_equiv. now apply Hwf.
              eassumption. eassumption. reflexivity.
              eapply HL.alloc_subheap. eassumption.
              eapply HL.alloc_subheap. eassumption.
            * normalize_occurs_free...
            * eapply restrict_env_correct. now eapply fundefs_fv_correct.
            * eapply restrict_env_correct. now eapply fundefs_fv_correct.
          - eapply injective_subdomain_Union.
            + eapply injective_subdomain_Singleton.
            + eapply injective_subdomain_extend'.
              * rewrite reach'_alloc; try eassumption. eapply injective_subdomain_antimon.
                eassumption.
                eapply Setminus_Included_preserv. eapply reach'_set_monotonic.
                eapply env_locs_monotonic...
                simpl. 
                eapply Included_trans. apply restrict_env_env_locs.
                eapply restrict_env_correct.
                replace (FromList (set_util.PS.elements (fundefs_fv B1)))
                with (set_util.FromSet (fundefs_fv B1)) by reflexivity.
                rewrite <- fundefs_fv_correct. reflexivity.
                eapply Included_trans; [| now eapply reach'_extensive ].
                eapply env_locs_monotonic. normalize_occurs_free.
                rewrite !Setminus_Union_distr. eapply Included_Union_preserv_l.
                eapply Included_Union_preserv_l. rewrite Setminus_Disjoint. reflexivity.
                eapply Disjoint_sym.
                eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
              * intros Hc.
                eapply image_monotonic with
                (S' := (reach' H1' (env_locs rho1' (occurs_free (Efun B1 e1))))) in Hc.
                eapply heap_env_equiv_image_reach in Hc; try (symmetry; eassumption).
                rewrite image_id in Hc. eapply reachable_in_dom in Hc.  destruct Hc as [x1 Hx1].
                erewrite alloc_fresh in Hx1; try eassumption. congruence.
                eassumption. eassumption. eassumption.
                rewrite reach'_alloc; try eassumption.
                eapply Included_trans. eapply Setminus_Included.
                eapply reach'_set_monotonic. eapply env_locs_monotonic...
                eapply Included_trans.
                eapply restrict_env_env_locs. eapply restrict_env_correct.
                now eapply fundefs_fv_correct.
                eapply Included_trans; [| now apply reach'_extensive ].
                normalize_occurs_free.
                eapply env_locs_monotonic.
                rewrite !Setminus_Union_distr. eapply Included_Union_preserv_l.
                rewrite Setminus_Disjoint. now eauto with Ensembles_DB.
                eapply Disjoint_sym.
                eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
            + rewrite image_Singleton, Setminus_Union_distr, Setminus_Same_set_Empty_set,
                      Union_Empty_set_neut_r.
              rewrite extend_gss.
              rewrite reach'_alloc; try eassumption.
              eapply Disjoint_Singleton_l. intros Hc.
              rewrite image_extend_not_In_S in Hc.
              rewrite Setminus_Disjoint in Hc.
              rewrite heap_env_equiv_image_reach in Hc; try (symmetry; eassumption).
              rewrite image_id in Hc. eapply reachable_in_dom in Hc.  destruct Hc as [x1 Hx1].
              erewrite alloc_fresh in Hx1; try eassumption. congruence.
              eassumption. eassumption. constructor. intros x [y Hc2]. inv Hc2. now eauto.
              eapply occurs_free_fundefs_name_in_fundefs_Disjoint. now split; eauto.
              intros Hc'. (*  *)
              eapply reachable_in_dom in Hc'.  destruct Hc' as [x1 Hx1].
              erewrite alloc_fresh in Hx1; try eassumption. congruence.
              eapply well_formed_respects_heap_env_equiv.
              eapply well_formed_antimon; [| now apply Hwf ].
              eapply reach'_set_monotonic. eapply env_locs_monotonic...
              eapply heap_env_equiv_antimon; [ eassumption |]...
              eapply Included_trans; [| eassumption ]. eapply env_locs_monotonic...
              eapply Included_trans.
              eapply restrict_env_env_locs. eapply restrict_env_correct.
              now eapply fundefs_fv_correct.
              eapply Included_trans; [| now apply reach'_extensive ].
              normalize_occurs_free.
              eapply env_locs_monotonic.
              rewrite !Setminus_Union_distr. eapply Included_Union_preserv_l.
              rewrite Setminus_Disjoint. reflexivity.
              eapply Disjoint_sym.
              eapply occurs_free_fundefs_name_in_fundefs_Disjoint.
          - rewrite Setminus_Union_distr, Setminus_Same_set_Empty_set, Union_Empty_set_neut_r.
            eapply heap_env_equiv_antimon.
            eapply heap_env_equiv_weaking_cor with (H1 := H1) (H2 := H1'); try eassumption.
            eapply well_formed_respects_heap_env_equiv; try now apply Hwf.
            eassumption.
            eapply heap_env_equiv_rename_ext. eassumption.
            reflexivity. eapply f_eq_subdomain_extend_not_In_S_r.
            intros Hc. eapply reachable_in_dom in Hc.
            destruct Hc as [v1' Hgetv1']. erewrite alloc_fresh in Hgetv1'; eauto. inv Hgetv1'.
            eapply well_formed_respects_heap_env_equiv; try now apply Hwf.
            eassumption. eassumption.
            reflexivity.
            eapply HL.alloc_subheap. eassumption.
            eapply HL.alloc_subheap. eassumption. now eauto with Ensembles_DB.
          - rewrite Hfuns, Hdef3 in Hs.
            destruct Hs as [Heq [Hfeq' [Hwf1 [Hwf2 [Henv1 [Henv2 Hinj]]]]]].
            edestruct Hpre as [v2 [c2 [m2 [b2' [Hstep [HS [Hinj' Hval]]]]]]]
            ; [ | now apply Ha3 | now apply Hdef3 | |
                | | | | | eassumption | | ].
            + reflexivity.
            + eapply heap_env_equiv_antimon. eassumption.
              normalize_occurs_free. rewrite <- Union_assoc. eapply Included_Union_preserv_r.
              eapply Included_Union_Setminus.
              now eauto with typeclass_instances.
            + eapply injective_subdomain_antimon. eassumption.
              eapply Included_Union_preserv_r. eapply reach'_set_monotonic.
              normalize_occurs_free. eapply env_locs_monotonic. rewrite <- Union_assoc.
              eapply Included_Union_preserv_r.
              eapply Included_Union_Setminus. now eauto with typeclass_instances.
            + eapply heap_env_equiv_def_funs'.
              eapply heap_env_equiv_antimon. eassumption. normalize_occurs_free...
            + eapply injective_subdomain_antimon. eassumption.
              eapply reach'_set_monotonic.
              eapply Included_trans. eapply def_funs_env_loc.
              eapply env_locs_monotonic. normalize_occurs_free...
            + eapply Hiinv; try eassumption.
              econstructor; eauto. now econstructor. 
              econstructor; eauto. now econstructor.
           + simpl; omega.
           + intros i. edestruct (Hstuck1 (i + cost (Efun B1 e1))) as [r' [m' Hstep']].
             inv Hstep'.
             * omega.
             * rewrite NPeano.Nat.add_sub in Hbs0. repeat subst_exp.
               repeat eexists. eassumption.
           + repeat eexists; eauto.
             * eapply Eval_fun_per_cc with (c := c2 + cost (Efun B2 e2)); try eassumption.
               simpl. omega. reflexivity. simpl.
               rewrite NPeano.Nat.add_sub. eassumption.
             * simpl.
               replace c1 with (c1 - 1 + 1) by (simpl in *; omega).
               eapply Hinv; try eassumption.
               replace 1 with (0 + cost_ctx (Efun1_c B1 Hole_c)) by (simpl; omega).
               econstructor; eauto. now econstructor.
               replace 1 with (0 + cost_ctx (Efun1_c B2 Hole_c)) by (simpl; omega).
               econstructor; eauto. now econstructor.
             * rewrite cc_approx_val_eq in *. 
               eapply cc_approx_val_monotonic. eassumption.
               simpl. simpl in Hcost. omega. }
    Qed.    

   
    (* TODO move *)
    Lemma comp_ctx_f_Hole_c C :
      comp_ctx_f C Hole_c = C
    with comp_f_ctx_f_Hole_c f : 
     comp_f_ctx_f f Hole_c = f.
    Proof.
      - destruct C; simpl; eauto;
        try (rewrite comp_ctx_f_Hole_c; reflexivity). 
        rewrite comp_f_ctx_f_Hole_c. reflexivity.
      - destruct f; simpl; eauto.
        rewrite comp_ctx_f_Hole_c; reflexivity.
        rewrite comp_f_ctx_f_Hole_c. reflexivity.
    Qed.
    

    Context (ILC : exp_ctx -> Inv).
    Context (IILC : exp_ctx -> IInv).

          
    Lemma cc_approx_exp_right_ctx_compat 
          (k j : nat) rho1 rho2 rho2' H1 H2 H2' e1 C e2 c' :
      InvCtxCompat_r IL1 IL2 H1 H2 rho1 rho2 C e1 e2 ->
      IInvCtxCompat_r IIL1 IIL2 H1 H2 rho1 rho2 C e1 e2 ->
      
      well_formed (reach' H1 (env_locs rho1 (occurs_free e1))) H1 ->
      well_formed (reach' H2 (env_locs rho2 (occurs_free (C |[ e2 ]|)))) H2 ->
      (env_locs rho1 (occurs_free e1)) \subset dom H1 ->
      (env_locs rho2 (occurs_free (C |[ e2 ]|))) \subset dom H2 ->

      ctx_to_heap_env_CC C H2 rho2 H2' rho2' c' ->
      (e1, rho1, H1) ⪯ ^ (k; j; IIL2 ; IIG ; IL2 ; IG) (e2, rho2', H2') ->
      (e1, rho1, H1) ⪯ ^ (k; j; IIL1 ; IIG ; IL1 ; IG) (C |[ e2 ]|, rho2, H2).
    Proof with now eauto with Ensembles_DB.
      intros Hinv Hiinv Hwf1 Hwf2 Henv1 Henv2 Hctx Hpre.
      intros b1 b2 H1' H3 rho1' rho3 v1 k1 m1 Heq1 Hinj1 Heq2 Hinj2 HII Hleq1 Hstep1 Hstuck1.
      edestruct ctx_to_heap_env_determistic as [H3' [rho3' [b' [Heq' [Hinj Heval]]]]]; try eassumption.
      edestruct Hpre as [r1 [c3 [m2 [b'' [Hstep2 [Hinj' [Hinv' Hccr]]]]]]]; try eassumption. 
      + eapply Hiinv; try eassumption.
      + eexists r1, (c3 + c'), m2, b''. split; [| split; [| split ]]; try eassumption.
        * eapply ctx_to_heap_env_big_step_compose; try eassumption.
        * eapply Hinv with (c' := c'); eauto.
    Qed.
    
   
  End CompatLemmas.

  
End Compat.