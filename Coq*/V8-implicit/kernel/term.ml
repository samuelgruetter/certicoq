(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id: term.ml 9851 2007-05-23 10:29:01Z herbelin $ *)

(* This module instantiates the structure of generic deBruijn terms to Coq *)

open Util
open Pp
open Names
open Univ
open Esubst

(* Coq abstract syntax with deBruijn variables; 'a is the type of sorts *)

type existential_key = int
type metavariable = int

(* This defines the strategy to use for verifiying a Cast *)

(* This defines Cases annotations *)
type case_style = LetStyle | IfStyle | MatchStyle | RegularStyle
type case_printing =
  { ind_nargs : int; (* number of real args of the inductive type *)
    style     : case_style }
type case_info =
  { ci_ind        : inductive;
    ci_npar       : int;
    ci_cstr_nargs : int array; (* number of real args of each constructor *)
    ci_pp_info    : case_printing (* not interpreted by the kernel *)
  }

(* Implicit *)

type impl = Impl | Expl
type impargs = impl list
let impl_merge l =
  if List.for_all (fun i -> i=Expl) l then Expl else Impl
let impl_mergev l =
  if array_for_all (fun i -> i=Expl) l then Expl else Impl


(* Sorts. *)

type contents = Pos | Null

type sorts =
  | Prop of contents                      (* proposition types *)
  | Type of universe
      
let mk_Set  = Prop Pos
let mk_Prop = Prop Null

type sorts_family = InProp | InSet | InType

let family_of_sort = function
  | Prop Null -> InProp
  | Prop Pos -> InSet
  | Type _ -> InType

(********************************************************************)
(*       Constructions as implemented                               *)
(********************************************************************)

type cast_kind = VMcast | DEFAULTcast 

(* [constr array] is an instance matching definitional [named_context] in
   the same order (i.e. last argument first) *)
type 'constr pexistential = existential_key * 'constr array
type ('constr, 'types) prec_declaration =
    name array * 'types array * 'constr array
type ('constr, 'types) pfixpoint =
    (int array * int) * ('constr, 'types) prec_declaration
type ('constr, 'types) pcofixpoint =
    int * ('constr, 'types) prec_declaration

(* [Var] is used for named variables and [Rel] for variables as
   de Bruijn indices. *)
type ('constr, 'types) kind_of_term =
  | Rel       of int
  | Var       of identifier
  | Meta      of metavariable
  | Evar      of 'constr pexistential
  | Sort      of sorts
  | Cast      of 'constr * cast_kind * 'types
  | Prod      of name * impl * 'types * 'types
  | Lambda    of name * impl * 'types * 'constr
  | LetIn     of name * impl * 'constr * 'types * 'constr
  | App       of 'constr * 'constr array * impl array
  | Const     of constant
  | Ind       of inductive
  | Construct of constructor
  | Case      of case_info * 'constr * 'constr * 'constr array
  | Fix       of ('constr, 'types) pfixpoint
  | CoFix     of ('constr, 'types) pcofixpoint

(* Experimental *)
type ('constr, 'types) kind_of_type =
  | SortType   of sorts
  | CastType   of 'types * 'types 
  | ProdType   of name * impl * 'types * 'types
  | LetInType  of name * impl * 'constr * 'types * 'types
  | AtomicType of 'constr * 'constr array * impl array

let kind_of_type = function
  | Sort s -> SortType s
  | Cast (c,_,t) -> CastType (c, t)
  | Prod (na,imp,t,c) -> ProdType (na, imp, t, c)
  | LetIn (na,imp,b,t,c) -> LetInType (na, imp, b, t, c)
  | App (c,l,imps) -> AtomicType (c, l, imps)
  | (Rel _ | Meta _ | Var _ | Evar _ | Const _ | Case _ | Fix _ | CoFix _ | Ind _ as c)
    -> AtomicType (c,[||],[||])
  | (Lambda _ | Construct _) -> failwith "Not a type"

(* constr is the fixpoint of the previous type. Requires option
   -rectypes of the Caml compiler to be set *)
type constr = (constr,constr) kind_of_term

type existential = existential_key * constr array
type rec_declaration = name array * constr array * constr array
type fixpoint = (int array * int) * rec_declaration
type cofixpoint = int * rec_declaration

(***************************)
(* hash-consing functions  *)                         
(***************************)

let comp_term t1 t2 =
  match t1, t2 with
  | Rel n1, Rel n2 -> n1 = n2
  | Meta m1, Meta m2 -> m1 == m2
  | Var id1, Var id2 -> id1 == id2
  | Sort s1, Sort s2 -> s1 == s2
  | Cast (c1,_,t1), Cast (c2,_,t2) -> c1 == c2 & t1 == t2
  | Prod (n1,imp1,t1,c1), Prod (n2,imp2,t2,c2) ->
      imp1 == imp2 &  n1 == n2 & t1 == t2 & c1 == c2
  | Lambda (n1,imp1,t1,c1), Lambda (n2,imp2,t2,c2) ->
      imp1 == imp2 &  n1 == n2 & t1 == t2 & c1 == c2
  | LetIn (n1,imp1,b1,t1,c1), LetIn (n2,imp2,b2,t2,c2) ->
      imp1 == imp2 & n1 == n2 & b1 == b2 & t1 == t2 & c1 == c2
  | App (c1,l1,imps1), App (c2,l2,imps2) ->
      array_for_all2 (==) imps1 imps2 & c1 == c2 & array_for_all2 (==) l1 l2
  | Evar (e1,l1), Evar (e2,l2) -> e1 = e2 & array_for_all2 (==) l1 l2
  | Const c1, Const c2 -> c1 == c2
  | Ind (sp1,i1), Ind (sp2,i2) -> sp1 == sp2 & i1 = i2
  | Construct ((sp1,i1),j1), Construct ((sp2,i2),j2) ->
      sp1 == sp2 & i1 = i2 & j1 = j2
  | Case (ci1,p1,c1,bl1), Case (ci2,p2,c2,bl2) ->
      ci1 == ci2 & p1 == p2 & c1 == c2 & array_for_all2 (==) bl1 bl2
  | Fix (ln1,(lna1,tl1,bl1)), Fix (ln2,(lna2,tl2,bl2)) ->
      ln1 = ln2
      & array_for_all2 (==) lna1 lna2
      & array_for_all2 (==) tl1 tl2
      & array_for_all2 (==) bl1 bl2
  | CoFix(ln1,(lna1,tl1,bl1)), CoFix(ln2,(lna2,tl2,bl2)) ->
      ln1 = ln2
      & array_for_all2 (==) lna1 lna2
      & array_for_all2 (==) tl1 tl2
      & array_for_all2 (==) bl1 bl2
  | _ -> false

let hash_term (sh_rec,(sh_sort,sh_con,sh_kn,sh_na,sh_id)) t =
  match t with
  | Rel _ -> t
  | Meta x -> Meta x
  | Var x -> Var (sh_id x)
  | Sort s -> Sort (sh_sort s)
  | Cast (c, k, t) -> Cast (sh_rec c, k, (sh_rec t))
  | Prod (na,imp,t,c) -> Prod (sh_na na, imp, sh_rec t, sh_rec c)
  | Lambda (na,imp,t,c) -> Lambda (sh_na na, imp, sh_rec t, sh_rec c)
  | LetIn (na,imp,b,t,c) -> LetIn (sh_na na, imp, sh_rec b, sh_rec t, sh_rec c)
  | App (c,l,imps) -> App (sh_rec c, Array.map sh_rec l, imps)
  | Evar (e,l) -> Evar (e, Array.map sh_rec l)
  | Const c -> Const (sh_con c)
  | Ind (kn,i) -> Ind (sh_kn kn,i)
  | Construct ((kn,i),j) -> Construct ((sh_kn kn,i),j)
  | Case (ci,p,c,bl) -> (* TO DO: extract ind_kn *)
      Case (ci, sh_rec p, sh_rec c, Array.map sh_rec bl)
  | Fix (ln,(lna,tl,bl)) ->
      Fix (ln,(Array.map sh_na lna,
                 Array.map sh_rec tl,
                 Array.map sh_rec bl))
  | CoFix(ln,(lna,tl,bl)) ->
      CoFix (ln,(Array.map sh_na lna,
                   Array.map sh_rec tl,
                   Array.map sh_rec bl))

module Hconstr =
  Hashcons.Make(
    struct
      type t = constr
      type u = (constr -> constr) *
               ((sorts -> sorts) * (constant -> constant) *
               (kernel_name -> kernel_name) * (name -> name) *
               (identifier -> identifier))
      let hash_sub = hash_term
      let equal = comp_term
      let hash = Hashtbl.hash
    end)

let hcons_term (hsorts,hcon,hkn,hname,hident) =
  Hashcons.recursive_hcons Hconstr.f (hsorts,hcon,hkn,hname,hident)

(************************)
(*  extraction function *)
(************************)

let expl_filter args imps =
  let l = Array.to_list args in
  let i = Array.to_list imps in
  Array.of_list(fst (Util.list_filter2 (fun _ i -> i=Expl) (l, i)))


let extract_closed_gen f g t =
  let rec extr n trm =
    match trm with
  | Rel i ->
      if i <= n then true
      else f (i-n)
  | Var id -> g id
  | Cast(c,_,_) -> extr n c
  | Prod(_,_,a,b) -> extr n a && extr (n+1) b
  | Lambda(_,_,a,b) -> extr (n+1) b
  | LetIn(_,Expl,a,_,b) -> extr n a && extr (n+1) b
  | LetIn(_,Impl,_,_,b) -> extr (n+1) b
  | App((Evar _|Meta _),_,_) -> true
  | App(f,args,imps) ->
      let args = expl_filter args imps in
      extr n f && array_for_all (extr n) args
  | Case(ci,p,c,br) -> extr n c && array_for_all (extr n) br
  | Fix(_,(_,_,bds)) ->
      let nf = Array.length bds in
      array_for_all (extr (n+nf)) bds
  | CoFix(_,(_,_,bds)) ->
      let nf = Array.length bds in
      array_for_all (extr (n+nf)) bds
  | (Construct _|Ind _|Const _|Sort _|Evar _|Meta _) -> true in
  extr 0 t

let extract_closed =
  extract_closed_gen (fun i -> i<>1) (fun _ -> true)

(* Constructs a DeBrujin index with number n *)
let rels =
  [|Rel  1;Rel  2;Rel  3;Rel  4;Rel  5;Rel  6;Rel  7; Rel  8;
    Rel  9;Rel 10;Rel 11;Rel 12;Rel 13;Rel 14;Rel 15; Rel 16|]

let mkRel n = if 0<n & n<=16 then rels.(n-1) else Rel n

(* Constructs an existential variable named "?n" *)
let mkMeta  n =  Meta n

(* Constructs a Variable named id *)
let mkVar id = Var id

(* Construct a type *)
let mkSort s = Sort s

(* Constructs the term t1::t2, i.e. the term t1 casted with the type t2 *)
(* (that means t2 is declared as the type of t1) 
   [s] is the strategy to use when *)
let mkCast (t1,k2,t2) =
  match t1 with
  | Cast (c,k1, _) when k1 = k2 -> Cast (c,k1,t2)
  | _ -> Cast (t1,k2,t2)

(* Constructs the product (x:t1)t2 *)
let mkProd (x,imp,t1,t2) = Prod (x,imp,t1,t2)

(* Constructs the abstraction [x:t1]t2 *)
let mkLambda (x,imp,t1,t2) =
  assert (imp=Expl || extract_closed t2);  
  Lambda (x,imp,t1,t2)

(* Constructs [x=c_1:t]c_2 *)
let mkLetIn (x,imp,c1,t,c2) = LetIn (x,imp,c1,t,c2)

(* If lt = [t1; ...; tn], constructs the application (t1 ... tn) *)
(* We ensure applicative terms have at least one argument and the
   function is not itself an applicative term *)
let mkApp (f, a, imps) = 
  assert (Array.length a = Array.length imps);
  if Array.length a = 0 then f else
    match f with
      | App (g, cl, imps2) ->
          App (g, Array.append cl a,Array.append imps2 imps)
      | _ -> App (f, a, imps)


(* Constructs a constant *) 
(* The array of terms correspond to the variables introduced in the section *)
let mkConst c = Const c

(* Constructs an existential variable *)
let mkEvar e = Evar e

(* Constructs the ith (co)inductive type of the block named kn *)
(* The array of terms correspond to the variables introduced in the section *)
let mkInd m = Ind m

(* Constructs the jth constructor of the ith (co)inductive type of the 
   block named kn. The array of terms correspond to the variables
   introduced in the section *)
let mkConstruct c = Construct c

(* Constructs the term <p>Case c of c1 | c2 .. | cn end *)
let mkCase (ci, p, c, ac) = Case (ci, p, c, ac)

let mkFix fix = Fix fix

let mkCoFix cofix = CoFix cofix

let kind_of_term c = c

(************************************************************************)
(*    kind_of_term = constructions as seen by the user                 *)
(************************************************************************)

(* User view of [constr]. For [App], it is ensured there is at
   least one argument and the function is not itself an applicative
   term *)

let kind_of_term = kind_of_term


(* En vue d'un kind_of_type : constr -> hnftype ??? *)
type hnftype =
  | HnfSort   of sorts
  | HnfProd   of name * impl * constr * constr
  | HnfAtom   of constr
  | HnfInd of inductive * constr array

(**********************************************************************)
(*          Non primitive term destructors                            *)
(**********************************************************************)

(* Destructor operations : partial functions 
   Raise invalid_arg "dest*" if the const has not the expected form *)

(* Destructs a DeBrujin index *)
let destRel c = match kind_of_term c with
  | Rel n -> n
  | _ -> invalid_arg "destRel"

(* Destructs an existential variable *)
let destMeta c = match kind_of_term c with
  | Meta n -> n
  | _ -> invalid_arg "destMeta"

let isMeta c = match kind_of_term c with Meta _ -> true | _ -> false

(* Destructs a variable *)
let destVar c = match kind_of_term c with
  | Var id -> id
  | _ -> invalid_arg "destVar"

(* Destructs a type *)
let isSort c = match kind_of_term c with
  | Sort s -> true
  | _ -> false

let destSort c = match kind_of_term c with
  | Sort s -> s
  | _ -> invalid_arg "destSort"

let rec isprop c = match kind_of_term c with
  | Sort (Prop _) -> true
  | Cast (c,_,_) -> isprop c
  | _ -> false

let rec is_Prop c = match kind_of_term c with
  | Sort (Prop Null) -> true
  | Cast (c,_,_) -> is_Prop c
  | _ -> false

let rec is_Set c = match kind_of_term c with
  | Sort (Prop Pos) -> true
  | Cast (c,_,_) -> is_Set c
  | _ -> false

let rec is_Type c = match kind_of_term c with
  | Sort (Type _) -> true
  | Cast (c,_,_) -> is_Type c
  | _ -> false

let isType = function
  | Type _ -> true
  | _ -> false

let is_small = function
  | Prop _ -> true
  | _ -> false

let iskind c = isprop c or is_Type c

let same_kind c1 c2 = (isprop c1 & isprop c2) or (is_Type c1 & is_Type c2)

(* Tests if an evar *)
let isEvar c = match kind_of_term c with Evar _ -> true | _ -> false

(* Destructs a casted term *)
let destCast c = match kind_of_term c with 
  | Cast (t1,k,t2) -> (t1,k,t2)
  | _ -> invalid_arg "destCast"

let isCast c = match kind_of_term c with Cast _ -> true | _ -> false


(* Tests if a de Bruijn index *)
let isRel c = match kind_of_term c with Rel _ -> true | _ -> false

(* Tests if a variable *)
let isVar c = match kind_of_term c with Var _ -> true | _ -> false

(* Tests if an inductive *)
let isInd c = match kind_of_term c with Ind _ -> true | _ -> false

(* Destructs the product (x:t1)t2 *)
let destProd c = match kind_of_term c with 
  | Prod (x,imp,t1,t2) -> (x,imp,t1,t2) 
  | _ -> invalid_arg "destProd"

(* Destructs the abstraction [x:t1]t2 *)
let destLambda c = match kind_of_term c with 
  | Lambda (x,imp,t1,t2) -> (x,imp,t1,t2) 
  | _ -> invalid_arg "destLambda"

(* Destructs the let [x:=b:t1]t2 *)
let destLetIn c = match kind_of_term c with 
  | LetIn (x,imp,b,t1,t2) -> (x,imp,b,t1,t2) 
  | _ -> invalid_arg "destProd"

(* Destructs an application *)
let destApp c = match kind_of_term c with
  | App (f,a,imps) -> (f, a,imps)
  | _ -> invalid_arg "destApplication"

let destApplication = destApp

let isApp c = match kind_of_term c with App _ -> true | _ -> false

let isProd c = match kind_of_term c with | Prod _ -> true | _ -> false

let isLambda c = match kind_of_term c with | Lambda _ -> true | _ -> false

(* Destructs a constant *)
let destConst c = match kind_of_term c with
  | Const kn -> kn
  | _ -> invalid_arg "destConst"

let isConst c = match kind_of_term c with Const _ -> true | _ -> false

(* Destructs an existential variable *)
let destEvar c = match kind_of_term c with
  | Evar (kn, a as r) -> r
  | _ -> invalid_arg "destEvar"

let num_of_evar c = match kind_of_term c with
  | Evar (n, _) -> n
  | _ -> anomaly "num_of_evar called with bad args"

(* Destructs a (co)inductive type named kn *)
let destInd c = match kind_of_term c with
  | Ind (kn, a as r) -> r
  | _ -> invalid_arg "destInd"

(* Destructs a constructor *)
let destConstruct c = match kind_of_term c with
  | Construct (kn, a as r) -> r
  | _ -> invalid_arg "dest"

let isConstruct c = match kind_of_term c with
    Construct _ -> true | _ -> false

(* Destructs a term <p>Case c of lc1 | lc2 .. | lcn end *)
let destCase c = match kind_of_term c with
  | Case (ci,p,c,v) -> (ci,p,c,v)
  | _ -> anomaly "destCase"

let destFix c = match kind_of_term c with 
  | Fix fix -> fix
  | _ -> invalid_arg "destFix"
	
let destCoFix c = match kind_of_term c with 
  | CoFix cofix -> cofix
  | _ -> invalid_arg "destCoFix"

(******************************************************************)
(* Cast management                                                *)
(******************************************************************)

let rec strip_outer_cast c = match kind_of_term c with
  | Cast (c,_,_) -> strip_outer_cast c
  | _ -> c

(* Fonction sp�ciale qui laisse les cast cl�s sous les Fix ou les Case *)

let under_outer_cast f c =  match kind_of_term c with
  | Cast (b,k,t) -> mkCast (f b, k, f t)
  | _ -> f c

let rec under_casts f c = match kind_of_term c with
  | Cast (c,k,t) -> mkCast (under_casts f c, k, t)
  | _            -> f c

(******************************************************************)
(* Flattening and unflattening of embedded applications and casts *)
(******************************************************************)

(* flattens application lists throwing casts in-between *)
let rec collapse_appl c = match kind_of_term c with
  | App (f,cl,imps) -> 
      let rec collapse_rec f cl2 imps2 =
        match kind_of_term (strip_outer_cast f) with
	| App (g,cl1,imps1) ->
            collapse_rec g (Array.append cl1 cl2) (Array.append imps1 imps2)
	| _ -> mkApp (f,cl2,imps2)
      in
      collapse_rec f cl imps
  | _ -> c

let decompose_app c =
  match kind_of_term c with
    | App (f,cl,imps) -> (f, Array.to_list cl, Array.to_list imps)
    | _ -> (c,[],[])

(* strips head casts and flattens head applications *)
let rec strip_head_cast c = match kind_of_term c with
  | App (f,cl,imps) -> 
      let rec collapse_rec f cl2 imps2 = match kind_of_term f with
	| App (g,cl1,imps1) ->
            collapse_rec g (Array.append cl1 cl2) (Array.append imps1 imps2)
	| Cast (c,_,_) -> collapse_rec c cl2 imps2
	| _ -> if Array.length cl2 = 0 then f else mkApp (f,cl2,imps2)
      in 
      collapse_rec f cl imps
  | Cast (c,_,_) -> strip_head_cast c
  | _ -> c

(****************************************************************************)
(*              Functions to recur through subterms                         *)
(****************************************************************************)

(* [fold_constr f acc c] folds [f] on the immediate subterms of [c]
   starting from [acc] and proceeding from left to right according to
   the usual representation of the constructions; it is not recursive *)

let fold_constr f acc c = match kind_of_term c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> acc
  | Cast (c,_,t) -> f (f acc c) t
  | Prod (_,_,t,c) -> f (f acc t) c
  | Lambda (_,_,t,c) -> f (f acc t) c
  | LetIn (_,_,b,t,c) -> f (f (f acc b) t) c
  | App (c,l,_) -> Array.fold_left f (f acc c) l
  | Evar (_,l) -> Array.fold_left f acc l
  | Case (_,p,c,bl) -> Array.fold_left f (f (f acc p) c) bl
  | Fix (_,(lna,tl,bl)) ->
      let fd = array_map3 (fun na t b -> (na,t,b)) lna tl bl in
      Array.fold_left (fun acc (na,t,b) -> f (f acc t) b) acc fd
  | CoFix (_,(lna,tl,bl)) ->
      let fd = array_map3 (fun na t b -> (na,t,b)) lna tl bl in
      Array.fold_left (fun acc (na,t,b) -> f (f acc t) b) acc fd

(* [iter_constr f c] iters [f] on the immediate subterms of [c]; it is
   not recursive and the order with which subterms are processed is
   not specified *)

let iter_constr f c = match kind_of_term c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> ()
  | Cast (c,_,t) -> f c; f t
  | Prod (_,_,t,c) -> f t; f c
  | Lambda (_,_,t,c) -> f t; f c
  | LetIn (_,_,b,t,c) -> f b; f t; f c
  | App (c,l,_) -> f c; Array.iter f l
  | Evar (_,l) -> Array.iter f l
  | Case (_,p,c,bl) -> f p; f c; Array.iter f bl
  | Fix (_,(_,tl,bl)) -> Array.iter f tl; Array.iter f bl
  | CoFix (_,(_,tl,bl)) -> Array.iter f tl; Array.iter f bl

(* [iter_constr_with_binders g f n c] iters [f n] on the immediate
   subterms of [c]; it carries an extra data [n] (typically a lift
   index) which is processed by [g] (which typically add 1 to [n]) at
   each binder traversal; it is not recursive and the order with which
   subterms are processed is not specified *)

let iter_constr_with_binders g f n c = match kind_of_term c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> ()
  | Cast (c,_,t) -> f n c; f n t
  | Prod (_,_,t,c) -> f n t; f (g n) c
  | Lambda (_,_,t,c) -> f n t; f (g n) c
  | LetIn (_,_,b,t,c) -> f n b; f n t; f (g n) c
  | App (c,l,_) -> f n c; Array.iter (f n) l
  | Evar (_,l) -> Array.iter (f n) l
  | Case (_,p,c,bl) -> f n p; f n c; Array.iter (f n) bl
  | Fix (_,(_,tl,bl)) -> 
      Array.iter (f n) tl;
      Array.iter (f (iterate g (Array.length tl) n)) bl
  | CoFix (_,(_,tl,bl)) ->
      Array.iter (f n) tl;
      Array.iter (f (iterate g (Array.length tl) n)) bl

(* [map_constr f c] maps [f] on the immediate subterms of [c]; it is
   not recursive and the order with which subterms are processed is
   not specified *)

let map_constr f c = match kind_of_term c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> c
  | Cast (c,k,t) -> mkCast (f c, k, f t)
  | Prod (na,imp,t,c) -> mkProd (na, imp, f t, f c)
  | Lambda (na,imp,t,c) -> mkLambda (na, imp, f t, f c)
  | LetIn (na,imp,b,t,c) -> mkLetIn (na, imp, f b, f t, f c)
  | App (c,l,imps) -> mkApp (f c, Array.map f l, imps)
  | Evar (e,l) -> mkEvar (e, Array.map f l)
  | Case (ci,p,c,bl) -> mkCase (ci, f p, f c, Array.map f bl)
  | Fix (ln,(lna,tl,bl)) ->
      mkFix (ln,(lna,Array.map f tl,Array.map f bl))
  | CoFix(ln,(lna,tl,bl)) ->
      mkCoFix (ln,(lna,Array.map f tl,Array.map f bl))

(* [map_constr_with_binders g f n c] maps [f n] on the immediate
   subterms of [c]; it carries an extra data [n] (typically a lift
   index) which is processed by [g] (which typically add 1 to [n]) at
   each binder traversal; it is not recursive and the order with which
   subterms are processed is not specified *)

let map_constr_with_binders g f l c = match kind_of_term c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> c
  | Cast (c,k,t) -> mkCast (f l c, k, f l t)
  | Prod (na,imp,t,c) -> mkProd (na, imp, f l t, f (g l) c)
  | Lambda (na,imp,t,c) -> mkLambda (na, imp, f l t, f (g l) c)
  | LetIn (na,imp,b,t,c) -> mkLetIn (na, imp, f l b, f l t, f (g l) c)
  | App (c,al,imps) -> mkApp (f l c, Array.map (f l) al, imps)
  | Evar (e,al) -> mkEvar (e, Array.map (f l) al)
  | Case (ci,p,c,bl) -> mkCase (ci, f l p, f l c, Array.map (f l) bl)
  | Fix (ln,(lna,tl,bl)) ->
      let l' = iterate g (Array.length tl) l in
      mkFix (ln,(lna,Array.map (f l) tl,Array.map (f l') bl))
  | CoFix(ln,(lna,tl,bl)) ->
      let l' = iterate g (Array.length tl) l in
      mkCoFix (ln,(lna,Array.map (f l) tl,Array.map (f l') bl))

(* [compare_constr f c1 c2] compare [c1] and [c2] using [f] to compare
   the immediate subterms of [c1] of [c2] if needed; Cast's,
   application associativity, binders name and Cases annotations are
   not taken into account *)

let compare_constr f t1 t2 =
  match kind_of_term t1, kind_of_term t2 with
  | Rel n1, Rel n2 -> n1 = n2
  | Meta m1, Meta m2 -> m1 = m2
  | Var id1, Var id2 -> id1 = id2
  | Sort s1, Sort s2 -> s1 = s2
  | Cast (c1,_,_), _ -> f c1 t2
  | _, Cast (c2,_,_) -> f t1 c2
  | Prod (_,imp1,t1,c1), Prod (_,imp2,t2,c2) -> imp1=imp2 & f t1 t2 & f c1 c2
  | Lambda (_,imp1,t1,c1), Lambda (_,imp2,t2,c2) ->
      imp1=imp2 & f t1 t2 & f c1 c2
  | LetIn (_,imp1,b1,t1,c1), LetIn (_,imp2,b2,t2,c2) ->
      imp1 = imp2 & f b1 b2 & f t1 t2 & f c1 c2
  | App (c1,l1,imps1), App (c2,l2,imps2) ->
      if Array.length l1 = Array.length l2 then
        f c1 c2 & array_for_all2 f l1 l2 & imps1 = imps2
      else
        let (h1,l1,imps1) = decompose_app t1 in        
        let (h2,l2,imps2) = decompose_app t2 in
        if List.length l1 = List.length l2 then
          f h1 h2 & List.for_all2 f l1 l2 & imps1 = imps2
        else false
  | Evar (e1,l1), Evar (e2,l2) -> e1 = e2 & array_for_all2 f l1 l2
  | Const c1, Const c2 -> c1 = c2
  | Ind c1, Ind c2 -> c1 = c2
  | Construct c1, Construct c2 -> c1 = c2
  | Case (_,p1,c1,bl1), Case (_,p2,c2,bl2) ->
      f p1 p2 & f c1 c2 & array_for_all2 f bl1 bl2
  | Fix (ln1,(_,tl1,bl1)), Fix (ln2,(_,tl2,bl2)) ->
      ln1 = ln2 & array_for_all2 f tl1 tl2 & array_for_all2 f bl1 bl2
  | CoFix(ln1,(_,tl1,bl1)), CoFix(ln2,(_,tl2,bl2)) ->
      ln1 = ln2 & array_for_all2 f tl1 tl2 & array_for_all2 f bl1 bl2
  | _ -> false

(***************************************************************************)
(*     Type of assumptions                                                 *)
(***************************************************************************)

type types = constr

type strategy = types option 

let type_app f tt = f tt

let body_of_type ty = ty

type named_declaration = identifier * impl * constr option * types
type rel_declaration = name * impl * constr option * types

let map_named_declaration f (id, imp, v, ty) = (id, imp, option_map f v, f ty)
let map_rel_declaration = map_named_declaration

let fold_named_declaration f (_, _, v, ty) a = f ty (option_fold_right f v a)
let fold_rel_declaration = fold_named_declaration

(****************************************************************************)
(*              Functions for dealing with constr terms                     *)
(****************************************************************************)

(*********************)
(*     Occurring     *)
(*********************)

exception LocalOccur

(* (closedn n M) raises FreeVar if a variable of height greater than n
   occurs in M, returns () otherwise *)

let closedn n c = 
  let rec closed_rec n c = match kind_of_term c with
    | Rel m -> if m>n then raise LocalOccur
    | _ -> iter_constr_with_binders succ closed_rec n c
  in 
  try closed_rec n c; true with LocalOccur -> false

(* [closed0 M] is true iff [M] is a (deBruijn) closed term *)

let closed0 = closedn 0

(* (noccurn n M) returns true iff (Rel n) does NOT occur in term M  *)

let noccurn n term = 
  let rec occur_rec n c = match kind_of_term c with
    | Rel m -> if m = n then raise LocalOccur
    | _ -> iter_constr_with_binders succ occur_rec n c
  in 
  try occur_rec n term; true with LocalOccur -> false

(* (noccur_between n m M) returns true iff (Rel p) does NOT occur in term M 
  for n <= p < n+m *)

let noccur_between n m term = 
  let rec occur_rec n c = match kind_of_term c with
    | Rel(p) -> if n<=p && p<n+m then raise LocalOccur
    | _        -> iter_constr_with_binders succ occur_rec n c
  in 
  try occur_rec n term; true with LocalOccur -> false

(* Checking function for terms containing existential variables.
 The function [noccur_with_meta] considers the fact that
 each existential variable (as well as each isevar)
 in the term appears applied to its local context,
 which may contain the CoFix variables. These occurrences of CoFix variables
 are not considered *)

let noccur_with_meta n m term = 
  let rec occur_rec n c = match kind_of_term c with
    | Rel p -> if n<=p & p<n+m then raise LocalOccur
    | App(f,cl,imps) ->
	(match kind_of_term f with
           | Cast (c,_,_) when isMeta c -> ()
           | Meta _ -> ()
	   | _ -> iter_constr_with_binders succ occur_rec n c)
    | Evar (_, _) -> ()
    | _ -> iter_constr_with_binders succ occur_rec n c
  in
  try (occur_rec n term; true) with LocalOccur -> false


(*********************)
(*      Lifting      *)
(*********************)

(* The generic lifting function *)
let rec exliftn el c = match kind_of_term c with
  | Rel i -> mkRel(reloc_rel i el)
  | _ -> map_constr_with_binders el_lift exliftn el c

(* Lifting the binding depth across k bindings *)

let liftn k n = 
  match el_liftn (pred n) (el_shft k ELID) with
    | ELID -> (fun c -> c)
    | el -> exliftn el
 
let lift k = liftn k 1

(*********************)
(*   Substituting    *)
(*********************)

(* (subst1 M c) substitutes M for Rel(1) in c 
   we generalise it to (substl [M1,...,Mn] c) which substitutes in parallel
   M1,...,Mn for respectively Rel(1),...,Rel(n) in c *)

(* 1st : general case *)

type info = Closed | Open | Unknown
type 'a substituend = { mutable sinfo: info; sit: 'a }

let rec lift_substituend depth s =
  match s.sinfo with
    | Closed -> s.sit
    | Open -> lift depth s.sit
    | Unknown ->
        s.sinfo <- if closed0 s.sit then Closed else Open;
        lift_substituend depth s

let make_substituend c = { sinfo=Unknown; sit=c }

let substn_many lamv n c =
  let lv = Array.length lamv in 
  if lv = 0 then c
  else 
    let rec substrec depth c = match kind_of_term c with
      | Rel k     ->
          if k<=depth then c
          else if k-depth <= lv then lift_substituend depth lamv.(k-depth-1)
          else mkRel (k-lv)
      | _ -> map_constr_with_binders succ substrec depth c in 
    substrec n c

(*
let substkey = Profile.declare_profile "substn_many";;
let substn_many lamv n c = Profile.profile3 substkey substn_many lamv n c;;
*)

let substnl laml n =
  substn_many (Array.map make_substituend (Array.of_list laml)) n
let substl laml = substnl laml 0
let subst1 lam = substl [lam]

let substnl_decl laml k (id,imp,bodyopt,typ) =
  (id,imp,option_map (substnl laml k) bodyopt,substnl laml k typ)
let substl_decl laml = substnl_decl laml 0
let subst1_decl lam = substl_decl [lam]
let subst1_named_decl = subst1_decl
let substl_named_decl = substl_decl

(* (thin_val sigma) removes identity substitutions from sigma *)

let rec thin_val = function
  | [] -> []
  | (((id,{ sit = v }) as s)::tl) when isVar v -> 
      if id = destVar v then thin_val tl else s::(thin_val tl)
  | h::tl -> h::(thin_val tl)

(* (replace_vars sigma M) applies substitution sigma to term M *)
let replace_vars var_alist = 
  let var_alist =
    List.map (fun (str,c) -> (str,make_substituend c)) var_alist in
  let var_alist = thin_val var_alist in 
  let rec substrec n c = match kind_of_term c with
    | Var x ->
        (try lift_substituend n (List.assoc x var_alist)
         with Not_found -> c)
    | _ -> map_constr_with_binders succ substrec n c
  in 
  if var_alist = [] then (function x -> x) else substrec 0

(*
let repvarkey = Profile.declare_profile "replace_vars";;
let replace_vars vl c = Profile.profile2 repvarkey replace_vars vl c ;;
*)

(* (subst_var str t) substitute (VAR str) by (Rel 1) in t *)
let subst_var str = replace_vars [(str, mkRel 1)]

(* (subst_vars [id1;...;idn] t) substitute (VAR idj) by (Rel j) in t *)
let substn_vars p vars =
  let _,subst =
    List.fold_left (fun (n,l) var -> ((n+1),(var,mkRel n)::l)) (p,[]) vars
  in replace_vars (List.rev subst)

let subst_vars = substn_vars 1

(*********************)
(* Term constructors *)
(*********************)

(* Constructs a DeBrujin index with number n *)
let mkRel = mkRel

(* Constructs an existential variable named "?n" *)
let mkMeta = mkMeta

(* Constructs a Variable named id *)
let mkVar = mkVar

(* Construct a type *)
let mkProp   = mkSort mk_Prop
let mkSet    = mkSort mk_Set
let mkType u = mkSort (Type u)
let mkSort   = function
  | Prop Null -> mkProp (* Easy sharing *)
  | Prop Pos -> mkSet
  | s -> mkSort s

let prop = mk_Prop
and spec = mk_Set
and type_0 = Type prop_univ

(* Constructs the term t1::t2, i.e. the term t1 casted with the type t2 *)
(* (that means t2 is declared as the type of t1) *)
let mkCast = mkCast

(* Constructs the product (x:t1)t2 *)
let mkProd = mkProd
let mkNamedProd id imp typ c = mkProd (Name id, imp, typ, subst_var id c)
let mkProd_string   s imp t c = mkProd (Name (id_of_string s), imp, t, c)

(* Constructs the abstraction [x:t1]t2 *)
let mkLambda = mkLambda
let mkNamedLambda id imp typ c = mkLambda (Name id, imp, typ, subst_var id c)
let mkLambda_string s imp t c = mkLambda (Name (id_of_string s), imp, t, c)

(* Constructs [x=c_1:t]c_2 *)
let mkLetIn = mkLetIn
let mkNamedLetIn id imp c1 t c2 =
  mkLetIn (Name id, imp, c1, t, subst_var id c2)

(* Constructs either [(x:t)c] or [[x=b:t]c] *)
let mkProd_or_LetIn (na,imp,body,t) c =
  match body with
    | None -> mkProd (na, imp, t, c)
    | Some b -> mkLetIn (na, imp, b, t, c)

let mkNamedProd_or_LetIn (id,imp,body,t) c =
  match body with
    | None -> mkNamedProd id imp t c
    | Some b -> mkNamedLetIn id imp b t c

(* Constructs either [[x:t]c] or [[x=b:t]c] *)
let mkLambda_or_LetIn (na,imp,body,t) c =
  match body with
    | None -> mkLambda (na, imp, t, c)
    | Some b -> mkLetIn (na, imp, b, t, c)

let mkNamedLambda_or_LetIn (id,imp,body,t) c =
  match body with
    | None -> mkNamedLambda id imp t c
    | Some b -> mkNamedLetIn id imp b t c

(* Constructs either [(x:t)c] or [c] where [x] is replaced by [b] *)
let mkProd_wo_LetIn (na,imp,body,t) c =
  match body with
    | None -> mkProd (na, imp, t, c)
    | Some b -> subst1 b c

let mkNamedProd_wo_LetIn (id,imp,body,t) c =
  match body with
    | None -> mkNamedProd id imp t c
    | Some b -> subst1 b (subst_var id c)

(* non-dependent product t1 -> t2 *)
let mkArrow imp t1 t2 = mkProd (Anonymous, imp, t1, t2)

(* If lt = [t1; ...; tn], constructs the application (t1 ... tn) *)
(* We ensure applicative terms have at most one arguments and the
   function is not itself an applicative term *)
let mkApp = mkApp

(* Constructs a constant *) 
(* The array of terms correspond to the variables introduced in the section *)
let mkConst = mkConst

(* Constructs an existential variable *)
let mkEvar = mkEvar

(* Constructs the ith (co)inductive type of the block named kn *)
(* The array of terms correspond to the variables introduced in the section *)
let mkInd = mkInd

(* Constructs the jth constructor of the ith (co)inductive type of the 
   block named kn. The array of terms correspond to the variables
   introduced in the section *)
let mkConstruct = mkConstruct

(* Constructs the term <p>Case c of c1 | c2 .. | cn end *)
let mkCase = mkCase
let mkCaseL (ci, p, c, ac) = mkCase (ci, p, c, Array.of_list ac)

(* If recindxs = [|i1,...in|] 
      funnames = [|f1,...fn|]
      typarray = [|t1,...tn|]
      bodies   = [|b1,...bn|]
   then    

      mkFix ((recindxs,i),(funnames,typarray,bodies))
   
   constructs the ith function of the block  

    Fixpoint f1 [ctx1] : t1 := b1
    with     f2 [ctx2] : t2 := b2
    ...
    with     fn [ctxn] : tn := bn.

   where the lenght of the jth context is ij.
*)

let mkFix = mkFix

(* If funnames = [|f1,...fn|]
      typarray = [|t1,...tn|]
      bodies   = [|b1,...bn|]
   then 

      mkCoFix (i,(funnames,typsarray,bodies))

   constructs the ith function of the block  
   
    CoFixpoint f1 : t1 := b1
    with       f2 : t2 := b2
    ...
    with       fn : tn := bn.
*)
let mkCoFix = mkCoFix

(* Construct an implicit *)
let implicit_sort = Type (make_univ(make_dirpath[id_of_string"implicit"],0))
let mkImplicit = mkSort implicit_sort

(***************************)
(* Other term constructors *)
(***************************)

let abs_implicit c = mkLambda (Anonymous, Impl, mkImplicit, c)
let lambda_implicit a = mkLambda (Name(id_of_string"y"), Impl, mkImplicit, a)
let lambda_implicit_lift n a = iterate lambda_implicit n (lift n a)

(* prodn n [xn:Tn;..;x1:T1;Gamma] b = (x1:T1)..(xn:Tn)b *)
let prodn n env b =
  let rec prodrec = function
    | (0, env, b)        -> b
    | (n, ((v,imp,t)::l), b) -> prodrec (n-1,  l, mkProd (v,imp,t,b))
    | _ -> assert false
  in 
  prodrec (n,env,b)

(* compose_prod [xn:Tn;..;x1:T1] b = (x1:T1)..(xn:Tn)b *)
let compose_prod l b = prodn (List.length l) l b

(* lamn n [xn:Tn;..;x1:T1;Gamma] b = [x1:T1]..[xn:Tn]b *)
let lamn n env b =
  let rec lamrec = function
    | (0, env, b)        -> b
    | (n, ((v,imp,t)::l), b) -> lamrec (n-1,  l, mkLambda (v,imp,t,b))
    | _ -> assert false
  in 
  lamrec (n,env,b)

(* compose_lam [xn:Tn;..;x1:T1] b = [x1:T1]..[xn:Tn]b *)
let compose_lam l b = lamn (List.length l) l b

let applist (f,l,imps) = mkApp (f, Array.of_list l, Array.of_list imps)

let applistc f l imps = mkApp (f, Array.of_list l, Array.of_list imps)

let appvect = mkApp
	    
let appvectc f l imps = mkApp (f,l,imps)
		     
(* to_lambda n (x1:T1)...(xn:Tn)T =
 * [x1:T1]...[xn:Tn]T *)
let rec to_lambda n prod =
  if n = 0 then 
    prod 
  else 
    match kind_of_term prod with 
      | Prod (na,imp,ty,bd) -> mkLambda (na,imp,ty,to_lambda (n-1) bd)
      | Cast (c,_,_) -> to_lambda n c
      | _   -> errorlabstrm "to_lambda" (mt ())                      

let rec to_prod n lam =
  if n=0 then 
    lam
  else   
    match kind_of_term lam with 
      | Lambda (na,imp,ty,bd) -> mkProd (na,imp,ty,to_prod (n-1) bd)
      | Cast (c,_,_) -> to_prod n c
      | _   -> errorlabstrm "to_prod" (mt ())                      
	    
(* pseudo-reduction rule:
 * [prod_app  s (Prod(_,B)) N --> B[N]
 * with an strip_outer_cast on the first argument to produce a product *)

let prod_app t n =
  match kind_of_term (strip_outer_cast t) with
    | Prod (_,_,_,b) -> subst1 n b
    | _ ->
	errorlabstrm "prod_app"
	  (str"Needed a product, but didn't find one" ++ fnl ())


(* prod_appvect T [| a1 ; ... ; an |] -> (T a1 ... an) *)
let prod_appvect t nL = Array.fold_left prod_app t nL

(* prod_applist T [ a1 ; ... ; an ] -> (T a1 ... an) *)
let prod_applist t nL = List.fold_left prod_app t nL

(*********************************)
(* Other term destructors        *)
(*********************************)

(* Transforms a product term (x1:T1)..(xn:Tn)T into the pair
   ([(xn,Tn);...;(x1,T1)],T), where T is not a product *)
let decompose_prod = 
  let rec prodec_rec l c = match kind_of_term c with
    | Prod (x,imp,t,c) -> prodec_rec ((x,imp,t)::l) c
    | Cast (c,_,_)     -> prodec_rec l c
    | _                -> l,c
  in 
  prodec_rec []

(* Transforms a lambda term [x1:T1]..[xn:Tn]T into the pair
   ([(xn,Tn);...;(x1,T1)],T), where T is not a lambda *)
let decompose_lam = 
  let rec lamdec_rec l c = match kind_of_term c with
    | Lambda (x,imp,t,c) -> lamdec_rec ((x,imp,t)::l) c
    | Cast (c,_,_)       -> lamdec_rec l c
    | _                  -> l,c
  in 
  lamdec_rec []

(* Given a positive integer n, transforms a product term (x1:T1)..(xn:Tn)T 
   into the pair ([(xn,Tn);...;(x1,T1)],T) *)
let decompose_prod_n n =
  if n < 0 then error "decompose_prod_n: integer parameter must be positive";
  let rec prodec_rec l n c = 
    if n=0 then l,c 
    else match kind_of_term c with 
      | Prod (x,imp,t,c) -> prodec_rec ((x,imp,t)::l) (n-1) c
      | Cast (c,_,_)     -> prodec_rec l n c
      | _ -> error "decompose_prod_n: not enough products"
  in 
  prodec_rec [] n 

(* Given a positive integer n, transforms a lambda term [x1:T1]..[xn:Tn]T 
   into the pair ([(xn,Tn);...;(x1,T1)],T) *)
let decompose_lam_n n =
  if n < 0 then error "decompose_lam_n: integer parameter must be positive";
  let rec lamdec_rec l n c = 
    if n=0 then l,c 
    else match kind_of_term c with 
      | Lambda (x,imp,t,c) -> lamdec_rec ((x,imp,t)::l) (n-1) c
      | Cast (c,_,_)       -> lamdec_rec l n c
      | _ -> error "decompose_lam_n: not enough abstractions"
  in 
  lamdec_rec [] n 

(* (nb_lam [na1:T1]...[nan:Tan]c) where c is not an abstraction
 * gives n (casts are ignored) *)
let nb_lam = 
  let rec nbrec n c = match kind_of_term c with
    | Lambda (_,_,_,c) -> nbrec (n+1) c
    | Cast (c,_,_) -> nbrec n c
    | _ -> n
  in 
  nbrec 0
    
(* similar to nb_lam, but gives the number of products instead *)
let nb_prod = 
  let rec nbrec n c = match kind_of_term c with
    | Prod (_,_,_,c) -> nbrec (n+1) c
    | Cast (c,_,_) -> nbrec n c
    | _ -> n
  in 
  nbrec 0

(* Rem: end of import from old module Generic *)

(*******************************)
(*  alpha conversion functions *)                         
(*******************************)

(* alpha conversion : ignore print names and casts *)

let rec eq_constr m n = 
  (m==n) or
  compare_constr eq_constr m n

let eq_constr m n = eq_constr m n (* to avoid tracing a recursive fun *)

(*******************)
(*  hash-consing   *)                         
(*******************)

module Htype =
  Hashcons.Make(
    struct
      type t = types
      type u = (constr -> constr) * (sorts -> sorts)
(*
      let hash_sub (hc,hs) j = {body=hc j.body; typ=hs j.typ}
      let equal j1 j2 = j1.body==j2.body & j1.typ==j2.typ
*)
(**)
      let hash_sub (hc,hs) j = hc j
      let equal j1 j2 = j1==j2
(**)
      let hash = Hashtbl.hash
    end)

module Hsorts =
  Hashcons.Make(
    struct
      type t = sorts
      type u = universe -> universe
      let hash_sub huniv = function
          Prop c -> Prop c
        | Type u -> Type (huniv u)
      let equal s1 s2 =
        match (s1,s2) with
            (Prop c1, Prop c2) -> c1=c2
          | (Type u1, Type u2) -> u1 == u2
          |_ -> false
      let hash = Hashtbl.hash
    end)

let hsort = Hsorts.f

let hcons_constr (hcon,hkn,hdir,hname,hident,hstr) =
  let hsortscci = Hashcons.simple_hcons hsort hcons1_univ in
  let hcci = hcons_term (hsortscci,hcon,hkn,hname,hident) in
  let htcci = Hashcons.simple_hcons Htype.f (hcci,hsortscci) in
  (hcci,htcci)

let (hcons1_constr, hcons1_types) = hcons_constr (hcons_names())


(**************************************)

type 'constr erec_declaration = name array * 'constr array
type 'constr efixpoint = (int array * int) * 'constr erec_declaration
type 'constr ecofixpoint = int * 'constr erec_declaration

type 'a extracted_term =
  | ERel       of int
  | EVar       of identifier
  | EMeta      of metavariable
  | EEvar      of existential_key * 'a array
  | ESort      of sorts
  | EProd      of name * impl * 'a * 'a
  | ELambda    of name * 'a
  | ELetIn     of name * 'a * 'a
  | EApp       of 'a * 'a array
  | EConst     of constant
  | EInd       of inductive
  | EConstruct of constructor
  | ECase      of inductive * 'a * (name array * 'a) array
  | EFix       of 'a efixpoint
  | ECoFix     of 'a ecofixpoint

let eapp(f,v) =
  if Array.length v = 0 then f else
    match f with
        EApp(f',v') -> EApp(f',Array.append v' v)
      | _ -> EApp(f,v)

let rec dest_pats acc n s c =
  match n,kind_of_term c with
    | 0,_ -> Array.of_list (List.rev acc), s, c
    | _,Lambda(x,Expl,_,b) -> dest_pats (x::acc) (n-1) (subs_lift s) b
    | _,Lambda(x,Impl,_,b) -> dest_pats acc (n-1) (subs_cons([|()|],s)) b
    | _ -> failwith "branch not in eta long form"

let translate_rec_index idx c =
  let rec transl acc i trm =
    if i=0 then acc 
    else match trm with
      | Lambda(_,Expl,_,b) -> transl (acc+1) (i-1) b
      | Lambda(_,Impl,_,b) -> transl acc (i-1) b
      | _ -> assert false in
  transl 0 idx c

let extract t =
  let rec extr s = function
    | Rel i -> 
        (match expand_rel i s with
            Inl _ -> assert false
          | Inr (k,_) -> ERel k)
    | Var x -> EVar x
    | Meta mv -> EMeta mv
    | Evar(ev,inst) -> EEvar (ev, Array.map (extr s) inst)
    | Sort s -> ESort s
    | Cast(c,_,_) -> extr s c
    | Prod(na,imp,a,b) -> EProd(na,imp,extr s a, extr(subs_lift s) b)
    | Lambda(na,Expl,a,b) -> ELambda(na,extr(subs_lift s) b)
    | Lambda(na,Impl,a,b) -> extr (subs_cons([|()|],s)) b
    | LetIn(na,Expl,a,b,c) -> ELetIn(na,extr s a,extr(subs_lift s) c)
    | LetIn(na,Impl,a,b,c) -> extr(subs_cons([|()|], s)) c
    | App(f,args,imps) ->
        eapp(extr s f, Array.map (extr s) (expl_filter args imps))
    | Const c -> EConst c
    | Ind i -> EInd i
    | Construct c -> EConstruct c
    | Case(ci,_,b,br) ->
        let extr_branch i c =
          let nargs = ci.ci_cstr_nargs.(i) in
          let (pats,s',b) = dest_pats [] nargs s c in
          pats,extr s' b in
        ECase(ci.ci_ind, extr s b, Array.mapi extr_branch br)
    | Fix((ra,i),(ln,_,lb)) ->
        let ra' = array_map2 translate_rec_index ra lb in
        let s' = subs_liftn (Array.length lb) s in
        EFix((ra',i),(ln,Array.map (extr s') lb)) 
    | CoFix(i,(ln,_,lb)) ->
        let s' = subs_liftn (Array.length lb) s in
        ECoFix(i,(ln,Array.map (extr s') lb)) in
  extr (ESID 0) t

type econstr = econstr extracted_term
let kind_of_econstr x = x
let fold_econstr x = x


let lft n l = List.map (fun i->i+n) l

let rec is_id funs t =
  match kind_of_econstr t with
      ERel i when List.mem i funs -> ()
    | ELambda(_,u) -> is_var (lft 1 funs) 1 u
    | EFix(_,(_,[|u|])) -> is_id (1::lft 1 funs) u
    | _ -> raise Not_found

and is_var funs i t =
  match kind_of_econstr t with
      ERel j when i=j -> ()
    | EApp(f,[|u|]) -> is_id funs f; is_var funs i u
    | ELambda(_,u) -> is_var_app (lft 1 funs) 1 i u
    | ECase(_,c,b) -> is_var funs i c;
        Array.iteri
          (fun i (k,b) -> let n = Array.length k in
           is_constr (lft n funs) (i+1) n b) b
    | _ -> raise Not_found

and is_var_app funs k i t =
  match kind_of_econstr t with
      ELambda(_,u) -> is_var_app (lft 1 funs) (k+1) (i+1) u
    | EApp(f,args) -> is_inst funs k args; is_var funs i f
    | _ -> raise Not_found

and is_constr funs i k t =
  match kind_of_econstr t with
      EConstruct(_,j) when i=j && k=0 -> ()
    | EApp(f,args) -> is_inst funs k args; is_constr funs i 0 f
    | _ -> raise Not_found

and is_inst funs k args =
  if Array.length args <> k then raise Not_found;
  Array.iteri
    (fun i t -> is_var funs (k-i) t) args

let is_coercion t =
  try is_id [] t; true
  with Not_found -> false


(*******)
(* Type of abstract machine values *)
type values



(* spiwack : internal representation printing *)
let string_of_sorts =
  function 
  | Prop Pos -> "Prop Pos"
  | Prop Null -> "Prop Null"
  | Type u -> "Type "^string_of_universe u

let string_of_cast_kind = 
  function
  |VMcast -> "VMcast"
  | DEFAULTcast -> "DEFAULTcast"

let string_of_impl_name imp na =
  if imp=Impl then "["^string_of_name na^"]"
  else string_of_name na

let rec string_of_constr = 
  let string_of_term string_of_expr string_of_type = function
  | Rel i -> "Rel "^string_of_int i
  | Var id -> "Var "^string_of_identifier id
  | Meta mv -> "Meta "^"mv?" (* need a function for printing metavariables *)
  | Evar ev -> "Evar "^"ev?" (* ??     of 'constr pexistential *)
  | Sort s -> "Sort "^string_of_sorts s
  | Cast (e,ck,t) -> 
         "Cast ("^string_of_expr e^", "^string_of_cast_kind ck^", "^
                 string_of_type t^")" 
  | Prod (n, imp, t1, t2) -> 
          "Prod ("^string_of_impl_name imp n^", "^string_of_type t1^", "^
                   string_of_type t2^")"  
  | Lambda (n,imp,t,e) ->
          "Lambda ("^string_of_impl_name imp n^", "^string_of_type t^", "^
                    string_of_expr e^")"
  | LetIn (n, imp, e1, t, e2) -> 
           "LetIn ("^string_of_impl_name imp n^", "^string_of_expr e1^", "^
           string_of_type t^", "^string_of_expr e2^")"
  | App   (e, args,imps) ->  "App ("^string_of_expr e^", [|"^
          String.concat "; " (Array.to_list (Array.map string_of_expr args)) ^
          "|])"
  | Const c ->  "Const "^string_of_constant c
  | Ind   ind ->  "Ind "^string_of_inductive ind
  | Construct ctr -> "Construct "^string_of_constructor ctr
  | Case(_,_,_,_)  -> "Case ..."    
             (*    of case_info * 'constr * 'constr * 'constr array   *)
  | Fix _  -> "Fix ..."  (*    of ('constr, 'types) pfixpoint *)
  | CoFix _ -> "CoFix ..."  (*     of ('constr, 'types) pcofixpoint *)
in 
fun x -> string_of_term string_of_constr string_of_constr x


(* /spiwack *)