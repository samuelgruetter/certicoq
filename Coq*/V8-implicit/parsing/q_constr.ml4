(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4use: "pa_extend.cmo q_MLast.cmo" i*)

(* $Id: g_constr.ml4,v 1.58 2005/12/30 10:55:32 herbelin Exp $ *)

open Rawterm
open Term
open Names
open Pattern
open Q_util
open Util
open Pcaml

let loc = dummy_loc
let dloc = <:expr< Util.dummy_loc >>

let apply_ref f l imps = 
  <:expr< 
    Rawterm.RApp
    ($dloc$, Rawterm.RRef ($dloc$, Lazy.force $f$),
     $mlexpr_of_list (fun x -> x) l$,
     $mlexpr_of_list Q_coqast.mlexpr_of_impl imps$)
  >>

EXTEND
  GLOBAL: expr;
  expr:
    [ [ "PATTERN"; "["; c = constr; "]" ->
      <:expr< snd (Pattern.pattern_of_rawconstr $c$) >> ] ]
  ;
  sort:
    [ [ "Set"  -> RProp Pos
      | "Prop" -> RProp Null
      | "Type" -> RType None ] ]
  ;
  ident:
    [ [ s = string -> <:expr< Names.id_of_string $str:s$ >> ] ]
  ;
  name:
    [ [ "_" -> <:expr< Anonymous >> | id = ident -> <:expr< Name $id$ >> ] ]
  ;
  string:
    [ [ UIDENT | LIDENT ] ]
  ;
  constr:
    [ "200" RIGHTA
      [ LIDENT "forall"; id = ident; ":"; c1 = constr; ","; c2 = constr ->
        <:expr< Rawterm.RProd ($dloc$,Name $id$,Term.Expl,$c1$,$c2$) >>
      | "fun"; id = ident; ":"; c1 = constr; "=>"; c2 = constr ->
        <:expr< Rawterm.RLambda ($dloc$,Name $id$,Term.Expl,$c1$,$c2$) >>
      | LIDENT "forall"; "["; id = ident; ":"; c1 = constr; "]"; ",";
        c2 = constr ->
        <:expr< Rawterm.RProd ($dloc$,Name $id$,Term.Impl,$c1$,$c2$) >>
      | "fun"; "["; id = ident; ":"; c1 = constr; "]"; "=>"; c2 = constr ->
        <:expr< Rawterm.RLambda ($dloc$,Name $id$,Term.Impl,$c1$,$c2$) >>
      | "let"; id = ident; ":="; c1 = constr; "in"; c2 = constr ->
        <:expr< Rawterm.RLetin ($dloc$,Name $id$,$c1$,$c2$) >>
      (* fix todo *)
      ]
    | "100" RIGHTA
      [ c1 = constr; ":"; c2 = SELF -> 
        <:expr< Rawterm.RCast($dloc$,$c1$,DEFAULTcast,$c2$) >> ]
    | "90" RIGHTA
      [ c1 = constr; "->"; c2 = SELF -> 
        <:expr< Rawterm.RProd ($dloc$,Anonymous,Term.Expl,$c1$,$c2$) >>
      | "["; c1 = constr; "]"; "->"; c2 = SELF -> 
        <:expr< Rawterm.RProd ($dloc$,Anonymous,Term.Impl,$c1$,$c2$) >> ]
    | "75" RIGHTA
      [ "~"; c = constr -> 
        apply_ref <:expr< coq_not_ref >> [c] [Expl] ]
    | "70" RIGHTA
      [ c1 = constr; "="; c2 = NEXT; ":>"; t = NEXT ->
        apply_ref <:expr< coq_eq_ref >> [t;c1;c2] [Expl;Expl;Expl] ]
    | "10" LEFTA
      [ f = constr;
        args = LIST1[a=constr LEVEL"0"-> (a,<:expr<Term.Expl>>)
                    | "[";a=constr;"]"-> (a,<:expr<Term.Impl>>) ] ->
        let args, imps = List.split args in
        let args = mlexpr_of_list (fun x -> x) args in
        let imps = mlexpr_of_list (fun x -> x) imps in
        <:expr< Rawterm.RApp ($dloc$,$f$,$args$,$imps$) >> ]
    | "0"
      [ s = sort -> <:expr< Rawterm.RSort ($dloc$,s) >>
      | id = ident -> <:expr< Rawterm.RVar ($dloc$,$id$) >>
      | "_" -> <:expr< Rawterm.RHole ($dloc$, QuestionMark False) >>
      | "?"; id = ident -> <:expr< Rawterm.RPatVar($dloc$,(False,$id$)) >>
      | "{"; c1 = constr; "}"; "+"; "{"; c2 = constr; "}" ->
          apply_ref <:expr< coq_sumbool_ref >> [c1;c2][Expl;Expl]
      | "%"; e = string -> <:expr< Rawterm.RRef ($dloc$,Lazy.force $lid:e$) >>
      | c = match_constr -> c
      | "("; c = constr LEVEL "200"; ")" -> c ] ]
  ;
  match_constr:
    [ [ "match"; c = constr LEVEL "100"; (ty,nal) = match_type;
        "with"; OPT"|"; br = LIST0 eqn SEP "|"; "end" -> 
          let br = mlexpr_of_list (fun x -> x) br in
     <:expr< Rawterm.RCases ($dloc$,$ty$,[($c$,$nal$)],$br$) >> 
    ] ]
  ;
  match_type:
    [ [ "as"; id = ident; "in"; ind = LIDENT; nal = LIST0 name;  
        "return"; ty = constr LEVEL "100" -> 
          let nal = mlexpr_of_list (fun x -> x) nal in
          <:expr< Some $ty$ >>, 
          <:expr< (Name $id$, Some ($dloc$,$lid:ind$,$nal$)) >> 
      | -> <:expr< None >>, <:expr< (Anonymous, None) >> ] ]
  ;
  eqn:
    [ [ (lid,pl) = pattern; "=>"; rhs = constr -> 
        let lid = mlexpr_of_list (fun x -> x) lid in
        <:expr< ($dloc$,$lid$,[$pl$],$rhs$) >> 
    ] ]
  ;
  pattern: 
    [ [ "%"; e = string; lip = LIST0 patvar ->
        let lp = mlexpr_of_list (fun (_,x) -> x) lip in
        let lid = List.flatten (List.map fst lip) in
        lid, <:expr< Rawterm.PatCstr ($dloc$,$lid:e$,$lp$,Anonymous) >>
      | p = patvar -> p
      | "("; p = pattern; ")" -> p ] ]
  ;
  patvar:
    [ [ "_" -> [], <:expr< Rawterm.PatVar ($dloc$,Anonymous) >> 
      | id = ident -> [id], <:expr< Rawterm.PatVar ($dloc$,Name $id$) >> 
    ] ]
  ;
  END;;

(* Example 
open Coqlib
let a = PATTERN [ match ?X with %path_of_S n => n | %path_of_O => ?X end ]
*)
