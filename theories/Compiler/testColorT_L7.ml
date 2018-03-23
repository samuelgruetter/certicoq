
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val xorb : bool -> bool -> bool **)

let xorb b1 b2 =
  if b1 then if b2 then false else true else b2

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Pervasives.succ (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

type compareSpecT =
| CompEqT
| CompLtT
| CompGtT

(** val compareSpec2Type : comparison -> compareSpecT **)

let compareSpec2Type = function
| Eq -> CompEqT
| Lt -> CompLtT
| Gt -> CompGtT

type 'a compSpecT = compareSpecT

(** val compSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 compSpecT **)

let compSpec2Type _ _ =
  compareSpec2Type

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



module Coq__1 = struct
 (** val add : int -> int -> int **)let rec add = (+)
end
include Coq__1

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Pervasives.max 0 (n-m)

(** val bool_dec : bool -> bool -> bool **)

let bool_dec b1 b2 =
  if b1 then if b2 then true else false else if b2 then false else true

(** val eqb : bool -> bool -> bool **)

let eqb b1 b2 =
  if b1 then b2 else if b2 then false else true

type reflect =
| ReflectT
| ReflectF

(** val iff_reflect : bool -> reflect **)

let iff_reflect = function
| true -> ReflectT
| false -> ReflectF

module Nat =
 struct
  type t = int

  (** val zero : int **)

  let zero =
    0

  (** val one : int **)

  let one =
    Pervasives.succ 0

  (** val two : int **)

  let two =
    Pervasives.succ (Pervasives.succ 0)

  (** val succ : int -> int **)

  let succ x =
    Pervasives.succ x

  (** val pred : int -> int **)

  let pred n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n0)
      (fun u -> u)
      n0

  (** val add : int -> int -> int **)

  let rec add n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> m)
      (fun p -> Pervasives.succ (add p m))
      n0

  (** val double : int -> int **)

  let double n0 =
    add n0 n0

  (** val mul : int -> int -> int **)

  let rec mul n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun p -> add m (mul p m))
      n0

  (** val sub : int -> int -> int **)

  let rec sub n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n0)
      (fun k -> (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> n0)
                  (fun l -> sub k l)
                  m)
      n0

  (** val ltb : int -> int -> bool **)

  let ltb n0 m =
    (<=) (Pervasives.succ n0) m

  (** val compare : int -> int -> comparison **)

  let rec compare = fun n m -> if n=m then Eq else if n<m then Lt else Gt

  (** val max : int -> int -> int **)

  let rec max n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> m)
      (fun n' -> (fun fO fS n -> if n=0 then fO () else fS (n-1))
                   (fun _ -> n0)
                   (fun m' -> Pervasives.succ (max n' m'))
                   m)
      n0

  (** val min : int -> int -> int **)

  let rec min n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' -> (fun fO fS n -> if n=0 then fO () else fS (n-1))
                   (fun _ -> 0)
                   (fun m' -> Pervasives.succ (min n' m'))
                   m)
      n0

  (** val even : int -> bool **)

  let rec even n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun n1 -> (fun fO fS n -> if n=0 then fO () else fS (n-1))
                   (fun _ -> false)
                   (fun n' -> even n')
                   n1)
      n0

  (** val odd : int -> bool **)

  let odd n0 =
    negb (even n0)

  (** val pow : int -> int -> int **)

  let rec pow n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Pervasives.succ 0)
      (fun m0 -> mul n0 (pow n0 m0))
      m

  (** val divmod : int -> int -> int -> int -> int * int **)

  let rec divmod x y q u =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> (q, u))
      (fun x' ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> divmod x' y (Pervasives.succ q) y)
        (fun u' -> divmod x' y q u')
        u)
      x

  (** val div : int -> int -> int **)

  let div x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> y)
      (fun y' -> fst (divmod x y' 0 y'))
      y

  (** val modulo : int -> int -> int **)

  let modulo x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> y)
      (fun y' -> sub y' (snd (divmod x y' 0 y')))
      y

  (** val gcd : int -> int -> int **)

  let rec gcd a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> b)
      (fun a' -> gcd (modulo b (Pervasives.succ a')) (Pervasives.succ a'))
      a

  (** val square : int -> int **)

  let square n0 =
    mul n0 n0

  (** val sqrt_iter : int -> int -> int -> int -> int **)

  let rec sqrt_iter k p q r =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun k' ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ ->
        sqrt_iter k' (Pervasives.succ p) (Pervasives.succ (Pervasives.succ q)) (Pervasives.succ (Pervasives.succ q)))
        (fun r' -> sqrt_iter k' p q r')
        r)
      k

  (** val sqrt : int -> int **)

  let sqrt n0 =
    sqrt_iter n0 0 0 0

  (** val log2_iter : int -> int -> int -> int -> int **)

  let rec log2_iter k p q r =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun k' ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> log2_iter k' (Pervasives.succ p) (Pervasives.succ q) q)
        (fun r' -> log2_iter k' p (Pervasives.succ q) r')
        r)
      k

  (** val log2 : int -> int **)

  let log2 n0 =
    log2_iter (pred n0) 0 (Pervasives.succ 0) 0

  (** val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec iter n0 f x =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> x)
      (fun n1 -> f (iter n1 f x))
      n0

  (** val div2 : int -> int **)

  let rec div2 = fun n -> n/2

  (** val testbit : int -> int -> bool **)

  let rec testbit a n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> odd a)
      (fun n1 -> testbit (div2 a) n1)
      n0

  (** val shiftl : int -> int -> int **)

  let rec shiftl a n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> a)
      (fun n1 -> double (shiftl a n1))
      n0

  (** val shiftr : int -> int -> int **)

  let rec shiftr a n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> a)
      (fun n1 -> div2 (shiftr a n1))
      n0

  (** val bitwise : (bool -> bool -> bool) -> int -> int -> int -> int **)

  let rec bitwise op n0 a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' ->
      add (if op (odd a) (odd b) then Pervasives.succ 0 else 0)
        (mul (Pervasives.succ (Pervasives.succ 0)) (bitwise op n' (div2 a) (div2 b))))
      n0

  (** val coq_land : int -> int -> int **)

  let coq_land a b =
    bitwise (&&) a a b

  (** val coq_lor : int -> int -> int **)

  let coq_lor a b =
    bitwise (||) (max a b) a b

  (** val ldiff : int -> int -> int **)

  let ldiff a b =
    bitwise (fun b0 b' -> (&&) b0 (negb b')) a a b

  (** val coq_lxor : int -> int -> int **)

  let coq_lxor a b =
    bitwise xorb (max a b) a b

  (** val recursion : 'a1 -> (int -> 'a1 -> 'a1) -> int -> 'a1 **)

  let rec recursion x f n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> x)
      (fun n1 -> f n1 (recursion x f n1))
      n0

  (** val leb_spec0 : int -> int -> reflect **)

  let leb_spec0 x y =
    iff_reflect ((<=) x y)

  (** val ltb_spec0 : int -> int -> reflect **)

  let ltb_spec0 x y =
    iff_reflect (ltb x y)

  module Private_OrderTac =
   struct
    module IsTotal =
     struct
     end

    module Tac =
     struct
     end
   end

  module Private_Tac =
   struct
   end

  module Private_Dec =
   struct
    (** val max_case_strong : int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

    let max_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat n0 (max n0 m) __ (hl __)
       | _ -> compat m (max n0 m) __ (hr __))

    (** val max_case : int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)

    let max_case n0 m x x0 x1 =
      max_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)

    (** val max_dec : int -> int -> bool **)

    let max_dec n0 m =
      max_case n0 m (fun _ _ _ h0 -> h0) true false

    (** val min_case_strong : int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

    let min_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat m (min n0 m) __ (hr __)
       | _ -> compat n0 (min n0 m) __ (hl __))

    (** val min_case : int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)

    let min_case n0 m x x0 x1 =
      min_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)

    (** val min_dec : int -> int -> bool **)

    let min_dec n0 m =
      min_case n0 m (fun _ _ _ h0 -> h0) true false
   end

  (** val max_case_strong : int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

  let max_case_strong n0 m x x0 =
    Private_Dec.max_case_strong n0 m (fun _ _ _ x1 -> x1) x x0

  (** val max_case : int -> int -> 'a1 -> 'a1 -> 'a1 **)

  let max_case n0 m x x0 =
    max_case_strong n0 m (fun _ -> x) (fun _ -> x0)

  (** val max_dec : int -> int -> bool **)

  let max_dec =
    Private_Dec.max_dec

  (** val min_case_strong : int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

  let min_case_strong n0 m x x0 =
    Private_Dec.min_case_strong n0 m (fun _ _ _ x1 -> x1) x x0

  (** val min_case : int -> int -> 'a1 -> 'a1 -> 'a1 **)

  let min_case n0 m x x0 =
    min_case_strong n0 m (fun _ -> x) (fun _ -> x0)

  (** val min_dec : int -> int -> bool **)

  let min_dec =
    Private_Dec.min_dec

  module Private_Parity =
   struct
   end

  module Private_NZPow =
   struct
   end

  module Private_NZSqrt =
   struct
   end

  (** val sqrt_up : int -> int **)

  let sqrt_up a =
    match compare 0 a with
    | Lt -> Pervasives.succ (sqrt (pred a))
    | _ -> 0

  (** val log2_up : int -> int **)

  let log2_up a =
    match compare (Pervasives.succ 0) a with
    | Lt -> Pervasives.succ (log2 (pred a))
    | _ -> 0

  module Private_NZDiv =
   struct
   end

  (** val lcm : int -> int -> int **)

  let lcm a b =
    mul a (div b (gcd a b))

  (** val eqb_spec : int -> int -> reflect **)

  let eqb_spec x y =
    iff_reflect ((=) x y)

  (** val b2n : bool -> int **)

  let b2n = function
  | true -> Pervasives.succ 0
  | false -> 0

  (** val setbit : int -> int -> int **)

  let setbit a n0 =
    coq_lor a (shiftl (Pervasives.succ 0) n0)

  (** val clearbit : int -> int -> int **)

  let clearbit a n0 =
    ldiff a (shiftl (Pervasives.succ 0) n0)

  (** val ones : int -> int **)

  let ones n0 =
    pred (shiftl (Pervasives.succ 0) n0)

  (** val lnot : int -> int -> int **)

  let lnot a n0 =
    coq_lxor a (ones n0)
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p -> (match y with
               | XI q -> XO (add_carry p q)
               | XO q -> XI (add p q)
               | XH -> XO (succ p))
    | XO p -> (match y with
               | XI q -> XI (add p q)
               | XO q -> XO (add p q)
               | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p -> (match y with
               | XI q -> XI (add_carry p q)
               | XO q -> XO (add_carry p q)
               | XH -> XI (succ p))
    | XO p -> (match y with
               | XI q -> XO (add_carry p q)
               | XO q -> XI (add p q)
               | XH -> XO (succ p))
    | XH -> (match y with
             | XI q -> XI (succ q)
             | XO q -> XO (succ q)
             | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val pred_N : positive -> n **)

  let pred_N = function
  | XI p -> Npos (XO p)
  | XO p -> Npos (pred_double p)
  | XH -> N0

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p -> (match y with
               | XI q -> double_mask (sub_mask p q)
               | XO q -> succ_double_mask (sub_mask p q)
               | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1 **)

  let rec iter f x = function
  | XI n' -> f (iter f (iter f x n') n')
  | XO n' -> iter f (iter f x n') n'
  | XH -> f x

  (** val div2 : positive -> positive **)

  let div2 = function
  | XI p0 -> p0
  | XO p0 -> p0
  | XH -> XH

  (** val div2_up : positive -> positive **)

  let div2_up = function
  | XI p0 -> succ p0
  | XO p0 -> p0
  | XH -> XH

  (** val size : positive -> positive **)

  let rec size = function
  | XI p0 -> succ (size p0)
  | XO p0 -> succ (size p0)
  | XH -> XH

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p -> (match y with
               | XI q -> compare_cont r p q
               | XO q -> compare_cont Gt p q
               | XH -> Gt)
    | XO p -> (match y with
               | XI q -> compare_cont Lt p q
               | XO q -> compare_cont r p q
               | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val coq_Nsucc_double : n -> n **)

  let coq_Nsucc_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val coq_Ndouble : n -> n **)

  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val coq_lor : positive -> positive -> positive **)

  let rec coq_lor p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> XI (coq_lor p0 q0)
                | XO q0 -> XI (coq_lor p0 q0)
                | XH -> p)
    | XO p0 -> (match q with
                | XI q0 -> XI (coq_lor p0 q0)
                | XO q0 -> XO (coq_lor p0 q0)
                | XH -> XI p0)
    | XH -> (match q with
             | XO q0 -> XI q0
             | _ -> q)

  (** val coq_land : positive -> positive -> n **)

  let rec coq_land p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> coq_Nsucc_double (coq_land p0 q0)
                | XO q0 -> coq_Ndouble (coq_land p0 q0)
                | XH -> Npos XH)
    | XO p0 -> (match q with
                | XI q0 -> coq_Ndouble (coq_land p0 q0)
                | XO q0 -> coq_Ndouble (coq_land p0 q0)
                | XH -> N0)
    | XH -> (match q with
             | XO _ -> N0
             | _ -> Npos XH)

  (** val ldiff : positive -> positive -> n **)

  let rec ldiff p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> coq_Ndouble (ldiff p0 q0)
                | XO q0 -> coq_Nsucc_double (ldiff p0 q0)
                | XH -> Npos (XO p0))
    | XO p0 -> (match q with
                | XI q0 -> coq_Ndouble (ldiff p0 q0)
                | XO q0 -> coq_Ndouble (ldiff p0 q0)
                | XH -> Npos p)
    | XH -> (match q with
             | XO _ -> Npos XH
             | _ -> N0)

  (** val coq_lxor : positive -> positive -> n **)

  let rec coq_lxor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XO q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XO q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XH -> Npos (XI p0))
    | XH -> (match q with
             | XI q0 -> Npos (XO q0)
             | XO q0 -> Npos (XI q0)
             | XH -> N0)

  (** val shiftl_nat : positive -> int -> positive **)

  let rec shiftl_nat p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun n1 -> XO (shiftl_nat p n1))
      n0

  (** val shiftr_nat : positive -> int -> positive **)

  let rec shiftr_nat p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun n1 -> div2 (shiftr_nat p n1))
      n0

  (** val testbit : positive -> n -> bool **)

  let rec testbit p n0 =
    match p with
    | XI p0 -> (match n0 with
                | N0 -> true
                | Npos n1 -> testbit p0 (pred_N n1))
    | XO p0 -> (match n0 with
                | N0 -> false
                | Npos n1 -> testbit p0 (pred_N n1))
    | XH -> (match n0 with
             | N0 -> true
             | Npos _ -> false)

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> int **)

  let to_nat x =
    iter_op Coq__1.add x (Pervasives.succ 0)

  (** val of_succ_nat : int -> positive **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> XH)
      (fun x -> succ (of_succ_nat x))
      n0

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | XI p0 -> (match x0 with
                | XI p1 -> eq_dec p0 p1
                | _ -> false)
    | XO p0 -> (match x0 with
                | XO p1 -> eq_dec p0 p1
                | _ -> false)
    | XH -> (match x0 with
             | XH -> true
             | _ -> false)
 end

module N =
 struct
  (** val succ_double : n -> n **)

  let succ_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val double : n -> n **)

  let double = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val succ_pos : n -> positive **)

  let succ_pos = function
  | N0 -> XH
  | Npos p -> Coq_Pos.succ p

  (** val add : n -> n -> n **)

  let add n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Npos (Coq_Pos.add p q))

  (** val sub : n -> n -> n **)

  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' -> (match m with
                  | N0 -> n0
                  | Npos m' -> (match Coq_Pos.sub_mask n' m' with
                                | Coq_Pos.IsPos p -> Npos p
                                | _ -> N0))

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Npos (Coq_Pos.mul p q))

  (** val compare : n -> n -> comparison **)

  let compare n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> Eq
             | Npos _ -> Lt)
    | Npos n' -> (match m with
                  | N0 -> Gt
                  | Npos m' -> Coq_Pos.compare n' m')

  (** val leb : n -> n -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val pos_div_eucl : positive -> n -> n * n **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = succ_double r in if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r')
    | XO a' ->
      let (q, r) = pos_div_eucl a' b in let r' = double r in if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r')
    | XH -> (match b with
             | N0 -> (N0, (Npos XH))
             | Npos p -> (match p with
                          | XH -> ((Npos XH), N0)
                          | _ -> (N0, (Npos XH))))

  (** val coq_lor : n -> n -> n **)

  let coq_lor n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Npos (Coq_Pos.coq_lor p q))

  (** val coq_land : n -> n -> n **)

  let coq_land n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Coq_Pos.coq_land p q)

  (** val ldiff : n -> n -> n **)

  let rec ldiff n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Coq_Pos.ldiff p q)

  (** val coq_lxor : n -> n -> n **)

  let coq_lxor n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Coq_Pos.coq_lxor p q)

  (** val testbit : n -> n -> bool **)

  let testbit a n0 =
    match a with
    | N0 -> false
    | Npos p -> Coq_Pos.testbit p n0

  (** val to_nat : n -> int **)

  let to_nat = function
  | N0 -> 0
  | Npos p -> Coq_Pos.to_nat p

  (** val of_nat : int -> n **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> N0)
      (fun n' -> Npos (Coq_Pos.of_succ_nat n'))
      n0

  (** val eq_dec : n -> n -> bool **)

  let eq_dec n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> true
             | Npos _ -> false)
    | Npos x -> (match m with
                 | N0 -> false
                 | Npos p0 -> Coq_Pos.eq_dec x p0)
 end

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

module Coq__2 = struct
 (** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)let rec map f = function
 | [] -> []
 | a :: t0 -> (f a) :: (map f t0)
end
include Coq__2

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t0 -> fold_left f t0 (f a0 b)

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Coq_Pos.pred_double p)

  (** val pred_double : z -> z **)

  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Coq_Pos.pred_double p)
  | Zneg p -> Zneg (XI p)

  (** val pos_sub : positive -> positive -> z **)

  let rec pos_sub x y =
    match x with
    | XI p -> (match y with
               | XI q -> double (pos_sub p q)
               | XO q -> succ_double (pos_sub p q)
               | XH -> Zpos (XO p))
    | XO p -> (match y with
               | XI q -> pred_double (pos_sub p q)
               | XO q -> double (pos_sub p q)
               | XH -> Zpos (Coq_Pos.pred_double p))
    | XH -> (match y with
             | XI q -> Zneg (XO q)
             | XO q -> Zneg (Coq_Pos.pred_double q)
             | XH -> Z0)

  (** val add : z -> z -> z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' -> (match y with
                  | Z0 -> x
                  | Zpos y' -> Zpos (Coq_Pos.add x' y')
                  | Zneg y' -> pos_sub x' y')
    | Zneg x' -> (match y with
                  | Z0 -> x
                  | Zpos y' -> pos_sub y' x'
                  | Zneg y' -> Zneg (Coq_Pos.add x' y'))

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val pred : z -> z **)

  let pred x =
    add x (Zneg XH)

  (** val sub : z -> z -> z **)

  let sub m n0 =
    add m (opp n0)

  (** val mul : z -> z -> z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' -> (match y with
                  | Z0 -> Z0
                  | Zpos y' -> Zpos (Coq_Pos.mul x' y')
                  | Zneg y' -> Zneg (Coq_Pos.mul x' y'))
    | Zneg x' -> (match y with
                  | Z0 -> Z0
                  | Zpos y' -> Zneg (Coq_Pos.mul x' y')
                  | Zneg y' -> Zpos (Coq_Pos.mul x' y'))

  (** val pow_pos : z -> positive -> z **)

  let pow_pos z0 =
    Coq_Pos.iter (mul z0) (Zpos XH)

  (** val pow : z -> z -> z **)

  let pow x = function
  | Z0 -> Zpos XH
  | Zpos p -> pow_pos x p
  | Zneg _ -> Z0

  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Coq_Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' -> (match y with
                  | Zneg y' -> compOpp (Coq_Pos.compare x' y')
                  | _ -> Lt)

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : z -> z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val max : z -> z -> z **)

  let max n0 m =
    match compare n0 m with
    | Lt -> m
    | _ -> n0

  (** val min : z -> z -> z **)

  let min n0 m =
    match compare n0 m with
    | Gt -> m
    | _ -> n0

  (** val to_nat : z -> int **)

  let to_nat = function
  | Zpos p -> Coq_Pos.to_nat p
  | _ -> 0

  (** val of_nat : int -> z **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Z0)
      (fun n1 -> Zpos (Coq_Pos.of_succ_nat n1))
      n0

  (** val of_N : n -> z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val iter : z -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let iter n0 f x =
    match n0 with
    | Zpos p -> Coq_Pos.iter f x p
    | _ -> x

  (** val pos_div_eucl : positive -> z -> z * z **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (XO XH)) r) (Zpos XH) in
      if ltb r' b then ((mul (Zpos (XO XH)) q), r') else ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b))
    | XO a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (XO XH)) r in
      if ltb r' b then ((mul (Zpos (XO XH)) q), r') else ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b))
    | XH -> if leb (Zpos (XO XH)) b then (Z0, (Zpos XH)) else ((Zpos XH), Z0)

  (** val div_eucl : z -> z -> z * z **)

  let div_eucl a b =
    match a with
    | Z0 -> (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> (Z0, Z0)
       | Zpos _ -> pos_div_eucl a' b
       | Zneg b' ->
         let (q, r) = pos_div_eucl a' (Zpos b') in (match r with
                                                    | Z0 -> ((opp q), Z0)
                                                    | _ -> ((opp (add q (Zpos XH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> (Z0, Z0)
       | Zpos _ ->
         let (q, r) = pos_div_eucl a' b in (match r with
                                            | Z0 -> ((opp q), Z0)
                                            | _ -> ((opp (add q (Zpos XH))), (sub b r)))
       | Zneg b' -> let (q, r) = pos_div_eucl a' (Zpos b') in (q, (opp r)))

  (** val div : z -> z -> z **)

  let div a b =
    let (q, _) = div_eucl a b in q

  (** val modulo : z -> z -> z **)

  let modulo a b =
    let (_, r) = div_eucl a b in r

  (** val quotrem : z -> z -> z * z **)

  let quotrem a b =
    match a with
    | Z0 -> (Z0, Z0)
    | Zpos a0 ->
      (match b with
       | Z0 -> (Z0, a)
       | Zpos b0 -> let (q, r) = N.pos_div_eucl a0 (Npos b0) in ((of_N q), (of_N r))
       | Zneg b0 -> let (q, r) = N.pos_div_eucl a0 (Npos b0) in ((opp (of_N q)), (of_N r)))
    | Zneg a0 ->
      (match b with
       | Z0 -> (Z0, a)
       | Zpos b0 -> let (q, r) = N.pos_div_eucl a0 (Npos b0) in ((opp (of_N q)), (opp (of_N r)))
       | Zneg b0 -> let (q, r) = N.pos_div_eucl a0 (Npos b0) in ((of_N q), (opp (of_N r))))

  (** val quot : z -> z -> z **)

  let quot a b =
    fst (quotrem a b)

  (** val rem : z -> z -> z **)

  let rem a b =
    snd (quotrem a b)

  (** val odd : z -> bool **)

  let odd = function
  | Z0 -> false
  | Zpos p -> (match p with
               | XO _ -> false
               | _ -> true)
  | Zneg p -> (match p with
               | XO _ -> false
               | _ -> true)

  (** val div2 : z -> z **)

  let div2 = function
  | Z0 -> Z0
  | Zpos p -> (match p with
               | XH -> Z0
               | _ -> Zpos (Coq_Pos.div2 p))
  | Zneg p -> Zneg (Coq_Pos.div2_up p)

  (** val testbit : z -> z -> bool **)

  let testbit a = function
  | Z0 -> odd a
  | Zpos p ->
    (match a with
     | Z0 -> false
     | Zpos a0 -> Coq_Pos.testbit a0 (Npos p)
     | Zneg a0 -> negb (N.testbit (Coq_Pos.pred_N a0) (Npos p)))
  | Zneg _ -> false

  (** val shiftl : z -> z -> z **)

  let shiftl a = function
  | Z0 -> a
  | Zpos p -> Coq_Pos.iter (mul (Zpos (XO XH))) a p
  | Zneg p -> Coq_Pos.iter div2 a p

  (** val shiftr : z -> z -> z **)

  let shiftr a n0 =
    shiftl a (opp n0)

  (** val coq_lor : z -> z -> z **)

  let coq_lor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zpos (Coq_Pos.coq_lor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.ldiff (Coq_Pos.pred_N b0) (Npos a0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.ldiff (Coq_Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> Zneg (N.succ_pos (N.coq_land (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))

  (** val coq_land : z -> z -> z **)

  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Coq_Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Coq_Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Coq_Pos.pred_N a0))
       | Zneg b0 -> Zneg (N.succ_pos (N.coq_lor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))

  (** val coq_lxor : z -> z -> z **)

  let coq_lxor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> of_N (Coq_Pos.coq_lxor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.coq_lxor (Npos a0) (Coq_Pos.pred_N b0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.coq_lxor (Coq_Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> of_N (N.coq_lxor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0)))

  (** val eq_dec : z -> z -> bool **)

  let eq_dec x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos x0 -> (match y with
                  | Zpos p0 -> Coq_Pos.eq_dec x0 p0
                  | _ -> false)
    | Zneg x0 -> (match y with
                  | Zneg p0 -> Coq_Pos.eq_dec x0 p0
                  | _ -> false)
 end

(** val z_lt_dec : z -> z -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_le_dec : z -> z -> bool **)

let z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

(** val z_le_gt_dec : z -> z -> bool **)

let z_le_gt_dec =
  z_le_dec

(** val zeq_bool : z -> z -> bool **)

let zeq_bool x y =
  match Z.compare x y with
  | Eq -> true
  | _ -> false

(** val zero0 : char **)

let zero0 = '\000'

(** val one0 : char **)

let one0 = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> zero0)
      (fun n' -> match p with
                 | XI p' -> shift true (loop n' p')
                 | XO p' -> shift false (loop n' p')
                 | XH -> one0)
      n0
  in loop (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
       (Pervasives.succ 0))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function
| N0 -> zero0
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : int -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

(** val n_of_digits : bool list -> n **)

let rec n_of_digits = function
| [] -> N0
| b :: l' -> N.add (if b then Npos XH else N0) (N.mul (Npos (XO XH)) (n_of_digits l'))

(** val n_of_ascii : char -> n **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 -> n_of_digits (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: [])))))))))
    a

(** val nat_of_ascii : char -> int **)

let nat_of_ascii a =
  N.to_nat (n_of_ascii a)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

type 'a exception0 =
| Exc of char list
| Ret of 'a

(** val shift_nat : int -> positive -> positive **)

let rec shift_nat n0 z0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> z0)
    (fun n1 -> XO (shift_nat n1 z0))
    n0

(** val shift_pos : positive -> positive -> positive **)

let shift_pos n0 z0 =
  Coq_Pos.iter (fun x -> XO x) z0 n0

(** val two_power_nat : int -> z **)

let two_power_nat n0 =
  Zpos (shift_nat n0 XH)

(** val two_power_pos : positive -> z **)

let two_power_pos x =
  Zpos (shift_pos x XH)

(** val two_p : z -> z **)

let two_p = function
| Z0 -> Zpos XH
| Zpos y -> two_power_pos y
| Zneg _ -> Z0

type ident = char list

type name =
| NAnon
| NNamed of ident

module Coq_Nat = Nat

(** val xor : bool -> bool -> bool **)

let xor b1 b2 =
  if b1 then b2 else if b2 then false else true

(** val ascii_dec_bool : char -> char -> bool **)

let ascii_dec_bool a b =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun b0 b1 b2 b3 b4 b5 b6 b7 ->
      (&&) ((&&) ((&&) (xor a0 b0) (xor a1 b1)) ((&&) (xor a2 b2) (xor a3 b3)))
        ((&&) ((&&) (xor a4 b4) (xor a5 b5)) ((&&) (xor a6 b6) (xor a7 b7))))
      b)
    a

(** val string_eq_bool : char list -> char list -> bool **)

let rec string_eq_bool a1 a2 =
  match a1 with
  | [] -> (match a2 with
           | [] -> true
           | _::_ -> false)
  | b::bs -> (match a2 with
              | [] -> false
              | c::cs -> (&&) (ascii_dec_bool b c) (string_eq_bool bs cs))

type ('term, 'value) bigStepResult =
| Result of 'value
| OutOfTime of 'term
| Error of char list * 'term option

(** val digit2ascii : int -> char **)

let digit2ascii n0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> '0')
    (fun n1 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> '1')
      (fun n2 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> '2')
        (fun n3 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> '3')
          (fun n4 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> '4')
            (fun n5 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> '5')
              (fun n6 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> '6')
                (fun n7 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> '7')
                  (fun n8 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> '8')
                    (fun n9 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> '9')
                      (fun _ ->
                      ascii_of_nat
                        (add
                          (sub n0 (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
                            (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))))))))))
                          (nat_of_ascii 'A')))
                      n9)
                    n8)
                  n7)
                n6)
              n5)
            n4)
          n3)
        n2)
      n1)
    n0

(** val nat2string : int -> int -> char list **)

let rec nat2string modulus0 x =
  if Coq_Nat.ltb x modulus0
  then (digit2ascii x)::[]
  else let m = Coq_Nat.div x modulus0 in append (nat2string modulus0 m) ((digit2ascii (sub x (mul modulus0 m)))::[])

(** val nat2string10 : int -> char list **)

let nat2string10 =
  nat2string (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0))))))))))

(** val peq : positive -> positive -> bool **)

let peq =
  Coq_Pos.eq_dec

(** val zeq : z -> z -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : z -> z -> bool **)

let zlt =
  z_lt_dec

(** val zle : z -> z -> bool **)

let zle =
  z_le_gt_dec

(** val zdivide_dec : z -> z -> bool **)

let zdivide_dec p q =
  zeq (Z.modulo q p) Z0

(** val nat_of_Z : z -> int **)

let nat_of_Z =
  Z.to_nat

(** val align : z -> z -> z **)

let align n0 amount =
  Z.mul (Z.div (Z.sub (Z.add n0 amount) (Zpos XH)) amount) amount

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

(** val list_repeat : int -> 'a1 -> 'a1 list **)

let rec list_repeat n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m -> x :: (list_repeat m x))
    n0



module PTree =
 struct
  type elt = positive

  (** val elt_eq : positive -> positive -> bool **)

  let elt_eq =
    peq

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  (** val empty : 'a1 t **)

  let empty =
    Leaf

  (** val get : positive -> 'a1 t -> 'a1 option **)

  let rec get i = function
  | Leaf -> None
  | Node (l, o, r) -> (match i with
                       | XI ii -> get ii r
                       | XO ii -> get ii l
                       | XH -> o)

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec set i v = function
  | Leaf ->
    (match i with
     | XI ii -> Node (Leaf, None, (set ii v Leaf))
     | XO ii -> Node ((set ii v Leaf), None, Leaf)
     | XH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | XI ii -> Node (l, o, (set ii v r))
     | XO ii -> Node ((set ii v l), o, r)
     | XH -> Node (l, (Some v), r))

  (** val remove : positive -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | XI ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match l with
          | Leaf ->
            (match o with
             | Some _ -> Node (l, o, (remove ii r))
             | None -> (match remove ii r with
                        | Leaf -> Leaf
                        | Node (t0, o0, t1) -> Node (Leaf, None, (Node (t0, o0, t1)))))
          | Node (_, _, _) -> Node (l, o, (remove ii r))))
    | XO ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match o with
          | Some _ -> Node ((remove ii l), o, r)
          | None ->
            (match r with
             | Leaf -> (match remove ii l with
                        | Leaf -> Leaf
                        | Node (t0, o0, t1) -> Node ((Node (t0, o0, t1)), None, Leaf))
             | Node (_, _, _) -> Node ((remove ii l), o, r))))
    | XH ->
      (match m with
       | Leaf -> Leaf
       | Node (l, _, r) ->
         (match l with
          | Leaf -> (match r with
                     | Leaf -> Leaf
                     | Node (_, _, _) -> Node (l, None, r))
          | Node (_, _, _) -> Node (l, None, r)))

  (** val bempty : 'a1 t -> bool **)

  let rec bempty = function
  | Leaf -> true
  | Node (l, o, r) -> (match o with
                       | Some _ -> false
                       | None -> (&&) (bempty l) (bempty r))

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let rec beq beqA m1 m2 =
    match m1 with
    | Leaf -> bempty m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> bempty m1
       | Node (l2, o2, r2) ->
         (&&)
           ((&&)
             (match o1 with
              | Some y1 -> (match o2 with
                            | Some y2 -> beqA y1 y2
                            | None -> false)
              | None -> (match o2 with
                         | Some _ -> false
                         | None -> true)) (beq beqA l1 l2)) (beq beqA r1 r2))

  (** val prev_append : positive -> positive -> positive **)

  let rec prev_append i j =
    match i with
    | XI i' -> prev_append i' (XI j)
    | XO i' -> prev_append i' (XO j)
    | XH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i XH

  (** val xmap : (positive -> 'a1 -> 'a2) -> 'a1 t -> positive -> 'a2 t **)

  let rec xmap f m i =
    match m with
    | Leaf -> Leaf
    | Node (l, o, r) -> Node ((xmap f l (XO i)), (match o with
                                                  | Some x -> Some (f (prev i) x)
                                                  | None -> None), (xmap f r (XI i)))

  (** val map : (positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    xmap f m XH

  (** val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let rec map1 f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> Node ((map1 f l), (option_map f o), (map1 f r))

  (** val coq_Node' : 'a1 t -> 'a1 option -> 'a1 t -> 'a1 t **)

  let coq_Node' l x r =
    match l with
    | Leaf -> (match x with
               | Some _ -> Node (l, x, r)
               | None -> (match r with
                          | Leaf -> Leaf
                          | Node (_, _, _) -> Node (l, x, r)))
    | Node (_, _, _) -> Node (l, x, r)

  (** val filter1 : ('a1 -> bool) -> 'a1 t -> 'a1 t **)

  let rec filter1 pred0 = function
  | Leaf -> Leaf
  | Node (l, o, r) ->
    let o' = match o with
             | Some x -> if pred0 x then o else None
             | None -> None in coq_Node' (filter1 pred0 l) o' (filter1 pred0 r)

  (** val xcombine_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)

  let rec xcombine_l f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> coq_Node' (xcombine_l f l) (f o None) (xcombine_l f r)

  (** val xcombine_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)

  let rec xcombine_r f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> coq_Node' (xcombine_r f l) (f None o) (xcombine_r f r)

  (** val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let rec combine f m1 m2 =
    match m1 with
    | Leaf -> xcombine_r f m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> xcombine_l f m1
       | Node (l2, o2, r2) -> coq_Node' (combine f l1 l2) (f o1 o2) (combine f r1 r2))

  (** val xelements : 'a1 t -> positive -> (positive * 'a1) list -> (positive * 'a1) list **)

  let rec xelements m i k =
    match m with
    | Leaf -> k
    | Node (l, o, r) ->
      (match o with
       | Some x -> xelements l (XO i) (((prev i), x) :: (xelements r (XI i) k))
       | None -> xelements l (XO i) (xelements r (XI i) k))

  (** val elements : 'a1 t -> (positive * 'a1) list **)

  let elements m =
    xelements m XH []

  (** val xkeys : 'a1 t -> positive -> positive list **)

  let xkeys m i =
    Coq__2.map fst (xelements m i [])

  (** val xfold : ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 t -> 'a2 -> 'a2 **)

  let rec xfold f i m v =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x -> let v1 = xfold f (XO i) l v in let v2 = f v1 (prev i) x in xfold f (XI i) r v2
       | None -> let v1 = xfold f (XO i) l v in xfold f (XI i) r v1)

  (** val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m v =
    xfold f XH m v

  (** val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let rec fold1 f m v =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x -> let v1 = fold1 f l v in let v2 = f v1 x in fold1 f r v2
       | None -> let v1 = fold1 f l v in fold1 f r v1)
 end

module PMap =
 struct
  type 'a t = 'a * 'a PTree.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init x =
    (x, PTree.empty)

  (** val get : positive -> 'a1 t -> 'a1 **)

  let get i m =
    match PTree.get i (snd m) with
    | Some x -> x
    | None -> fst m

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.t **)

  let set i x m =
    ((fst m), (PTree.set i x (snd m)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    ((f (fst m)), (PTree.map1 f (snd m)))
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IMap =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PMap.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init =
    PMap.init

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get i m =
    PMap.get (X.index i) m

  (** val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.t **)

  let set i v m =
    PMap.set (X.index i) v m

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    PMap.map
 end

module ZIndexed =
 struct
  type t = z

  (** val index : z -> positive **)

  let index = function
  | Z0 -> XH
  | Zpos p -> XO p
  | Zneg p -> XI p

  (** val eq : z -> z -> bool **)

  let eq =
    zeq
 end

module ZMap = IMap(ZIndexed)

module M = PTree

type errcode =
| MSG of char list
| CTX of positive
| POS of positive

type errmsg = errcode list

type 'a res =
| OK of 'a
| Error0 of errmsg

(** val bind : 'a1 res -> ('a1 -> 'a2 res) -> 'a2 res **)

let bind f g =
  match f with
  | OK x -> g x
  | Error0 msg -> Error0 msg

(** val bind2 : ('a1 * 'a2) res -> ('a1 -> 'a2 -> 'a3 res) -> 'a3 res **)

let bind2 f g =
  match f with
  | OK p -> let (x, y) = p in g x y
  | Error0 msg -> Error0 msg

type comparison0 =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module type WORDSIZE =
 sig
  val wordsize : int
 end

module Make =
 functor (WS:WORDSIZE) ->
 struct
  (** val wordsize : int **)

  let wordsize =
    WS.wordsize

  (** val zwordsize : z **)

  let zwordsize =
    Z.of_nat wordsize

  (** val modulus : z **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : z **)

  let half_modulus =
    Z.div modulus (Zpos (XO XH))

  (** val max_unsigned : z **)

  let max_unsigned =
    Z.sub modulus (Zpos XH)

  (** val max_signed : z **)

  let max_signed =
    Z.sub half_modulus (Zpos XH)

  (** val min_signed : z **)

  let min_signed =
    Z.opp half_modulus

  type int_ = z
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int_ -> z **)

  let intval i =
    i

  type int = int_

  (** val coq_P_mod_two_p : positive -> int -> z **)

  let rec coq_P_mod_two_p p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Z0)
      (fun m ->
      match p with
      | XI q -> Z.succ_double (coq_P_mod_two_p q m)
      | XO q -> Z.double (coq_P_mod_two_p q m)
      | XH -> Zpos XH)
      n0

  (** val coq_Z_mod_modulus : z -> z **)

  let coq_Z_mod_modulus = function
  | Z0 -> Z0
  | Zpos p -> coq_P_mod_two_p p wordsize
  | Zneg p -> let r = coq_P_mod_two_p p wordsize in if zeq r Z0 then Z0 else Z.sub modulus r

  (** val unsigned : int -> z **)

  let unsigned =
    intval

  (** val signed : int -> z **)

  let signed n0 =
    let x = unsigned n0 in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : z -> int **)

  let repr =
    coq_Z_mod_modulus

  (** val zero : int **)

  let zero =
    repr Z0

  (** val one : int **)

  let one =
    repr (Zpos XH)

  (** val mone : int **)

  let mone =
    repr (Zneg XH)

  (** val iwordsize : int **)

  let iwordsize =
    repr zwordsize

  (** val eq_dec : int -> int -> bool **)

  let eq_dec =
    zeq

  (** val eq : int -> int -> bool **)

  let eq x y =
    if zeq (unsigned x) (unsigned y) then true else false

  (** val lt : int -> int -> bool **)

  let lt x y =
    if zlt (signed x) (signed y) then true else false

  (** val ltu : int -> int -> bool **)

  let ltu x y =
    if zlt (unsigned x) (unsigned y) then true else false

  (** val neg : int -> int **)

  let neg x =
    repr (Z.opp (unsigned x))

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val mul : int -> int -> int **)

  let mul x y =
    repr (Z.mul (unsigned x) (unsigned y))

  (** val divs : int -> int -> int **)

  let divs x y =
    repr (Z.quot (signed x) (signed y))

  (** val mods : int -> int -> int **)

  let mods x y =
    repr (Z.rem (signed x) (signed y))

  (** val divu : int -> int -> int **)

  let divu x y =
    repr (Z.div (unsigned x) (unsigned y))

  (** val modu : int -> int -> int **)

  let modu x y =
    repr (Z.modulo (unsigned x) (unsigned y))

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val coq_or : int -> int -> int **)

  let coq_or x y =
    repr (Z.coq_lor (unsigned x) (unsigned y))

  (** val xor : int -> int -> int **)

  let xor x y =
    repr (Z.coq_lxor (unsigned x) (unsigned y))

  (** val not : int -> int **)

  let not x =
    xor x mone

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val shr : int -> int -> int **)

  let shr x y =
    repr (Z.shiftr (signed x) (unsigned y))

  (** val rol : int -> int -> int **)

  let rol x y =
    let n0 = Z.modulo (unsigned y) zwordsize in
    repr (Z.coq_lor (Z.shiftl (unsigned x) n0) (Z.shiftr (unsigned x) (Z.sub zwordsize n0)))

  (** val ror : int -> int -> int **)

  let ror x y =
    let n0 = Z.modulo (unsigned y) zwordsize in
    repr (Z.coq_lor (Z.shiftr (unsigned x) n0) (Z.shiftl (unsigned x) (Z.sub zwordsize n0)))

  (** val rolm : int -> int -> int -> int **)

  let rolm x a m =
    coq_and (rol x a) m

  (** val shrx : int -> int -> int **)

  let shrx x y =
    divs x (shl one y)

  (** val mulhu : int -> int -> int **)

  let mulhu x y =
    repr (Z.div (Z.mul (unsigned x) (unsigned y)) modulus)

  (** val mulhs : int -> int -> int **)

  let mulhs x y =
    repr (Z.div (Z.mul (signed x) (signed y)) modulus)

  (** val negative : int -> int **)

  let negative x =
    if lt x zero then one else zero

  (** val add_carry : int -> int -> int -> int **)

  let add_carry x y cin =
    if zlt (Z.add (Z.add (unsigned x) (unsigned y)) (unsigned cin)) modulus then zero else one

  (** val add_overflow : int -> int -> int -> int **)

  let add_overflow x y cin =
    let s = Z.add (Z.add (signed x) (signed y)) (signed cin) in
    if (&&) ((fun x -> x) (zle min_signed s)) ((fun x -> x) (zle s max_signed)) then zero else one

  (** val sub_borrow : int -> int -> int -> int **)

  let sub_borrow x y bin =
    if zlt (Z.sub (Z.sub (unsigned x) (unsigned y)) (unsigned bin)) Z0 then one else zero

  (** val sub_overflow : int -> int -> int -> int **)

  let sub_overflow x y bin =
    let s = Z.sub (Z.sub (signed x) (signed y)) (signed bin) in
    if (&&) ((fun x -> x) (zle min_signed s)) ((fun x -> x) (zle s max_signed)) then zero else one

  (** val shr_carry : int -> int -> int **)

  let shr_carry x y =
    if (&&) (lt x zero) (negb (eq (coq_and x (sub (shl one y) one)) zero)) then one else zero

  (** val coq_Zshiftin : bool -> z -> z **)

  let coq_Zshiftin b x =
    if b then Z.succ_double x else Z.double x

  (** val coq_Zzero_ext : z -> z -> z **)

  let coq_Zzero_ext n0 x =
    Z.iter n0 (fun rec0 x0 -> coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0))) (fun _ -> Z0) x

  (** val coq_Zsign_ext : z -> z -> z **)

  let coq_Zsign_ext n0 x =
    Z.iter (Z.pred n0) (fun rec0 x0 -> coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0))) (fun x0 -> if Z.odd x0 then Zneg XH else Z0) x

  (** val zero_ext : z -> int -> int **)

  let zero_ext n0 x =
    repr (coq_Zzero_ext n0 (unsigned x))

  (** val sign_ext : z -> int -> int **)

  let sign_ext n0 x =
    repr (coq_Zsign_ext n0 (unsigned x))

  (** val coq_Z_one_bits : int -> z -> z -> z list **)

  let rec coq_Z_one_bits n0 x i =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun m ->
      if Z.odd x then i :: (coq_Z_one_bits m (Z.div2 x) (Z.add i (Zpos XH))) else coq_Z_one_bits m (Z.div2 x) (Z.add i (Zpos XH)))
      n0

  (** val one_bits : int -> int list **)

  let one_bits x =
    map repr (coq_Z_one_bits wordsize (unsigned x) Z0)

  (** val is_power2 : int -> int option **)

  let is_power2 x =
    match coq_Z_one_bits wordsize (unsigned x) Z0 with
    | [] -> None
    | i :: l -> (match l with
                 | [] -> Some (repr i)
                 | _ :: _ -> None)

  (** val cmp : comparison0 -> int -> int -> bool **)

  let cmp c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> lt x y
    | Cle -> negb (lt y x)
    | Cgt -> lt y x
    | Cge -> negb (lt x y)

  (** val cmpu : comparison0 -> int -> int -> bool **)

  let cmpu c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> ltu x y
    | Cle -> negb (ltu y x)
    | Cgt -> ltu y x
    | Cge -> negb (ltu x y)

  (** val notbool : int -> int **)

  let notbool x =
    if eq x zero then one else zero

  (** val testbit : int -> z -> bool **)

  let testbit x i =
    Z.testbit (unsigned x) i

  (** val powerserie : z list -> z **)

  let rec powerserie = function
  | [] -> Z0
  | x :: xs -> Z.add (two_p x) (powerserie xs)

  (** val int_of_one_bits : int list -> int **)

  let rec int_of_one_bits = function
  | [] -> zero
  | a :: b -> add (shl one a) (int_of_one_bits b)

  (** val no_overlap : int -> z -> int -> z -> bool **)

  let no_overlap ofs1 sz1 ofs2 sz2 =
    let x1 = unsigned ofs1 in
    let x2 = unsigned ofs2 in
    (&&) ((&&) ((fun x -> x) (zlt (Z.add x1 sz1) modulus)) ((fun x -> x) (zlt (Z.add x2 sz2) modulus)))
      ((||) ((fun x -> x) (zle (Z.add x1 sz1) x2)) ((fun x -> x) (zle (Z.add x2 sz2) x1)))

  (** val coq_Zsize : z -> z **)

  let coq_Zsize = function
  | Zpos p -> Zpos (Coq_Pos.size p)
  | _ -> Z0

  (** val size : int -> z **)

  let size x =
    coq_Zsize (unsigned x)
 end

module Wordsize_32 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))))))))))))
 end

module Int = Make(Wordsize_32)

module Wordsize_8 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ 0)))))))
 end

module Byte = Make(Wordsize_8)

module Wordsize_64 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module Int64 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Wordsize_64.wordsize

  (** val zwordsize : z **)

  let zwordsize =
    Z.of_nat wordsize

  (** val modulus : z **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : z **)

  let half_modulus =
    Z.div modulus (Zpos (XO XH))

  (** val max_unsigned : z **)

  let max_unsigned =
    Z.sub modulus (Zpos XH)

  (** val max_signed : z **)

  let max_signed =
    Z.sub half_modulus (Zpos XH)

  (** val min_signed : z **)

  let min_signed =
    Z.opp half_modulus

  type int_ = z
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int_ -> z **)

  let intval i =
    i

  type int = int_

  (** val coq_P_mod_two_p : positive -> int -> z **)

  let rec coq_P_mod_two_p p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Z0)
      (fun m ->
      match p with
      | XI q -> Z.succ_double (coq_P_mod_two_p q m)
      | XO q -> Z.double (coq_P_mod_two_p q m)
      | XH -> Zpos XH)
      n0

  (** val coq_Z_mod_modulus : z -> z **)

  let coq_Z_mod_modulus = function
  | Z0 -> Z0
  | Zpos p -> coq_P_mod_two_p p wordsize
  | Zneg p -> let r = coq_P_mod_two_p p wordsize in if zeq r Z0 then Z0 else Z.sub modulus r

  (** val unsigned : int -> z **)

  let unsigned =
    intval

  (** val signed : int -> z **)

  let signed n0 =
    let x = unsigned n0 in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : z -> int **)

  let repr =
    coq_Z_mod_modulus

  (** val zero : int **)

  let zero =
    repr Z0

  (** val mone : int **)

  let mone =
    repr (Zneg XH)

  (** val iwordsize : int **)

  let iwordsize =
    repr zwordsize

  (** val eq_dec : int -> int -> bool **)

  let eq_dec =
    zeq

  (** val eq : int -> int -> bool **)

  let eq x y =
    if zeq (unsigned x) (unsigned y) then true else false

  (** val lt : int -> int -> bool **)

  let lt x y =
    if zlt (signed x) (signed y) then true else false

  (** val ltu : int -> int -> bool **)

  let ltu x y =
    if zlt (unsigned x) (unsigned y) then true else false

  (** val neg : int -> int **)

  let neg x =
    repr (Z.opp (unsigned x))

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val mul : int -> int -> int **)

  let mul x y =
    repr (Z.mul (unsigned x) (unsigned y))

  (** val divs : int -> int -> int **)

  let divs x y =
    repr (Z.quot (signed x) (signed y))

  (** val mods : int -> int -> int **)

  let mods x y =
    repr (Z.rem (signed x) (signed y))

  (** val divu : int -> int -> int **)

  let divu x y =
    repr (Z.div (unsigned x) (unsigned y))

  (** val modu : int -> int -> int **)

  let modu x y =
    repr (Z.modulo (unsigned x) (unsigned y))

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val coq_or : int -> int -> int **)

  let coq_or x y =
    repr (Z.coq_lor (unsigned x) (unsigned y))

  (** val xor : int -> int -> int **)

  let xor x y =
    repr (Z.coq_lxor (unsigned x) (unsigned y))

  (** val not : int -> int **)

  let not x =
    xor x mone

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val shr : int -> int -> int **)

  let shr x y =
    repr (Z.shiftr (signed x) (unsigned y))

  (** val cmp : comparison0 -> int -> int -> bool **)

  let cmp c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> lt x y
    | Cle -> negb (lt y x)
    | Cgt -> lt y x
    | Cge -> negb (lt x y)

  (** val cmpu : comparison0 -> int -> int -> bool **)

  let cmpu c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> ltu x y
    | Cle -> negb (ltu y x)
    | Cgt -> ltu y x
    | Cge -> negb (ltu x y)

  (** val iwordsize' : Int.int **)

  let iwordsize' =
    Int.repr zwordsize

  (** val loword : int -> Int.int **)

  let loword n0 =
    Int.repr (unsigned n0)
 end

(** val zeven : z -> bool **)

let zeven = function
| Z0 -> true
| Zpos p -> (match p with
             | XO _ -> true
             | _ -> false)
| Zneg p -> (match p with
             | XO _ -> true
             | _ -> false)

type radix = z
  (* singleton inductive, whose constructor was Build_radix *)

(** val radix_val : radix -> z **)

let radix_val r =
  r

(** val radix2 : radix **)

let radix2 =
  Zpos (XO XH)

(** val cond_Zopp : bool -> z -> z **)

let cond_Zopp b m =
  if b then Z.opp m else m

(** val zpos_div_eucl_aux1 : positive -> positive -> z * z **)

let rec zpos_div_eucl_aux1 a b = match b with
| XI _ -> Z.pos_div_eucl a (Zpos b)
| XO b' ->
  (match a with
   | XI a' -> let (q, r) = zpos_div_eucl_aux1 a' b' in (q, (Z.add (Z.mul (Zpos (XO XH)) r) (Zpos XH)))
   | XO a' -> let (q, r) = zpos_div_eucl_aux1 a' b' in (q, (Z.mul (Zpos (XO XH)) r))
   | XH -> (Z0, (Zpos a)))
| XH -> ((Zpos a), Z0)

(** val zpos_div_eucl_aux : positive -> positive -> z * z **)

let zpos_div_eucl_aux a b =
  match Coq_Pos.compare a b with
  | Eq -> ((Zpos XH), Z0)
  | Lt -> (Z0, (Zpos a))
  | Gt -> zpos_div_eucl_aux1 a b

(** val zfast_div_eucl : z -> z -> z * z **)

let zfast_div_eucl a b =
  match a with
  | Z0 -> (Z0, Z0)
  | Zpos a' ->
    (match b with
     | Z0 -> (Z0, Z0)
     | Zpos b' -> zpos_div_eucl_aux a' b'
     | Zneg b' ->
       let (q, r) = zpos_div_eucl_aux a' b' in
       (match r with
        | Z0 -> ((Z.opp q), Z0)
        | _ -> ((Z.opp (Z.add q (Zpos XH))), (Z.add b r))))
  | Zneg a' ->
    (match b with
     | Z0 -> (Z0, Z0)
     | Zpos b' ->
       let (q, r) = zpos_div_eucl_aux a' b' in
       (match r with
        | Z0 -> ((Z.opp q), Z0)
        | _ -> ((Z.opp (Z.add q (Zpos XH))), (Z.sub b r)))
     | Zneg b' -> let (q, r) = zpos_div_eucl_aux a' b' in (q, (Z.opp r)))

(** val iter_nat : ('a1 -> 'a1) -> int -> 'a1 -> 'a1 **)

let rec iter_nat f n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> x)
    (fun n' -> iter_nat f n' (f x))
    n0

(** val iter_pos : ('a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

let rec iter_pos f n0 x =
  match n0 with
  | XI n' -> iter_pos f n' (iter_pos f n' (f x))
  | XO n' -> iter_pos f n' (iter_pos f n' x)
  | XH -> f x

(** val fLT_exp : z -> z -> z -> z **)

let fLT_exp emin prec e =
  Z.max (Z.sub e prec) emin

(** val digits2_pos : positive -> positive **)

let rec digits2_pos = function
| XI p -> Coq_Pos.succ (digits2_pos p)
| XO p -> Coq_Pos.succ (digits2_pos p)
| XH -> XH

(** val zdigits2 : z -> z **)

let zdigits2 n0 = match n0 with
| Z0 -> n0
| Zpos p -> Zpos (digits2_pos p)
| Zneg p -> Zpos (digits2_pos p)

type location =
| Loc_Exact
| Loc_Inexact of comparison

(** val new_location_even : z -> z -> location -> location **)

let new_location_even nb_steps k l =
  if zeq_bool k Z0
  then (match l with
        | Loc_Exact -> l
        | Loc_Inexact _ -> Loc_Inexact Lt)
  else Loc_Inexact
         (match Z.compare (Z.mul (Zpos (XO XH)) k) nb_steps with
          | Eq -> (match l with
                   | Loc_Exact -> Eq
                   | Loc_Inexact _ -> Gt)
          | x -> x)

(** val new_location_odd : z -> z -> location -> location **)

let new_location_odd nb_steps k l =
  if zeq_bool k Z0
  then (match l with
        | Loc_Exact -> l
        | Loc_Inexact _ -> Loc_Inexact Lt)
  else Loc_Inexact
         (match Z.compare (Z.add (Z.mul (Zpos (XO XH)) k) (Zpos XH)) nb_steps with
          | Eq -> (match l with
                   | Loc_Exact -> Lt
                   | Loc_Inexact l0 -> l0)
          | x -> x)

(** val new_location : z -> z -> location -> location **)

let new_location nb_steps =
  if zeven nb_steps then new_location_even nb_steps else new_location_odd nb_steps

(** val cond_incr : bool -> z -> z **)

let cond_incr b m =
  if b then Z.add m (Zpos XH) else m

(** val round_sign_DN : bool -> location -> bool **)

let round_sign_DN s = function
| Loc_Exact -> false
| Loc_Inexact _ -> s

(** val round_sign_UP : bool -> location -> bool **)

let round_sign_UP s = function
| Loc_Exact -> false
| Loc_Inexact _ -> negb s

(** val round_N : bool -> location -> bool **)

let round_N p = function
| Loc_Exact -> false
| Loc_Inexact c -> (match c with
                    | Eq -> p
                    | Lt -> false
                    | Gt -> true)

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * z

type nan_pl = positive

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * nan_pl
| B754_finite of bool * positive * z

(** val fF2B : z -> z -> full_float -> binary_float **)

let fF2B _ _ = function
| F754_zero s -> B754_zero s
| F754_infinity s -> B754_infinity s
| F754_nan (b, pl) -> B754_nan (b, pl)
| F754_finite (s, m, e) -> B754_finite (s, m, e)

(** val bopp : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float **)

let bopp _ _ opp_nan = function
| B754_zero sx -> B754_zero (negb sx)
| B754_infinity sx -> B754_infinity (negb sx)
| B754_nan (sx, plx) -> let (sres, plres) = opp_nan sx plx in B754_nan (sres, plres)
| B754_finite (sx, mx, ex) -> B754_finite ((negb sx), mx, ex)

(** val babs : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float **)

let babs _ _ abs_nan = function
| B754_zero _ -> B754_zero false
| B754_infinity _ -> B754_infinity false
| B754_nan (sx, plx) -> let (sres, plres) = abs_nan sx plx in B754_nan (sres, plres)
| B754_finite (_, mx, ex) -> B754_finite (false, mx, ex)

(** val bcompare : z -> z -> binary_float -> binary_float -> comparison option **)

let bcompare _ _ f1 f2 =
  match f1 with
  | B754_zero _ ->
    (match f2 with
     | B754_zero _ -> Some Eq
     | B754_infinity b -> if b then Some Gt else Some Lt
     | B754_nan (_, _) -> None
     | B754_finite (b, _, _) -> if b then Some Gt else Some Lt)
  | B754_infinity b ->
    if b
    then (match f2 with
          | B754_infinity b0 -> if b0 then Some Eq else Some Lt
          | B754_nan (_, _) -> None
          | _ -> Some Lt)
    else (match f2 with
          | B754_infinity b0 -> if b0 then Some Gt else Some Eq
          | B754_nan (_, _) -> None
          | _ -> Some Gt)
  | B754_nan (_, _) -> None
  | B754_finite (s1, m1, e1) ->
    if s1
    then (match f2 with
          | B754_zero _ -> Some Lt
          | B754_infinity b -> if b then Some Gt else Some Lt
          | B754_nan (_, _) -> None
          | B754_finite (s2, m2, e2) ->
            if s1
            then if s2
                 then (match Z.compare e1 e2 with
                       | Eq -> Some (compOpp (Coq_Pos.compare_cont Eq m1 m2))
                       | Lt -> Some Gt
                       | Gt -> Some Lt)
                 else Some Lt
            else if s2 then Some Gt else (match Z.compare e1 e2 with
                                          | Eq -> Some (Coq_Pos.compare_cont Eq m1 m2)
                                          | x -> Some x))
    else (match f2 with
          | B754_zero _ -> Some Gt
          | B754_infinity b -> if b then Some Gt else Some Lt
          | B754_nan (_, _) -> None
          | B754_finite (s2, m2, e2) ->
            if s1
            then if s2
                 then (match Z.compare e1 e2 with
                       | Eq -> Some (compOpp (Coq_Pos.compare_cont Eq m1 m2))
                       | Lt -> Some Gt
                       | Gt -> Some Lt)
                 else Some Lt
            else if s2 then Some Gt else (match Z.compare e1 e2 with
                                          | Eq -> Some (Coq_Pos.compare_cont Eq m1 m2)
                                          | x -> Some x))

type shr_record = { shr_m : z; shr_r : bool; shr_s : bool }

(** val shr_m : shr_record -> z **)

let shr_m x = x.shr_m

(** val shr_1 : shr_record -> shr_record **)

let shr_1 mrs =
  let { shr_m = m; shr_r = r; shr_s = s } = mrs in
  let s0 = (||) r s in
  (match m with
   | Z0 -> { shr_m = Z0; shr_r = false; shr_s = s0 }
   | Zpos p0 ->
     (match p0 with
      | XI p -> { shr_m = (Zpos p); shr_r = true; shr_s = s0 }
      | XO p -> { shr_m = (Zpos p); shr_r = false; shr_s = s0 }
      | XH -> { shr_m = Z0; shr_r = true; shr_s = s0 })
   | Zneg p0 ->
     (match p0 with
      | XI p -> { shr_m = (Zneg p); shr_r = true; shr_s = s0 }
      | XO p -> { shr_m = (Zneg p); shr_r = false; shr_s = s0 }
      | XH -> { shr_m = Z0; shr_r = true; shr_s = s0 }))

(** val loc_of_shr_record : shr_record -> location **)

let loc_of_shr_record mrs =
  let { shr_m = _; shr_r = shr_r0; shr_s = shr_s0 } = mrs in
  if shr_r0 then if shr_s0 then Loc_Inexact Gt else Loc_Inexact Eq else if shr_s0 then Loc_Inexact Lt else Loc_Exact

(** val shr_record_of_loc : z -> location -> shr_record **)

let shr_record_of_loc m = function
| Loc_Exact -> { shr_m = m; shr_r = false; shr_s = false }
| Loc_Inexact c ->
  (match c with
   | Eq -> { shr_m = m; shr_r = true; shr_s = false }
   | Lt -> { shr_m = m; shr_r = false; shr_s = true }
   | Gt -> { shr_m = m; shr_r = true; shr_s = true })

(** val shr0 : shr_record -> z -> z -> shr_record * z **)

let shr0 mrs e n0 = match n0 with
| Zpos p -> ((iter_pos shr_1 p mrs), (Z.add e n0))
| _ -> (mrs, e)

(** val shr_fexp : z -> z -> z -> z -> location -> shr_record * z **)

let shr_fexp prec emax =
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  let fexp = fLT_exp emin prec in (fun m e l -> shr0 (shr_record_of_loc m l) e (Z.sub (fexp (Z.add (zdigits2 m) e)) e))

type mode =
| Mode_NE
| Mode_ZR
| Mode_DN
| Mode_UP
| Mode_NA

(** val choice_mode : mode -> bool -> z -> location -> z **)

let choice_mode m sx mx lx =
  match m with
  | Mode_NE -> cond_incr (round_N (negb (zeven mx)) lx) mx
  | Mode_ZR -> mx
  | Mode_DN -> cond_incr (round_sign_DN sx lx) mx
  | Mode_UP -> cond_incr (round_sign_UP sx lx) mx
  | Mode_NA -> cond_incr (round_N true lx) mx

(** val overflow_to_inf : mode -> bool -> bool **)

let overflow_to_inf m s =
  match m with
  | Mode_ZR -> false
  | Mode_DN -> s
  | Mode_UP -> negb s
  | _ -> true

(** val binary_overflow : z -> z -> mode -> bool -> full_float **)

let binary_overflow prec emax m s =
  if overflow_to_inf m s
  then F754_infinity s
  else F754_finite (s, (match Z.sub (Z.pow (Zpos (XO XH)) prec) (Zpos XH) with
                        | Zpos p -> p
                        | _ -> XH), (Z.sub emax prec))

(** val binary_round_aux : z -> z -> mode -> bool -> positive -> z -> location -> full_float **)

let binary_round_aux prec emax mode1 sx mx ex lx =
  let (mrs', e') = shr_fexp prec emax (Zpos mx) ex lx in
  let (mrs'', e'') = shr_fexp prec emax (choice_mode mode1 sx mrs'.shr_m (loc_of_shr_record mrs')) e' Loc_Exact in
  (match mrs''.shr_m with
   | Z0 -> F754_zero sx
   | Zpos m -> if Z.leb e'' (Z.sub emax prec) then F754_finite (sx, m, e'') else binary_overflow prec emax mode1 sx
   | Zneg _ -> F754_nan (false, XH))

(** val bmult :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float -> binary_float **)

let bmult prec emax mult_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_zero sy -> B754_zero (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
      | _ -> f (mult_nan x y))
   | B754_infinity sx ->
     (match y with
      | B754_infinity sy -> B754_infinity (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
      | _ -> f (mult_nan x y))
   | B754_nan (_, _) -> f (mult_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero sy -> B754_zero (xorb sx sy)
      | B754_infinity sy -> B754_infinity (xorb sx sy)
      | B754_nan (_, _) -> f (mult_nan x y)
      | B754_finite (sy, my, ey) ->
        fF2B prec emax (binary_round_aux prec emax m (xorb sx sy) (Coq_Pos.mul mx my) (Z.add ex ey) Loc_Exact)))

(** val shl_align : positive -> z -> z -> positive * z **)

let shl_align mx ex ex' =
  match Z.sub ex' ex with
  | Zneg d -> ((shift_pos d mx), ex')
  | _ -> (mx, ex)

(** val shl_align_fexp : z -> z -> positive -> z -> positive * z **)

let shl_align_fexp prec emax =
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  let fexp = fLT_exp emin prec in (fun mx ex -> shl_align mx ex (fexp (Z.add (Zpos (digits2_pos mx)) ex)))

(** val binary_round : z -> z -> mode -> bool -> positive -> z -> full_float **)

let binary_round prec emax m sx mx ex =
  let (mz, ez) = shl_align_fexp prec emax mx ex in binary_round_aux prec emax m sx mz ez Loc_Exact

(** val binary_normalize : z -> z -> mode -> z -> z -> bool -> binary_float **)

let binary_normalize prec emax mode1 m e szero =
  match m with
  | Z0 -> B754_zero szero
  | Zpos m0 -> fF2B prec emax (binary_round prec emax mode1 false m0 e)
  | Zneg m0 -> fF2B prec emax (binary_round prec emax mode1 true m0 e)

(** val bplus :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float -> binary_float **)

let bplus prec emax plus_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_zero sy -> if eqb sx sy then x else (match m with
                                                  | Mode_DN -> B754_zero true
                                                  | _ -> B754_zero false)
      | B754_nan (_, _) -> f (plus_nan x y)
      | _ -> y)
   | B754_infinity sx ->
     (match y with
      | B754_infinity sy -> if eqb sx sy then x else f (plus_nan x y)
      | B754_nan (_, _) -> f (plus_nan x y)
      | _ -> x)
   | B754_nan (_, _) -> f (plus_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero _ -> x
      | B754_infinity _ -> y
      | B754_nan (_, _) -> f (plus_nan x y)
      | B754_finite (sy, my, ey) ->
        let ez = Z.min ex ey in
        binary_normalize prec emax m
          (Z.add (cond_Zopp sx (Zpos (fst (shl_align mx ex ez)))) (cond_Zopp sy (Zpos (fst (shl_align my ey ez))))) ez
          (match m with
           | Mode_DN -> true
           | _ -> false)))

(** val bminus :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float -> binary_float **)

let bminus prec emax minus_nan m x y =
  bplus prec emax minus_nan m x (bopp prec emax (fun x0 x1 -> (x0, x1)) y)

(** val fdiv_core_binary : z -> z -> z -> z -> z -> (z * z) * location **)

let fdiv_core_binary prec m1 e1 m2 e2 =
  let d1 = zdigits2 m1 in
  let d2 = zdigits2 m2 in
  let e = Z.sub e1 e2 in
  (match Z.sub (Z.add d2 prec) d1 with
   | Zpos p ->
     let m = Z.shiftl m1 (Zpos p) in
     let e' = Z.add e (Zneg p) in let (q, r) = zfast_div_eucl m m2 in ((q, e'), (new_location m2 r Loc_Exact))
   | _ -> let (q, r) = zfast_div_eucl m1 m2 in ((q, e), (new_location m2 r Loc_Exact)))

(** val bdiv :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float -> binary_float **)

let bdiv prec emax div_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_infinity sy -> B754_zero (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
      | _ -> f (div_nan x y))
   | B754_infinity sx ->
     (match y with
      | B754_zero sy -> B754_infinity (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
      | _ -> f (div_nan x y))
   | B754_nan (_, _) -> f (div_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero sy -> B754_infinity (xorb sx sy)
      | B754_infinity sy -> B754_zero (xorb sx sy)
      | B754_nan (_, _) -> f (div_nan x y)
      | B754_finite (sy, my, ey) ->
        fF2B prec emax
          (let (p, lz) = fdiv_core_binary prec (Zpos mx) ex (Zpos my) ey in
           let (mz, ez) = p in
           (match mz with
            | Zpos mz0 -> binary_round_aux prec emax m (xorb sx sy) mz0 ez lz
            | _ -> F754_nan (false, XH)))))

(** val join_bits : z -> z -> bool -> z -> z -> z **)

let join_bits mw ew s m e =
  Z.add (Z.shiftl (Z.add (if s then Z.pow (Zpos (XO XH)) ew else Z0) e) mw) m

(** val split_bits : z -> z -> z -> (bool * z) * z **)

let split_bits mw ew x =
  let mm = Z.pow (Zpos (XO XH)) mw in
  let em = Z.pow (Zpos (XO XH)) ew in (((Z.leb (Z.mul mm em) x), (Z.modulo x mm)), (Z.modulo (Z.div x mm) em))

(** val bits_of_binary_float : z -> z -> binary_float -> z **)

let bits_of_binary_float mw ew =
  let emax = Z.pow (Zpos (XO XH)) (Z.sub ew (Zpos XH)) in
  let prec = Z.add mw (Zpos XH) in
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  (fun x ->
  match x with
  | B754_zero sx -> join_bits mw ew sx Z0 Z0
  | B754_infinity sx -> join_bits mw ew sx Z0 (Z.sub (Z.pow (Zpos (XO XH)) ew) (Zpos XH))
  | B754_nan (sx, n0) -> join_bits mw ew sx (Zpos n0) (Z.sub (Z.pow (Zpos (XO XH)) ew) (Zpos XH))
  | B754_finite (sx, mx, ex) ->
    let m = Z.sub (Zpos mx) (Z.pow (Zpos (XO XH)) mw) in
    if Z.leb Z0 m then join_bits mw ew sx m (Z.add (Z.sub ex emin) (Zpos XH)) else join_bits mw ew sx (Zpos mx) Z0)

(** val binary_float_of_bits_aux : z -> z -> z -> full_float **)

let binary_float_of_bits_aux mw ew =
  let emax = Z.pow (Zpos (XO XH)) (Z.sub ew (Zpos XH)) in
  let prec = Z.add mw (Zpos XH) in
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  (fun x ->
  let (p, ex) = split_bits mw ew x in
  let (sx, mx) = p in
  if zeq_bool ex Z0
  then (match mx with
        | Z0 -> F754_zero sx
        | Zpos px -> F754_finite (sx, px, emin)
        | Zneg _ -> F754_nan (false, XH))
  else if zeq_bool ex (Z.sub (Z.pow (Zpos (XO XH)) ew) (Zpos XH))
       then (match mx with
             | Z0 -> F754_infinity sx
             | Zpos plx -> F754_nan (sx, plx)
             | Zneg _ -> F754_nan (false, XH))
       else (match Z.add mx (Z.pow (Zpos (XO XH)) mw) with
             | Zpos px -> F754_finite (sx, px, (Z.sub (Z.add ex emin) (Zpos XH)))
             | _ -> F754_nan (false, XH)))

(** val binary_float_of_bits : z -> z -> z -> binary_float **)

let binary_float_of_bits mw ew x =
  let emax = Z.pow (Zpos (XO XH)) (Z.sub ew (Zpos XH)) in
  let prec = Z.add mw (Zpos XH) in fF2B prec emax (binary_float_of_bits_aux mw ew x)

type binary32 = binary_float

(** val b32_of_bits : z -> binary32 **)

let b32_of_bits =
  binary_float_of_bits (Zpos (XI (XI (XI (XO XH))))) (Zpos (XO (XO (XO XH))))

(** val bits_of_b32 : binary32 -> z **)

let bits_of_b32 =
  bits_of_binary_float (Zpos (XI (XI (XI (XO XH))))) (Zpos (XO (XO (XO XH))))

type binary64 = binary_float

(** val b64_of_bits : z -> binary64 **)

let b64_of_bits =
  binary_float_of_bits (Zpos (XO (XO (XI (XO (XI XH)))))) (Zpos (XI (XI (XO XH))))

(** val bits_of_b64 : binary64 -> z **)

let bits_of_b64 =
  bits_of_binary_float (Zpos (XO (XO (XI (XO (XI XH)))))) (Zpos (XI (XI (XO XH))))

(** val beq_dec : z -> z -> binary_float -> binary_float -> bool **)

let beq_dec _ _ f1 f2 =
  match f1 with
  | B754_zero b ->
    (match f2 with
     | B754_zero b0 -> if b then if b0 then true else false else if b0 then false else true
     | _ -> false)
  | B754_infinity b ->
    (match f2 with
     | B754_infinity b0 -> if b then if b0 then true else false else if b0 then false else true
     | _ -> false)
  | B754_nan (b, n0) ->
    (match f2 with
     | B754_nan (b0, n1) -> if b then if b0 then Coq_Pos.eq_dec n0 n1 else false else if b0 then false else Coq_Pos.eq_dec n0 n1
     | _ -> false)
  | B754_finite (b, m, e) ->
    (match f2 with
     | B754_finite (b0, m0, e1) ->
       if b
       then if b0 then let s = Coq_Pos.eq_dec m m0 in if s then Z.eq_dec e e1 else false else false
       else if b0 then false else let s = Coq_Pos.eq_dec m m0 in if s then Z.eq_dec e e1 else false
     | _ -> false)

(** val bofZ : z -> z -> z -> binary_float **)

let bofZ prec emax n0 =
  binary_normalize prec emax Mode_NE n0 Z0 false

(** val zofB : z -> z -> binary_float -> z option **)

let zofB _ _ = function
| B754_zero _ -> Some Z0
| B754_finite (s, m, e0) ->
  (match e0 with
   | Z0 -> Some (cond_Zopp s (Zpos m))
   | Zpos e -> Some (Z.mul (cond_Zopp s (Zpos m)) (Z.pow_pos (radix_val radix2) e))
   | Zneg e -> Some (cond_Zopp s (Z.div (Zpos m) (Z.pow_pos (radix_val radix2) e))))
| _ -> None

(** val zofB_range : z -> z -> binary_float -> z -> z -> z option **)

let zofB_range prec emax f zmin zmax =
  match zofB prec emax f with
  | Some z0 -> if (&&) (Z.leb zmin z0) (Z.leb z0 zmax) then Some z0 else None
  | None -> None

(** val bconv : z -> z -> z -> z -> (bool -> nan_pl -> bool * nan_pl) -> mode -> binary_float -> binary_float **)

let bconv _ _ prec2 emax2 conv_nan md = function
| B754_nan (s, pl) -> let (s0, pl0) = conv_nan s pl in B754_nan (s0, pl0)
| B754_finite (s, m, e) -> binary_normalize prec2 emax2 md (cond_Zopp s (Zpos m)) e s
| x -> x

(** val big_endian : bool **)

let big_endian =
  false

(** val default_pl_64 : bool * nan_pl **)

let default_pl_64 =
  (true,
    (let rec f n0 =
       (fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> XH)
         (fun n1 -> XO (f n1))
         n0
     in f (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val choose_binop_pl_64 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_64 _ _ _ _ =
  false

(** val default_pl_32 : bool * nan_pl **)

let default_pl_32 =
  (true,
    (let rec f n0 =
       (fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> XH)
         (fun n1 -> XO (f n1))
         n0
     in f (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
          (Pervasives.succ 0))))))))))))))))))))))))

(** val choose_binop_pl_32 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_32 _ _ _ _ =
  false

(** val float_of_single_preserves_sNaN : bool **)

let float_of_single_preserves_sNaN =
  false

type float = binary64

type float32 = binary32

(** val cmp_of_comparison : comparison0 -> comparison option -> bool **)

let cmp_of_comparison c x =
  match c with
  | Ceq -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> true
                          | _ -> false)
            | None -> false)
  | Cne -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> false
                          | _ -> true)
            | None -> true)
  | Clt -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> true
                          | _ -> false)
            | None -> false)
  | Cle -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> false
                          | _ -> true)
            | None -> false)
  | Cgt -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> true
                          | _ -> false)
            | None -> false)
  | Cge -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> false
                          | _ -> true)
            | None -> false)

module Float =
 struct
  (** val transform_quiet_pl : nan_pl -> nan_pl **)

  let transform_quiet_pl pl =
    Coq_Pos.coq_lor pl
      (iter_nat (fun x -> XO x) (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))
        XH)

  (** val expand_pl : nan_pl -> nan_pl **)

  let expand_pl pl =
    Coq_Pos.shiftl_nat pl (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))))))))))

  (** val of_single_pl : bool -> nan_pl -> bool * nan_pl **)

  let of_single_pl s pl =
    (s, (if float_of_single_preserves_sNaN then expand_pl pl else transform_quiet_pl (expand_pl pl)))

  (** val reduce_pl : nan_pl -> nan_pl **)

  let reduce_pl pl =
    Coq_Pos.shiftr_nat pl (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
      (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))))))))))

  (** val to_single_pl : bool -> nan_pl -> bool * nan_pl **)

  let to_single_pl s pl =
    (s, (reduce_pl (transform_quiet_pl pl)))

  (** val neg_pl : bool -> nan_pl -> bool * nan_pl **)

  let neg_pl s pl =
    ((negb s), pl)

  (** val abs_pl : bool -> nan_pl -> bool * nan_pl **)

  let abs_pl _ pl =
    (false, pl)

  (** val binop_pl : binary64 -> binary64 -> bool * nan_pl **)

  let binop_pl x y =
    match x with
    | B754_nan (s1, pl1) ->
      (match y with
       | B754_nan (s2, pl2) ->
         if choose_binop_pl_64 s1 pl1 s2 pl2 then (s2, (transform_quiet_pl pl2)) else (s1, (transform_quiet_pl pl1))
       | _ -> (s1, (transform_quiet_pl pl1)))
    | _ -> (match y with
            | B754_nan (s2, pl2) -> (s2, (transform_quiet_pl pl2))
            | _ -> default_pl_64)

  (** val zero : float **)

  let zero =
    B754_zero false

  (** val eq_dec : float -> float -> bool **)

  let eq_dec =
    beq_dec (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))

  (** val neg : float -> float **)

  let neg =
    bopp (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) neg_pl

  (** val abs : float -> float **)

  let abs =
    babs (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) abs_pl

  (** val add : float -> float -> float **)

  let add =
    bplus (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) binop_pl Mode_NE

  (** val sub : float -> float -> float **)

  let sub =
    bminus (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) binop_pl Mode_NE

  (** val mul : float -> float -> float **)

  let mul =
    bmult (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) binop_pl Mode_NE

  (** val div : float -> float -> float **)

  let div =
    bdiv (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) binop_pl Mode_NE

  (** val cmp : comparison0 -> float -> float -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c
      (bcompare (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f1 f2)

  (** val of_single : float32 -> float **)

  let of_single =
    bconv (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO
      (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) of_single_pl Mode_NE

  (** val to_single : float -> float32 **)

  let to_single =
    bconv (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) (Zpos (XO (XO (XO (XI
      XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) to_single_pl Mode_NE

  (** val to_int : float -> Int.int option **)

  let to_int f =
    option_map Int.repr
      (zofB_range (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f Int.min_signed
        Int.max_signed)

  (** val to_intu : float -> Int.int option **)

  let to_intu f =
    option_map Int.repr
      (zofB_range (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f Z0
        Int.max_unsigned)

  (** val to_long : float -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (zofB_range (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f
        Int64.min_signed Int64.max_signed)

  (** val to_longu : float -> Int64.int option **)

  let to_longu f =
    option_map Int64.repr
      (zofB_range (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f Z0
        Int64.max_unsigned)

  (** val of_int : Int.int -> float **)

  let of_int n0 =
    bofZ (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) (Int.signed n0)

  (** val of_intu : Int.int -> float **)

  let of_intu n0 =
    bofZ (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) (Int.unsigned n0)

  (** val of_long : Int64.int -> float **)

  let of_long n0 =
    bofZ (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) (Int64.signed n0)

  (** val of_longu : Int64.int -> float **)

  let of_longu n0 =
    bofZ (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) (Int64.unsigned n0)

  (** val to_bits : float -> Int64.int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : Int64.int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)
 end

module Float32 =
 struct
  (** val transform_quiet_pl : nan_pl -> nan_pl **)

  let transform_quiet_pl pl =
    Coq_Pos.coq_lor pl
      (iter_nat (fun x -> XO x) (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
        (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))))))))))))))))))))) XH)

  (** val neg_pl : bool -> nan_pl -> bool * nan_pl **)

  let neg_pl s pl =
    ((negb s), pl)

  (** val binop_pl : binary32 -> binary32 -> bool * nan_pl **)

  let binop_pl x y =
    match x with
    | B754_nan (s1, pl1) ->
      (match y with
       | B754_nan (s2, pl2) ->
         if choose_binop_pl_32 s1 pl1 s2 pl2 then (s2, (transform_quiet_pl pl2)) else (s1, (transform_quiet_pl pl1))
       | _ -> (s1, (transform_quiet_pl pl1)))
    | _ -> (match y with
            | B754_nan (s2, pl2) -> (s2, (transform_quiet_pl pl2))
            | _ -> default_pl_32)

  (** val zero : float32 **)

  let zero =
    B754_zero false

  (** val eq_dec : float32 -> float32 -> bool **)

  let eq_dec =
    beq_dec (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH))))))))

  (** val neg : float32 -> float32 **)

  let neg =
    bopp (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) neg_pl

  (** val add : float32 -> float32 -> float32 **)

  let add =
    bplus (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) binop_pl Mode_NE

  (** val sub : float32 -> float32 -> float32 **)

  let sub =
    bminus (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) binop_pl Mode_NE

  (** val mul : float32 -> float32 -> float32 **)

  let mul =
    bmult (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) binop_pl Mode_NE

  (** val div : float32 -> float32 -> float32 **)

  let div =
    bdiv (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) binop_pl Mode_NE

  (** val cmp : comparison0 -> float32 -> float32 -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (bcompare (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) f1 f2)

  (** val to_int : float32 -> Int.int option **)

  let to_int f =
    option_map Int.repr
      (zofB_range (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) f Int.min_signed Int.max_signed)

  (** val to_intu : float32 -> Int.int option **)

  let to_intu f =
    option_map Int.repr
      (zofB_range (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) f Z0 Int.max_unsigned)

  (** val to_long : float32 -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (zofB_range (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) f Int64.min_signed Int64.max_signed)

  (** val to_longu : float32 -> Int64.int option **)

  let to_longu f =
    option_map Int64.repr
      (zofB_range (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) f Z0 Int64.max_unsigned)

  (** val of_int : Int.int -> float32 **)

  let of_int n0 =
    bofZ (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Int.signed n0)

  (** val of_intu : Int.int -> float32 **)

  let of_intu n0 =
    bofZ (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Int.unsigned n0)

  (** val of_long : Int64.int -> float32 **)

  let of_long n0 =
    bofZ (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Int64.signed n0)

  (** val of_longu : Int64.int -> float32 **)

  let of_longu n0 =
    bofZ (Zpos (XO (XO (XO (XI XH))))) (Zpos (XO (XO (XO (XO (XO (XO (XO XH)))))))) (Int64.unsigned n0)

  (** val to_bits : float32 -> Int.int **)

  let to_bits f =
    Int.repr (bits_of_b32 f)

  (** val of_bits : Int.int -> float32 **)

  let of_bits b =
    b32_of_bits (Int.unsigned b)
 end

type ident0 = positive

(** val ident_eq : positive -> positive -> bool **)

let ident_eq =
  peq

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

type calling_convention = { cc_vararg : bool; cc_unproto : bool; cc_structret : bool }

type signature = { sig_args : typ list; sig_res : typ option; sig_cc : calling_convention }

type memory_chunk =
| Mint8signed
| Mint8unsigned
| Mint16signed
| Mint16unsigned
| Mint32
| Mint64
| Mfloat32
| Mfloat64
| Many32
| Many64

type init_data =
| Init_int8 of Int.int
| Init_int16 of Int.int
| Init_int32 of Int.int
| Init_int64 of Int64.int
| Init_float32 of float32
| Init_float64 of float
| Init_space of z
| Init_addrof of ident0 * Int.int

(** val init_data_size : init_data -> z **)

let init_data_size = function
| Init_int8 _ -> Zpos XH
| Init_int16 _ -> Zpos (XO XH)
| Init_int64 _ -> Zpos (XO (XO (XO XH)))
| Init_float64 _ -> Zpos (XO (XO (XO XH)))
| Init_space n0 -> Z.max n0 Z0
| _ -> Zpos (XO (XO XH))

(** val init_data_list_size : init_data list -> z **)

let rec init_data_list_size = function
| [] -> Z0
| i :: il' -> Z.add (init_data_size i) (init_data_list_size il')

type 'v globvar = { gvar_info : 'v; gvar_init : init_data list; gvar_readonly : bool; gvar_volatile : bool }

(** val gvar_init : 'a1 globvar -> init_data list **)

let gvar_init x = x.gvar_init

(** val gvar_readonly : 'a1 globvar -> bool **)

let gvar_readonly x = x.gvar_readonly

(** val gvar_volatile : 'a1 globvar -> bool **)

let gvar_volatile x = x.gvar_volatile

type ('f, 'v) globdef =
| Gfun of 'f
| Gvar of 'v globvar

type ('f, 'v) program = { prog_defs : (ident0 * ('f, 'v) globdef) list; prog_public : ident0 list; prog_main : ident0 }

(** val prog_defs : ('a1, 'a2) program -> (ident0 * ('a1, 'a2) globdef) list **)

let prog_defs x = x.prog_defs

(** val prog_public : ('a1, 'a2) program -> ident0 list **)

let prog_public x = x.prog_public

type external_function =
| EF_external of char list * signature
| EF_builtin of char list * signature
| EF_runtime of char list * signature
| EF_vload of memory_chunk
| EF_vstore of memory_chunk
| EF_malloc
| EF_free
| EF_memcpy of z * z
| EF_annot of char list * typ list
| EF_annot_val of char list * typ
| EF_inline_asm of char list * signature * char list list
| EF_debug of positive * ident0 * typ list

type block = positive

(** val eq_block : positive -> positive -> bool **)

let eq_block =
  peq

type val0 =
| Vundef
| Vint of Int.int
| Vlong of Int64.int
| Vfloat of float
| Vsingle of float32
| Vptr of block * Int.int

(** val vzero : val0 **)

let vzero =
  Vint Int.zero

(** val vone : val0 **)

let vone =
  Vint Int.one

(** val vtrue : val0 **)

let vtrue =
  Vint Int.one

(** val vfalse : val0 **)

let vfalse =
  Vint Int.zero

module Val =
 struct
  (** val eq : val0 -> val0 -> bool **)

  let eq x y =
    match x with
    | Vundef -> (match y with
                 | Vundef -> true
                 | _ -> false)
    | Vint x0 -> (match y with
                  | Vint i0 -> Int.eq_dec x0 i0
                  | _ -> false)
    | Vlong x0 -> (match y with
                   | Vlong i0 -> Int64.eq_dec x0 i0
                   | _ -> false)
    | Vfloat x0 -> (match y with
                    | Vfloat f0 -> Float.eq_dec x0 f0
                    | _ -> false)
    | Vsingle x0 -> (match y with
                     | Vsingle f0 -> Float32.eq_dec x0 f0
                     | _ -> false)
    | Vptr (x0, x1) -> (match y with
                        | Vptr (b0, i0) -> if eq_block x0 b0 then Int.eq_dec x1 i0 else false
                        | _ -> false)

  (** val of_bool : bool -> val0 **)

  let of_bool = function
  | true -> vtrue
  | false -> vfalse

  (** val cmp_different_blocks : comparison0 -> bool option **)

  let cmp_different_blocks = function
  | Ceq -> Some false
  | Cne -> Some true
  | _ -> None

  (** val cmpu_bool : (block -> z -> bool) -> comparison0 -> val0 -> val0 -> bool option **)

  let cmpu_bool valid_ptr =
    let weak_valid_ptr = fun b ofs -> (||) (valid_ptr b ofs) (valid_ptr b (Z.sub ofs (Zpos XH))) in
    (fun c v1 v2 ->
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Some (Int.cmpu c n1 n2)
       | Vptr (b2, ofs2) ->
         if (&&) (Int.eq n1 Int.zero) (weak_valid_ptr b2 (Int.unsigned ofs2)) then cmp_different_blocks c else None
       | _ -> None)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vint n2 -> if (&&) (Int.eq n2 Int.zero) (weak_valid_ptr b1 (Int.unsigned ofs1)) then cmp_different_blocks c else None
       | Vptr (b2, ofs2) ->
         if eq_block b1 b2
         then if (&&) (weak_valid_ptr b1 (Int.unsigned ofs1)) (weak_valid_ptr b2 (Int.unsigned ofs2))
              then Some (Int.cmpu c ofs1 ofs2)
              else None
         else if (&&) (valid_ptr b1 (Int.unsigned ofs1)) (valid_ptr b2 (Int.unsigned ofs2)) then cmp_different_blocks c else None
       | _ -> None)
    | _ -> None)

  (** val load_result : memory_chunk -> val0 -> val0 **)

  let load_result chunk v =
    match chunk with
    | Mint8signed -> (match v with
                      | Vint n0 -> Vint (Int.sign_ext (Zpos (XO (XO (XO XH)))) n0)
                      | _ -> Vundef)
    | Mint8unsigned -> (match v with
                        | Vint n0 -> Vint (Int.zero_ext (Zpos (XO (XO (XO XH)))) n0)
                        | _ -> Vundef)
    | Mint16signed -> (match v with
                       | Vint n0 -> Vint (Int.sign_ext (Zpos (XO (XO (XO (XO XH))))) n0)
                       | _ -> Vundef)
    | Mint16unsigned -> (match v with
                         | Vint n0 -> Vint (Int.zero_ext (Zpos (XO (XO (XO (XO XH))))) n0)
                         | _ -> Vundef)
    | Mint32 -> (match v with
                 | Vint n0 -> Vint n0
                 | Vptr (b, ofs) -> Vptr (b, ofs)
                 | _ -> Vundef)
    | Mint64 -> (match v with
                 | Vlong n0 -> Vlong n0
                 | _ -> Vundef)
    | Mfloat32 -> (match v with
                   | Vsingle f -> Vsingle f
                   | _ -> Vundef)
    | Mfloat64 -> (match v with
                   | Vfloat f -> Vfloat f
                   | _ -> Vundef)
    | Many32 -> (match v with
                 | Vundef -> Vundef
                 | Vlong _ -> Vundef
                 | Vfloat _ -> Vundef
                 | _ -> v)
    | Many64 -> v
 end

(** val size_chunk : memory_chunk -> z **)

let size_chunk = function
| Mint8signed -> Zpos XH
| Mint8unsigned -> Zpos XH
| Mint16signed -> Zpos (XO XH)
| Mint16unsigned -> Zpos (XO XH)
| Mint32 -> Zpos (XO (XO XH))
| Mfloat32 -> Zpos (XO (XO XH))
| Many32 -> Zpos (XO (XO XH))
| _ -> Zpos (XO (XO (XO XH)))

(** val size_chunk_nat : memory_chunk -> int **)

let size_chunk_nat chunk =
  nat_of_Z (size_chunk chunk)

(** val align_chunk : memory_chunk -> z **)

let align_chunk = function
| Mint8signed -> Zpos XH
| Mint8unsigned -> Zpos XH
| Mint16signed -> Zpos (XO XH)
| Mint16unsigned -> Zpos (XO XH)
| Mint64 -> Zpos (XO (XO (XO XH)))
| _ -> Zpos (XO (XO XH))

type quantity =
| Q32
| Q64

(** val quantity_eq : quantity -> quantity -> bool **)

let quantity_eq q1 q2 =
  match q1 with
  | Q32 -> (match q2 with
            | Q32 -> true
            | Q64 -> false)
  | Q64 -> (match q2 with
            | Q32 -> false
            | Q64 -> true)

(** val size_quantity_nat : quantity -> int **)

let size_quantity_nat = function
| Q32 -> Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))
| Q64 ->
  Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ 0)))))))

type memval =
| Undef
| Byte of Byte.int
| Fragment of val0 * quantity * int

(** val bytes_of_int : int -> z -> Byte.int list **)

let rec bytes_of_int n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m -> (Byte.repr x) :: (bytes_of_int m (Z.div x (Zpos (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))))
    n0

(** val int_of_bytes : Byte.int list -> z **)

let rec int_of_bytes = function
| [] -> Z0
| b :: l' -> Z.add (Byte.unsigned b) (Z.mul (int_of_bytes l') (Zpos (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))

(** val rev_if_be : Byte.int list -> Byte.int list **)

let rev_if_be l =
  if big_endian then rev l else l

(** val encode_int : int -> z -> Byte.int list **)

let encode_int sz x =
  rev_if_be (bytes_of_int sz x)

(** val decode_int : Byte.int list -> z **)

let decode_int b =
  int_of_bytes (rev_if_be b)

(** val inj_bytes : Byte.int list -> memval list **)

let inj_bytes bl =
  map (fun x -> Byte x) bl

(** val proj_bytes : memval list -> Byte.int list option **)

let rec proj_bytes = function
| [] -> Some []
| m :: vl' -> (match m with
               | Byte b -> (match proj_bytes vl' with
                            | Some bl -> Some (b :: bl)
                            | None -> None)
               | _ -> None)

(** val inj_value_rec : int -> val0 -> quantity -> memval list **)

let rec inj_value_rec n0 v q =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m -> (Fragment (v, q, m)) :: (inj_value_rec m v q))
    n0

(** val inj_value : quantity -> val0 -> memval list **)

let inj_value q v =
  inj_value_rec (size_quantity_nat q) v q

(** val check_value : int -> val0 -> quantity -> memval list -> bool **)

let rec check_value n0 v q vl =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match vl with
              | [] -> true
              | _ :: _ -> false)
    (fun m ->
    match vl with
    | [] -> false
    | m0 :: vl' ->
      (match m0 with
       | Fragment (v', q', m') ->
         (&&) ((&&) ((&&) ((fun x -> x) (Val.eq v v')) ((fun x -> x) (quantity_eq q q'))) ((=) m m')) (check_value m v q vl')
       | _ -> false))
    n0

(** val proj_value : quantity -> memval list -> val0 **)

let proj_value q vl = match vl with
| [] -> Vundef
| m :: _ -> (match m with
             | Fragment (v, _, _) -> if check_value (size_quantity_nat q) v q vl then v else Vundef
             | _ -> Vundef)

(** val encode_val : memory_chunk -> val0 -> memval list **)

let encode_val chunk v = match v with
| Vundef ->
  (match chunk with
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)
| Vint n0 ->
  (match chunk with
   | Mint8signed -> inj_bytes (encode_int (Pervasives.succ 0) (Int.unsigned n0))
   | Mint8unsigned -> inj_bytes (encode_int (Pervasives.succ 0) (Int.unsigned n0))
   | Mint16signed -> inj_bytes (encode_int (Pervasives.succ (Pervasives.succ 0)) (Int.unsigned n0))
   | Mint16unsigned -> inj_bytes (encode_int (Pervasives.succ (Pervasives.succ 0)) (Int.unsigned n0))
   | Mint32 -> inj_bytes (encode_int (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))) (Int.unsigned n0))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)
| Vlong n0 ->
  (match chunk with
   | Mint64 ->
     inj_bytes
       (encode_int (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
         (Pervasives.succ (Pervasives.succ 0)))))))) (Int64.unsigned n0))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)
| Vfloat n0 ->
  (match chunk with
   | Mfloat64 ->
     inj_bytes
       (encode_int (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
         (Pervasives.succ (Pervasives.succ 0)))))))) (Int64.unsigned (Float.to_bits n0)))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)
| Vsingle n0 ->
  (match chunk with
   | Mfloat32 ->
     inj_bytes
       (encode_int (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))) (Int.unsigned (Float32.to_bits n0)))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)
| Vptr (_, _) ->
  (match chunk with
   | Mint32 -> inj_value Q32 v
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> list_repeat (size_chunk_nat chunk) Undef)

(** val decode_val : memory_chunk -> memval list -> val0 **)

let decode_val chunk vl =
  match proj_bytes vl with
  | Some bl ->
    (match chunk with
     | Mint8signed -> Vint (Int.sign_ext (Zpos (XO (XO (XO XH)))) (Int.repr (decode_int bl)))
     | Mint8unsigned -> Vint (Int.zero_ext (Zpos (XO (XO (XO XH)))) (Int.repr (decode_int bl)))
     | Mint16signed -> Vint (Int.sign_ext (Zpos (XO (XO (XO (XO XH))))) (Int.repr (decode_int bl)))
     | Mint16unsigned -> Vint (Int.zero_ext (Zpos (XO (XO (XO (XO XH))))) (Int.repr (decode_int bl)))
     | Mint32 -> Vint (Int.repr (decode_int bl))
     | Mint64 -> Vlong (Int64.repr (decode_int bl))
     | Mfloat32 -> Vsingle (Float32.of_bits (Int.repr (decode_int bl)))
     | Mfloat64 -> Vfloat (Float.of_bits (Int64.repr (decode_int bl)))
     | _ -> Vundef)
  | None ->
    (match chunk with
     | Mint32 -> Val.load_result chunk (proj_value Q32 vl)
     | Many32 -> Val.load_result chunk (proj_value Q32 vl)
     | Many64 -> Val.load_result chunk (proj_value Q64 vl)
     | _ -> Vundef)

type permission =
| Freeable
| Writable
| Readable
| Nonempty

type perm_kind =
| Max
| Cur

module Mem =
 struct
  type mem' = { mem_contents : memval ZMap.t PMap.t; mem_access : (z -> perm_kind -> permission option) PMap.t; nextblock : block }

  (** val mem_contents : mem' -> memval ZMap.t PMap.t **)

  let mem_contents x = x.mem_contents

  (** val mem_access : mem' -> (z -> perm_kind -> permission option) PMap.t **)

  let mem_access x = x.mem_access

  (** val nextblock : mem' -> block **)

  let nextblock x = x.nextblock

  type mem = mem'

  (** val perm_order_dec : permission -> permission -> bool **)

  let perm_order_dec p1 p2 =
    match p1 with
    | Freeable -> true
    | Writable -> (match p2 with
                   | Freeable -> false
                   | _ -> true)
    | Readable -> (match p2 with
                   | Freeable -> false
                   | Writable -> false
                   | _ -> true)
    | Nonempty -> (match p2 with
                   | Nonempty -> true
                   | _ -> false)

  (** val perm_order'_dec : permission option -> permission -> bool **)

  let perm_order'_dec op p =
    match op with
    | Some p0 -> perm_order_dec p0 p
    | None -> false

  (** val perm_dec : mem -> block -> z -> perm_kind -> permission -> bool **)

  let perm_dec m b ofs k p =
    perm_order'_dec (PMap.get b m.mem_access ofs k) p

  (** val range_perm_dec : mem -> block -> z -> z -> perm_kind -> permission -> bool **)

  let rec range_perm_dec m b lo hi k p =
    let s = zlt lo hi in
    if s
    then let s0 = perm_dec m b lo k p in if s0 then let y = Z.add lo (Zpos XH) in range_perm_dec m b y hi k p else false
    else true

  (** val valid_access_dec : mem -> memory_chunk -> block -> z -> permission -> bool **)

  let valid_access_dec m chunk b ofs p =
    let s = range_perm_dec m b ofs (Z.add ofs (size_chunk chunk)) Cur p in
    if s then zdivide_dec (align_chunk chunk) ofs else false

  (** val valid_pointer : mem -> block -> z -> bool **)

  let valid_pointer m b ofs =
    (fun x -> x) (perm_dec m b ofs Cur Nonempty)

  (** val weak_valid_pointer : mem -> block -> z -> bool **)

  let weak_valid_pointer m b ofs =
    (||) (valid_pointer m b ofs) (valid_pointer m b (Z.sub ofs (Zpos XH)))

  (** val empty : mem **)

  let empty =
    { mem_contents = (PMap.init (ZMap.init Undef)); mem_access = (PMap.init (fun _ _ -> None)); nextblock = XH }

  (** val alloc : mem -> z -> z -> mem' * block **)

  let alloc m lo hi =
    ({ mem_contents = (PMap.set m.nextblock (ZMap.init Undef) m.mem_contents); mem_access =
      (PMap.set m.nextblock (fun ofs _ ->
        if (&&) ((fun x -> x) (zle lo ofs)) ((fun x -> x) (zlt ofs hi)) then Some Freeable else None) m.mem_access); nextblock =
      (Coq_Pos.succ m.nextblock) }, m.nextblock)

  (** val unchecked_free : mem -> block -> z -> z -> mem **)

  let unchecked_free m b lo hi =
    { mem_contents = m.mem_contents; mem_access =
      (PMap.set b (fun ofs k ->
        if (&&) ((fun x -> x) (zle lo ofs)) ((fun x -> x) (zlt ofs hi)) then None else PMap.get b m.mem_access ofs k)
        m.mem_access); nextblock = m.nextblock }

  (** val free : mem -> block -> z -> z -> mem option **)

  let free m b lo hi =
    if range_perm_dec m b lo hi Cur Freeable then Some (unchecked_free m b lo hi) else None

  (** val free_list : mem -> ((block * z) * z) list -> mem option **)

  let rec free_list m = function
  | [] -> Some m
  | p :: l' -> let (p0, hi) = p in let (b, lo) = p0 in (match free m b lo hi with
                                                        | Some m' -> free_list m' l'
                                                        | None -> None)

  (** val getN : int -> z -> memval ZMap.t -> memval list **)

  let rec getN n0 p c =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n' -> (ZMap.get p c) :: (getN n' (Z.add p (Zpos XH)) c))
      n0

  (** val load : memory_chunk -> mem -> block -> z -> val0 option **)

  let load chunk m b ofs =
    if valid_access_dec m chunk b ofs Readable
    then Some (decode_val chunk (getN (size_chunk_nat chunk) ofs (PMap.get b m.mem_contents)))
    else None

  (** val loadv : memory_chunk -> mem -> val0 -> val0 option **)

  let loadv chunk m = function
  | Vptr (b, ofs) -> load chunk m b (Int.unsigned ofs)
  | _ -> None

  (** val loadbytes : mem -> block -> z -> z -> memval list option **)

  let loadbytes m b ofs n0 =
    if range_perm_dec m b ofs (Z.add ofs n0) Cur Readable then Some (getN (nat_of_Z n0) ofs (PMap.get b m.mem_contents)) else None

  (** val setN : memval list -> z -> memval ZMap.t -> memval ZMap.t **)

  let rec setN vl p c =
    match vl with
    | [] -> c
    | v :: vl' -> setN vl' (Z.add p (Zpos XH)) (ZMap.set p v c)

  (** val store : memory_chunk -> mem -> block -> z -> val0 -> mem option **)

  let store chunk m b ofs v =
    if valid_access_dec m chunk b ofs Writable
    then Some { mem_contents = (PMap.set b (setN (encode_val chunk v) ofs (PMap.get b m.mem_contents)) m.mem_contents);
           mem_access = m.mem_access; nextblock = m.nextblock }
    else None

  (** val storev : memory_chunk -> mem -> val0 -> val0 -> mem option **)

  let storev chunk m addr v =
    match addr with
    | Vptr (b, ofs) -> store chunk m b (Int.unsigned ofs) v
    | _ -> None

  (** val storebytes : mem -> block -> z -> memval list -> mem option **)

  let storebytes m b ofs bytes =
    if range_perm_dec m b ofs (Z.add ofs (Z.of_nat (length bytes))) Cur Writable
    then Some { mem_contents = (PMap.set b (setN bytes ofs (PMap.get b m.mem_contents)) m.mem_contents); mem_access =
           m.mem_access; nextblock = m.nextblock }
    else None

  (** val drop_perm : mem -> block -> z -> z -> permission -> mem option **)

  let drop_perm m b lo hi p =
    if range_perm_dec m b lo hi Cur Freeable
    then Some { mem_contents = m.mem_contents; mem_access =
           (PMap.set b (fun ofs k ->
             if (&&) ((fun x -> x) (zle lo ofs)) ((fun x -> x) (zlt ofs hi)) then Some p else PMap.get b m.mem_access ofs k)
             m.mem_access); nextblock = m.nextblock }
    else None
 end

type signedness =
| Signed
| Unsigned

type intsize =
| I8
| I16
| I32
| IBool

type floatsize =
| F32
| F64

type attr = { attr_volatile : bool; attr_alignas : n option }

(** val attr_volatile : attr -> bool **)

let attr_volatile x = x.attr_volatile

(** val attr_alignas : attr -> n option **)

let attr_alignas x = x.attr_alignas

(** val noattr : attr **)

let noattr =
  { attr_volatile = false; attr_alignas = None }

type type0 =
| Tvoid
| Tint0 of intsize * signedness * attr
| Tlong0 of signedness * attr
| Tfloat0 of floatsize * attr
| Tpointer of type0 * attr
| Tarray of type0 * z * attr
| Tfunction of typelist * type0 * calling_convention
| Tstruct of ident0 * attr
| Tunion of ident0 * attr
and typelist =
| Tnil
| Tcons of type0 * typelist

(** val type_eq : type0 -> type0 -> bool **)

let rec type_eq =
  let h = fun x y ->
    match x with
    | I8 -> (match y with
             | I8 -> true
             | _ -> false)
    | I16 -> (match y with
              | I16 -> true
              | _ -> false)
    | I32 -> (match y with
              | I32 -> true
              | _ -> false)
    | IBool -> (match y with
                | IBool -> true
                | _ -> false)
  in
  let h0 = fun x y ->
    match x with
    | Signed -> (match y with
                 | Signed -> true
                 | Unsigned -> false)
    | Unsigned -> (match y with
                   | Signed -> false
                   | Unsigned -> true)
  in
  let h1 = fun x y ->
    match x with
    | F32 -> (match y with
              | F32 -> true
              | F64 -> false)
    | F64 -> (match y with
              | F32 -> false
              | F64 -> true)
  in
  let h2 = fun x y ->
    let { attr_volatile = attr_volatile0; attr_alignas = attr_alignas0 } = x in
    let { attr_volatile = attr_volatile1; attr_alignas = attr_alignas1 } = y in
    if bool_dec attr_volatile0 attr_volatile1
    then (match attr_alignas0 with
          | Some x0 -> (match attr_alignas1 with
                        | Some n0 -> N.eq_dec x0 n0
                        | None -> false)
          | None -> (match attr_alignas1 with
                     | Some _ -> false
                     | None -> true))
    else false
  in
  (fun ty1 ty2 ->
  match ty1 with
  | Tvoid -> (match ty2 with
              | Tvoid -> true
              | _ -> false)
  | Tint0 (i, s, a) ->
    (match ty2 with
     | Tint0 (i0, s0, a0) -> if h i i0 then if h0 s s0 then h2 a a0 else false else false
     | _ -> false)
  | Tlong0 (s, a) -> (match ty2 with
                      | Tlong0 (s0, a0) -> if h0 s s0 then h2 a a0 else false
                      | _ -> false)
  | Tfloat0 (f, a) -> (match ty2 with
                       | Tfloat0 (f0, a0) -> if h1 f f0 then h2 a a0 else false
                       | _ -> false)
  | Tpointer (t0, a) -> (match ty2 with
                         | Tpointer (t1, a0) -> if type_eq t0 t1 then h2 a a0 else false
                         | _ -> false)
  | Tarray (t0, z0, a) ->
    (match ty2 with
     | Tarray (t1, z1, a0) -> if type_eq t0 t1 then if zeq z0 z1 then h2 a a0 else false else false
     | _ -> false)
  | Tfunction (t0, t1, c) ->
    (match ty2 with
     | Tfunction (t2, t3, c0) ->
       if typelist_eq t0 t2
       then if type_eq t1 t3
            then let { cc_vararg = cc_vararg0; cc_unproto = cc_unproto0; cc_structret = cc_structret0 } = c in
                 let { cc_vararg = cc_vararg1; cc_unproto = cc_unproto1; cc_structret = cc_structret1 } = c0 in
                 if bool_dec cc_vararg0 cc_vararg1
                 then if bool_dec cc_unproto0 cc_unproto1 then bool_dec cc_structret0 cc_structret1 else false
                 else false
            else false
       else false
     | _ -> false)
  | Tstruct (i, a) -> (match ty2 with
                       | Tstruct (i0, a0) -> if ident_eq i i0 then h2 a a0 else false
                       | _ -> false)
  | Tunion (i, a) -> (match ty2 with
                      | Tunion (i0, a0) -> if ident_eq i i0 then h2 a a0 else false
                      | _ -> false))

(** val typelist_eq : typelist -> typelist -> bool **)

and typelist_eq tyl1 tyl2 =
  match tyl1 with
  | Tnil -> (match tyl2 with
             | Tnil -> true
             | Tcons (_, _) -> false)
  | Tcons (t0, t1) -> (match tyl2 with
                       | Tnil -> false
                       | Tcons (t2, t3) -> if type_eq t0 t2 then typelist_eq t1 t3 else false)

(** val attr_of_type : type0 -> attr **)

let attr_of_type = function
| Tint0 (_, _, a) -> a
| Tlong0 (_, a) -> a
| Tfloat0 (_, a) -> a
| Tpointer (_, a) -> a
| Tarray (_, _, a) -> a
| Tstruct (_, a) -> a
| Tunion (_, a) -> a
| _ -> noattr

(** val change_attributes : (attr -> attr) -> type0 -> type0 **)

let change_attributes f ty = match ty with
| Tint0 (sz, si, a) -> Tint0 (sz, si, (f a))
| Tlong0 (si, a) -> Tlong0 (si, (f a))
| Tfloat0 (sz, a) -> Tfloat0 (sz, (f a))
| Tpointer (elt0, a) -> Tpointer (elt0, (f a))
| Tarray (elt0, sz, a) -> Tarray (elt0, sz, (f a))
| Tstruct (id, a) -> Tstruct (id, (f a))
| Tunion (id, a) -> Tunion (id, (f a))
| _ -> ty

(** val remove_attributes : type0 -> type0 **)

let remove_attributes ty =
  change_attributes (fun _ -> noattr) ty

type struct_or_union =
| Struct
| Union

type members = (ident0 * type0) list

type composite_definition =
| Composite of ident0 * struct_or_union * members * attr

type composite = { co_su : struct_or_union; co_members : members; co_attr : attr; co_sizeof : z; co_alignof : z; co_rank : int }

(** val co_members : composite -> members **)

let co_members x = x.co_members

(** val co_sizeof : composite -> z **)

let co_sizeof x = x.co_sizeof

(** val co_alignof : composite -> z **)

let co_alignof x = x.co_alignof

type composite_env = composite PTree.t

(** val typeconv : type0 -> type0 **)

let typeconv ty = match ty with
| Tint0 (i, _, _) -> (match i with
                      | I32 -> remove_attributes ty
                      | _ -> Tint0 (I32, Signed, noattr))
| Tarray (t0, _, _) -> Tpointer (t0, noattr)
| Tfunction (_, _, _) -> Tpointer (ty, noattr)
| _ -> remove_attributes ty

(** val align_attr : attr -> z -> z **)

let align_attr a al =
  match a.attr_alignas with
  | Some l -> two_p (Z.of_N l)
  | None -> al

(** val alignof : composite_env -> type0 -> z **)

let rec alignof env0 t0 =
  align_attr (attr_of_type t0)
    (match t0 with
     | Tvoid -> Zpos XH
     | Tint0 (i, _, _) -> (match i with
                           | I16 -> Zpos (XO XH)
                           | I32 -> Zpos (XO (XO XH))
                           | _ -> Zpos XH)
     | Tarray (t', _, _) -> alignof env0 t'
     | Tfunction (_, _, _) -> Zpos XH
     | Tstruct (id, _) -> (match PTree.get id env0 with
                           | Some co -> co.co_alignof
                           | None -> Zpos XH)
     | Tunion (id, _) -> (match PTree.get id env0 with
                          | Some co -> co.co_alignof
                          | None -> Zpos XH)
     | _ -> Zpos (XO (XO XH)))

(** val sizeof : composite_env -> type0 -> z **)

let rec sizeof env0 = function
| Tint0 (i, _, _) -> (match i with
                      | I16 -> Zpos (XO XH)
                      | I32 -> Zpos (XO (XO XH))
                      | _ -> Zpos XH)
| Tlong0 (_, _) -> Zpos (XO (XO (XO XH)))
| Tfloat0 (f, _) -> (match f with
                     | F32 -> Zpos (XO (XO XH))
                     | F64 -> Zpos (XO (XO (XO XH))))
| Tpointer (_, _) -> Zpos (XO (XO XH))
| Tarray (t', n0, _) -> Z.mul (sizeof env0 t') (Z.max Z0 n0)
| Tstruct (id, _) -> (match PTree.get id env0 with
                      | Some co -> co.co_sizeof
                      | None -> Z0)
| Tunion (id, _) -> (match PTree.get id env0 with
                     | Some co -> co.co_sizeof
                     | None -> Z0)
| _ -> Zpos XH

(** val field_offset_rec : composite_env -> ident0 -> members -> z -> z res **)

let rec field_offset_rec env0 id fld pos =
  match fld with
  | [] ->
    Error0 ((MSG ('U'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::[]))))))))))))))) :: ((CTX
      id) :: []))
  | p :: fld' ->
    let (id', t0) = p in
    if ident_eq id id'
    then OK (align pos (alignof env0 t0))
    else field_offset_rec env0 id fld' (Z.add (align pos (alignof env0 t0)) (sizeof env0 t0))

(** val field_offset : composite_env -> ident0 -> members -> z res **)

let field_offset env0 id fld =
  field_offset_rec env0 id fld Z0

type mode0 =
| By_value of memory_chunk
| By_reference
| By_copy
| By_nothing

(** val access_mode : type0 -> mode0 **)

let access_mode = function
| Tvoid -> By_nothing
| Tint0 (i, s, _) ->
  (match i with
   | I8 -> (match s with
            | Signed -> By_value Mint8signed
            | Unsigned -> By_value Mint8unsigned)
   | I16 -> (match s with
             | Signed -> By_value Mint16signed
             | Unsigned -> By_value Mint16unsigned)
   | I32 -> By_value Mint32
   | IBool -> By_value Mint8unsigned)
| Tlong0 (_, _) -> By_value Mint64
| Tfloat0 (f, _) -> (match f with
                     | F32 -> By_value Mfloat32
                     | F64 -> By_value Mfloat64)
| Tpointer (_, _) -> By_value Mint32
| Tarray (_, _, _) -> By_reference
| Tfunction (_, _, _) -> By_reference
| _ -> By_copy

(** val type_is_volatile : type0 -> bool **)

let type_is_volatile ty =
  match access_mode ty with
  | By_value _ -> (attr_of_type ty).attr_volatile
  | _ -> false

(** val alignof_blockcopy : composite_env -> type0 -> z **)

let rec alignof_blockcopy env0 = function
| Tint0 (i, _, _) -> (match i with
                      | I16 -> Zpos (XO XH)
                      | I32 -> Zpos (XO (XO XH))
                      | _ -> Zpos XH)
| Tlong0 (_, _) -> Zpos (XO (XO (XO XH)))
| Tfloat0 (f, _) -> (match f with
                     | F32 -> Zpos (XO (XO XH))
                     | F64 -> Zpos (XO (XO (XO XH))))
| Tpointer (_, _) -> Zpos (XO (XO XH))
| Tarray (t', _, _) -> alignof_blockcopy env0 t'
| Tstruct (id, _) -> (match PTree.get id env0 with
                      | Some co -> Z.min (Zpos (XO (XO (XO XH)))) co.co_alignof
                      | None -> Zpos XH)
| Tunion (id, _) -> (match PTree.get id env0 with
                     | Some co -> Z.min (Zpos (XO (XO (XO XH)))) co.co_alignof
                     | None -> Zpos XH)
| _ -> Zpos XH

(** val type_of_params : (ident0 * type0) list -> typelist **)

let rec type_of_params = function
| [] -> Tnil
| p :: rem0 -> let (_, ty) = p in Tcons (ty, (type_of_params rem0))

type 'f fundef =
| Internal of 'f
| External of external_function * typelist * type0 * calling_convention

type 'f program0 = { prog_defs0 : (ident0 * ('f fundef, type0) globdef) list; prog_public0 : ident0 list; prog_main0 : ident0;
                     prog_types : composite_definition list; prog_comp_env : composite_env }

(** val prog_defs0 : 'a1 program0 -> (ident0 * ('a1 fundef, type0) globdef) list **)

let prog_defs0 x = x.prog_defs0

(** val prog_public0 : 'a1 program0 -> ident0 list **)

let prog_public0 x = x.prog_public0

(** val prog_main0 : 'a1 program0 -> ident0 **)

let prog_main0 x = x.prog_main0

(** val prog_comp_env : 'a1 program0 -> composite_env **)

let prog_comp_env x = x.prog_comp_env

(** val program_of_program : 'a1 program0 -> ('a1 fundef, type0) program **)

let program_of_program p =
  { prog_defs = p.prog_defs0; prog_public = p.prog_public0; prog_main = p.prog_main0 }

type unary_operation =
| Onotbool
| Onotint
| Oneg
| Oabsfloat

type binary_operation =
| Oadd
| Osub
| Omul
| Odiv
| Omod
| Oand
| Oor
| Oxor
| Oshl
| Oshr
| Oeq
| One
| Olt
| Ogt
| Ole
| Oge

type classify_cast_cases =
| Cast_case_neutral
| Cast_case_i2i of intsize * signedness
| Cast_case_f2f
| Cast_case_s2s
| Cast_case_f2s
| Cast_case_s2f
| Cast_case_i2f of signedness
| Cast_case_i2s of signedness
| Cast_case_f2i of intsize * signedness
| Cast_case_s2i of intsize * signedness
| Cast_case_l2l
| Cast_case_i2l of signedness
| Cast_case_l2i of intsize * signedness
| Cast_case_l2f of signedness
| Cast_case_l2s of signedness
| Cast_case_f2l of signedness
| Cast_case_s2l of signedness
| Cast_case_f2bool
| Cast_case_s2bool
| Cast_case_l2bool
| Cast_case_p2bool
| Cast_case_struct of ident0 * ident0
| Cast_case_union of ident0 * ident0
| Cast_case_void
| Cast_case_default

(** val classify_cast : type0 -> type0 -> classify_cast_cases **)

let classify_cast tfrom = function
| Tvoid -> Cast_case_void
| Tint0 (sz2, si2, _) ->
  (match sz2 with
   | I32 ->
     (match tfrom with
      | Tvoid -> Cast_case_default
      | Tlong0 (_, _) -> Cast_case_l2i (sz2, si2)
      | Tfloat0 (f, _) -> (match f with
                           | F32 -> Cast_case_s2i (sz2, si2)
                           | F64 -> Cast_case_f2i (sz2, si2))
      | Tstruct (_, _) -> Cast_case_default
      | Tunion (_, _) -> Cast_case_default
      | _ -> Cast_case_neutral)
   | IBool ->
     (match tfrom with
      | Tvoid -> Cast_case_default
      | Tint0 (_, _, _) -> Cast_case_i2i (sz2, si2)
      | Tlong0 (_, _) -> Cast_case_l2bool
      | Tfloat0 (f, _) -> (match f with
                           | F32 -> Cast_case_s2bool
                           | F64 -> Cast_case_f2bool)
      | Tstruct (_, _) -> Cast_case_default
      | Tunion (_, _) -> Cast_case_default
      | _ -> Cast_case_p2bool)
   | _ ->
     (match tfrom with
      | Tint0 (_, _, _) -> Cast_case_i2i (sz2, si2)
      | Tlong0 (_, _) -> Cast_case_l2i (sz2, si2)
      | Tfloat0 (f, _) -> (match f with
                           | F32 -> Cast_case_s2i (sz2, si2)
                           | F64 -> Cast_case_f2i (sz2, si2))
      | _ -> Cast_case_default))
| Tlong0 (si2, _) ->
  (match tfrom with
   | Tvoid -> Cast_case_default
   | Tint0 (_, si1, _) -> Cast_case_i2l si1
   | Tlong0 (_, _) -> Cast_case_l2l
   | Tfloat0 (f, _) -> (match f with
                        | F32 -> Cast_case_s2l si2
                        | F64 -> Cast_case_f2l si2)
   | Tstruct (_, _) -> Cast_case_default
   | Tunion (_, _) -> Cast_case_default
   | _ -> Cast_case_i2l si2)
| Tfloat0 (f, _) ->
  (match f with
   | F32 ->
     (match tfrom with
      | Tint0 (_, si1, _) -> Cast_case_i2s si1
      | Tlong0 (si1, _) -> Cast_case_l2s si1
      | Tfloat0 (f0, _) -> (match f0 with
                            | F32 -> Cast_case_s2s
                            | F64 -> Cast_case_f2s)
      | _ -> Cast_case_default)
   | F64 ->
     (match tfrom with
      | Tint0 (_, si1, _) -> Cast_case_i2f si1
      | Tlong0 (si1, _) -> Cast_case_l2f si1
      | Tfloat0 (f0, _) -> (match f0 with
                            | F32 -> Cast_case_s2f
                            | F64 -> Cast_case_f2f)
      | _ -> Cast_case_default))
| Tpointer (_, _) ->
  (match tfrom with
   | Tvoid -> Cast_case_default
   | Tlong0 (_, _) -> Cast_case_l2i (I32, Unsigned)
   | Tfloat0 (_, _) -> Cast_case_default
   | Tstruct (_, _) -> Cast_case_default
   | Tunion (_, _) -> Cast_case_default
   | _ -> Cast_case_neutral)
| Tstruct (id2, _) -> (match tfrom with
                       | Tstruct (id1, _) -> Cast_case_struct (id1, id2)
                       | _ -> Cast_case_default)
| Tunion (id2, _) -> (match tfrom with
                      | Tunion (id1, _) -> Cast_case_union (id1, id2)
                      | _ -> Cast_case_default)
| _ -> Cast_case_default

(** val cast_int_int : intsize -> signedness -> Int.int -> Int.int **)

let cast_int_int sz sg i =
  match sz with
  | I8 -> (match sg with
           | Signed -> Int.sign_ext (Zpos (XO (XO (XO XH)))) i
           | Unsigned -> Int.zero_ext (Zpos (XO (XO (XO XH)))) i)
  | I16 ->
    (match sg with
     | Signed -> Int.sign_ext (Zpos (XO (XO (XO (XO XH))))) i
     | Unsigned -> Int.zero_ext (Zpos (XO (XO (XO (XO XH))))) i)
  | I32 -> i
  | IBool -> if Int.eq i Int.zero then Int.zero else Int.one

(** val cast_int_float : signedness -> Int.int -> float **)

let cast_int_float si i =
  match si with
  | Signed -> Float.of_int i
  | Unsigned -> Float.of_intu i

(** val cast_float_int : signedness -> float -> Int.int option **)

let cast_float_int si f =
  match si with
  | Signed -> Float.to_int f
  | Unsigned -> Float.to_intu f

(** val cast_int_single : signedness -> Int.int -> float32 **)

let cast_int_single si i =
  match si with
  | Signed -> Float32.of_int i
  | Unsigned -> Float32.of_intu i

(** val cast_single_int : signedness -> float32 -> Int.int option **)

let cast_single_int si f =
  match si with
  | Signed -> Float32.to_int f
  | Unsigned -> Float32.to_intu f

(** val cast_int_long : signedness -> Int.int -> Int64.int **)

let cast_int_long si i =
  match si with
  | Signed -> Int64.repr (Int.signed i)
  | Unsigned -> Int64.repr (Int.unsigned i)

(** val cast_long_float : signedness -> Int64.int -> float **)

let cast_long_float si i =
  match si with
  | Signed -> Float.of_long i
  | Unsigned -> Float.of_longu i

(** val cast_long_single : signedness -> Int64.int -> float32 **)

let cast_long_single si i =
  match si with
  | Signed -> Float32.of_long i
  | Unsigned -> Float32.of_longu i

(** val cast_float_long : signedness -> float -> Int64.int option **)

let cast_float_long si f =
  match si with
  | Signed -> Float.to_long f
  | Unsigned -> Float.to_longu f

(** val cast_single_long : signedness -> float32 -> Int64.int option **)

let cast_single_long si f =
  match si with
  | Signed -> Float32.to_long f
  | Unsigned -> Float32.to_longu f

(** val sem_cast : val0 -> type0 -> type0 -> Mem.mem -> val0 option **)

let sem_cast v t1 t2 m =
  match classify_cast t1 t2 with
  | Cast_case_neutral -> (match v with
                          | Vint _ -> Some v
                          | Vptr (_, _) -> Some v
                          | _ -> None)
  | Cast_case_i2i (sz2, si2) -> (match v with
                                 | Vint i -> Some (Vint (cast_int_int sz2 si2 i))
                                 | _ -> None)
  | Cast_case_f2f -> (match v with
                      | Vfloat f -> Some (Vfloat f)
                      | _ -> None)
  | Cast_case_s2s -> (match v with
                      | Vsingle f -> Some (Vsingle f)
                      | _ -> None)
  | Cast_case_f2s -> (match v with
                      | Vfloat f -> Some (Vsingle (Float.to_single f))
                      | _ -> None)
  | Cast_case_s2f -> (match v with
                      | Vsingle f -> Some (Vfloat (Float.of_single f))
                      | _ -> None)
  | Cast_case_i2f si1 -> (match v with
                          | Vint i -> Some (Vfloat (cast_int_float si1 i))
                          | _ -> None)
  | Cast_case_i2s si1 -> (match v with
                          | Vint i -> Some (Vsingle (cast_int_single si1 i))
                          | _ -> None)
  | Cast_case_f2i (sz2, si2) ->
    (match v with
     | Vfloat f -> (match cast_float_int si2 f with
                    | Some i -> Some (Vint (cast_int_int sz2 si2 i))
                    | None -> None)
     | _ -> None)
  | Cast_case_s2i (sz2, si2) ->
    (match v with
     | Vsingle f -> (match cast_single_int si2 f with
                     | Some i -> Some (Vint (cast_int_int sz2 si2 i))
                     | None -> None)
     | _ -> None)
  | Cast_case_l2l -> (match v with
                      | Vlong n0 -> Some (Vlong n0)
                      | _ -> None)
  | Cast_case_i2l si -> (match v with
                         | Vint n0 -> Some (Vlong (cast_int_long si n0))
                         | _ -> None)
  | Cast_case_l2i (sz, si) ->
    (match v with
     | Vlong n0 -> Some (Vint (cast_int_int sz si (Int.repr (Int64.unsigned n0))))
     | _ -> None)
  | Cast_case_l2f si1 -> (match v with
                          | Vlong i -> Some (Vfloat (cast_long_float si1 i))
                          | _ -> None)
  | Cast_case_l2s si1 -> (match v with
                          | Vlong i -> Some (Vsingle (cast_long_single si1 i))
                          | _ -> None)
  | Cast_case_f2l si2 ->
    (match v with
     | Vfloat f -> (match cast_float_long si2 f with
                    | Some i -> Some (Vlong i)
                    | None -> None)
     | _ -> None)
  | Cast_case_s2l si2 ->
    (match v with
     | Vsingle f -> (match cast_single_long si2 f with
                     | Some i -> Some (Vlong i)
                     | None -> None)
     | _ -> None)
  | Cast_case_f2bool ->
    (match v with
     | Vfloat f -> Some (Vint (if Float.cmp Ceq f Float.zero then Int.zero else Int.one))
     | _ -> None)
  | Cast_case_s2bool ->
    (match v with
     | Vsingle f -> Some (Vint (if Float32.cmp Ceq f Float32.zero then Int.zero else Int.one))
     | _ -> None)
  | Cast_case_l2bool -> (match v with
                         | Vlong n0 -> Some (Vint (if Int64.eq n0 Int64.zero then Int.zero else Int.one))
                         | _ -> None)
  | Cast_case_p2bool ->
    (match v with
     | Vint i -> Some (Vint (cast_int_int IBool Signed i))
     | Vptr (b, ofs) -> if Mem.weak_valid_pointer m b (Int.unsigned ofs) then Some vone else None
     | _ -> None)
  | Cast_case_struct (id1, id2) -> (match v with
                                    | Vptr (_, _) -> if ident_eq id1 id2 then Some v else None
                                    | _ -> None)
  | Cast_case_union (id1, id2) -> (match v with
                                   | Vptr (_, _) -> if ident_eq id1 id2 then Some v else None
                                   | _ -> None)
  | Cast_case_void -> Some v
  | Cast_case_default -> None

type classify_bool_cases =
| Bool_case_i
| Bool_case_f
| Bool_case_s
| Bool_case_p
| Bool_case_l
| Bool_default

(** val classify_bool : type0 -> classify_bool_cases **)

let classify_bool ty =
  match typeconv ty with
  | Tint0 (_, _, _) -> Bool_case_i
  | Tlong0 (_, _) -> Bool_case_l
  | Tfloat0 (f, _) -> (match f with
                       | F32 -> Bool_case_s
                       | F64 -> Bool_case_f)
  | Tpointer (_, _) -> Bool_case_p
  | _ -> Bool_default

(** val bool_val : val0 -> type0 -> Mem.mem -> bool option **)

let bool_val v t0 m =
  match classify_bool t0 with
  | Bool_case_i -> (match v with
                    | Vint n0 -> Some (negb (Int.eq n0 Int.zero))
                    | _ -> None)
  | Bool_case_f -> (match v with
                    | Vfloat f -> Some (negb (Float.cmp Ceq f Float.zero))
                    | _ -> None)
  | Bool_case_s -> (match v with
                    | Vsingle f -> Some (negb (Float32.cmp Ceq f Float32.zero))
                    | _ -> None)
  | Bool_case_p ->
    (match v with
     | Vint n0 -> Some (negb (Int.eq n0 Int.zero))
     | Vptr (b, ofs) -> if Mem.weak_valid_pointer m b (Int.unsigned ofs) then Some true else None
     | _ -> None)
  | Bool_case_l -> (match v with
                    | Vlong n0 -> Some (negb (Int64.eq n0 Int64.zero))
                    | _ -> None)
  | Bool_default -> None

(** val sem_notbool : val0 -> type0 -> Mem.mem -> val0 option **)

let sem_notbool v ty m =
  match classify_bool ty with
  | Bool_case_i -> (match v with
                    | Vint n0 -> Some (Val.of_bool (Int.eq n0 Int.zero))
                    | _ -> None)
  | Bool_case_f -> (match v with
                    | Vfloat f -> Some (Val.of_bool (Float.cmp Ceq f Float.zero))
                    | _ -> None)
  | Bool_case_s -> (match v with
                    | Vsingle f -> Some (Val.of_bool (Float32.cmp Ceq f Float32.zero))
                    | _ -> None)
  | Bool_case_p ->
    (match v with
     | Vint n0 -> Some (Val.of_bool (Int.eq n0 Int.zero))
     | Vptr (b, ofs) -> if Mem.weak_valid_pointer m b (Int.unsigned ofs) then Some vfalse else None
     | _ -> None)
  | Bool_case_l -> (match v with
                    | Vlong n0 -> Some (Val.of_bool (Int64.eq n0 Int64.zero))
                    | _ -> None)
  | Bool_default -> None

type classify_neg_cases =
| Neg_case_i of signedness
| Neg_case_f
| Neg_case_s
| Neg_case_l of signedness
| Neg_default

(** val classify_neg : type0 -> classify_neg_cases **)

let classify_neg = function
| Tint0 (i, s, _) -> (match i with
                      | I32 -> Neg_case_i s
                      | _ -> Neg_case_i Signed)
| Tlong0 (si, _) -> Neg_case_l si
| Tfloat0 (f, _) -> (match f with
                     | F32 -> Neg_case_s
                     | F64 -> Neg_case_f)
| _ -> Neg_default

(** val sem_neg : val0 -> type0 -> val0 option **)

let sem_neg v ty =
  match classify_neg ty with
  | Neg_case_i _ -> (match v with
                     | Vint n0 -> Some (Vint (Int.neg n0))
                     | _ -> None)
  | Neg_case_f -> (match v with
                   | Vfloat f -> Some (Vfloat (Float.neg f))
                   | _ -> None)
  | Neg_case_s -> (match v with
                   | Vsingle f -> Some (Vsingle (Float32.neg f))
                   | _ -> None)
  | Neg_case_l _ -> (match v with
                     | Vlong n0 -> Some (Vlong (Int64.neg n0))
                     | _ -> None)
  | Neg_default -> None

(** val sem_absfloat : val0 -> type0 -> val0 option **)

let sem_absfloat v ty =
  match classify_neg ty with
  | Neg_case_i sg -> (match v with
                      | Vint n0 -> Some (Vfloat (Float.abs (cast_int_float sg n0)))
                      | _ -> None)
  | Neg_case_f -> (match v with
                   | Vfloat f -> Some (Vfloat (Float.abs f))
                   | _ -> None)
  | Neg_case_s -> (match v with
                   | Vsingle f -> Some (Vfloat (Float.abs (Float.of_single f)))
                   | _ -> None)
  | Neg_case_l sg -> (match v with
                      | Vlong n0 -> Some (Vfloat (Float.abs (cast_long_float sg n0)))
                      | _ -> None)
  | Neg_default -> None

type classify_notint_cases =
| Notint_case_i of signedness
| Notint_case_l of signedness
| Notint_default

(** val classify_notint : type0 -> classify_notint_cases **)

let classify_notint = function
| Tint0 (i, s, _) -> (match i with
                      | I32 -> Notint_case_i s
                      | _ -> Notint_case_i Signed)
| Tlong0 (si, _) -> Notint_case_l si
| _ -> Notint_default

(** val sem_notint : val0 -> type0 -> val0 option **)

let sem_notint v ty =
  match classify_notint ty with
  | Notint_case_i _ -> (match v with
                        | Vint n0 -> Some (Vint (Int.not n0))
                        | _ -> None)
  | Notint_case_l _ -> (match v with
                        | Vlong n0 -> Some (Vlong (Int64.not n0))
                        | _ -> None)
  | Notint_default -> None

type binarith_cases =
| Bin_case_i of signedness
| Bin_case_l of signedness
| Bin_case_f
| Bin_case_s
| Bin_default

(** val classify_binarith : type0 -> type0 -> binarith_cases **)

let classify_binarith ty1 ty2 =
  match ty1 with
  | Tint0 (i, s, _) ->
    (match i with
     | I32 ->
       (match s with
        | Signed ->
          (match ty2 with
           | Tint0 (i0, s0, _) -> (match i0 with
                                   | I32 -> Bin_case_i s0
                                   | _ -> Bin_case_i Signed)
           | Tlong0 (sg, _) -> Bin_case_l sg
           | Tfloat0 (f, _) -> (match f with
                                | F32 -> Bin_case_s
                                | F64 -> Bin_case_f)
           | _ -> Bin_default)
        | Unsigned ->
          (match ty2 with
           | Tint0 (_, _, _) -> Bin_case_i Unsigned
           | Tlong0 (sg, _) -> Bin_case_l sg
           | Tfloat0 (f, _) -> (match f with
                                | F32 -> Bin_case_s
                                | F64 -> Bin_case_f)
           | _ -> Bin_default))
     | _ ->
       (match ty2 with
        | Tint0 (i0, s0, _) -> (match i0 with
                                | I32 -> Bin_case_i s0
                                | _ -> Bin_case_i Signed)
        | Tlong0 (sg, _) -> Bin_case_l sg
        | Tfloat0 (f, _) -> (match f with
                             | F32 -> Bin_case_s
                             | F64 -> Bin_case_f)
        | _ -> Bin_default))
  | Tlong0 (sg, _) ->
    (match sg with
     | Signed ->
       (match ty2 with
        | Tint0 (_, _, _) -> Bin_case_l sg
        | Tlong0 (s, _) -> Bin_case_l s
        | Tfloat0 (f, _) -> (match f with
                             | F32 -> Bin_case_s
                             | F64 -> Bin_case_f)
        | _ -> Bin_default)
     | Unsigned ->
       (match ty2 with
        | Tint0 (_, _, _) -> Bin_case_l sg
        | Tlong0 (_, _) -> Bin_case_l Unsigned
        | Tfloat0 (f, _) -> (match f with
                             | F32 -> Bin_case_s
                             | F64 -> Bin_case_f)
        | _ -> Bin_default))
  | Tfloat0 (f, _) ->
    (match f with
     | F32 ->
       (match ty2 with
        | Tint0 (_, _, _) -> Bin_case_s
        | Tlong0 (_, _) -> Bin_case_s
        | Tfloat0 (f0, _) -> (match f0 with
                              | F32 -> Bin_case_s
                              | F64 -> Bin_case_f)
        | _ -> Bin_default)
     | F64 ->
       (match ty2 with
        | Tint0 (_, _, _) -> Bin_case_f
        | Tlong0 (_, _) -> Bin_case_f
        | Tfloat0 (_, _) -> Bin_case_f
        | _ -> Bin_default))
  | _ -> Bin_default

(** val binarith_type : binarith_cases -> type0 **)

let binarith_type = function
| Bin_case_i sg -> Tint0 (I32, sg, noattr)
| Bin_case_l sg -> Tlong0 (sg, noattr)
| Bin_case_f -> Tfloat0 (F64, noattr)
| Bin_case_s -> Tfloat0 (F32, noattr)
| Bin_default -> Tvoid

(** val sem_binarith :
    (signedness -> Int.int -> Int.int -> val0 option) -> (signedness -> Int64.int -> Int64.int -> val0 option) -> (float -> float
    -> val0 option) -> (float32 -> float32 -> val0 option) -> val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_binarith sem_int sem_long sem_float sem_single v1 t1 v2 t2 m =
  let c = classify_binarith t1 t2 in
  let t0 = binarith_type c in
  (match sem_cast v1 t1 t0 m with
   | Some v1' ->
     (match sem_cast v2 t2 t0 m with
      | Some v2' ->
        (match c with
         | Bin_case_i sg -> (match v1' with
                             | Vint n1 -> (match v2' with
                                           | Vint n2 -> sem_int sg n1 n2
                                           | _ -> None)
                             | _ -> None)
         | Bin_case_l sg -> (match v1' with
                             | Vlong n1 -> (match v2' with
                                            | Vlong n2 -> sem_long sg n1 n2
                                            | _ -> None)
                             | _ -> None)
         | Bin_case_f -> (match v1' with
                          | Vfloat n1 -> (match v2' with
                                          | Vfloat n2 -> sem_float n1 n2
                                          | _ -> None)
                          | _ -> None)
         | Bin_case_s -> (match v1' with
                          | Vsingle n1 -> (match v2' with
                                           | Vsingle n2 -> sem_single n1 n2
                                           | _ -> None)
                          | _ -> None)
         | Bin_default -> None)
      | None -> None)
   | None -> None)

type classify_add_cases =
| Add_case_pi of type0
| Add_case_ip of type0
| Add_case_pl of type0
| Add_case_lp of type0
| Add_default

(** val classify_add : type0 -> type0 -> classify_add_cases **)

let classify_add ty1 ty2 =
  match typeconv ty1 with
  | Tint0 (_, _, _) -> (match typeconv ty2 with
                        | Tpointer (ty, _) -> Add_case_ip ty
                        | _ -> Add_default)
  | Tlong0 (_, _) -> (match typeconv ty2 with
                      | Tpointer (ty, _) -> Add_case_lp ty
                      | _ -> Add_default)
  | Tpointer (ty, _) ->
    (match typeconv ty2 with
     | Tint0 (_, _, _) -> Add_case_pi ty
     | Tlong0 (_, _) -> Add_case_pl ty
     | _ -> Add_default)
  | _ -> Add_default

(** val sem_add : composite_env -> val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_add cenv v1 t1 v2 t2 m =
  match classify_add t1 t2 with
  | Add_case_pi ty ->
    (match v1 with
     | Vint n1 -> (match v2 with
                   | Vint n2 -> Some (Vint (Int.add n1 (Int.mul (Int.repr (sizeof cenv ty)) n2)))
                   | _ -> None)
     | Vptr (b1, ofs1) ->
       (match v2 with
        | Vint n2 -> Some (Vptr (b1, (Int.add ofs1 (Int.mul (Int.repr (sizeof cenv ty)) n2))))
        | _ -> None)
     | _ -> None)
  | Add_case_ip ty ->
    (match v1 with
     | Vint n1 ->
       (match v2 with
        | Vint n2 -> Some (Vint (Int.add n2 (Int.mul (Int.repr (sizeof cenv ty)) n1)))
        | Vptr (b2, ofs2) -> Some (Vptr (b2, (Int.add ofs2 (Int.mul (Int.repr (sizeof cenv ty)) n1))))
        | _ -> None)
     | _ -> None)
  | Add_case_pl ty ->
    (match v1 with
     | Vint n1 ->
       (match v2 with
        | Vlong n2 -> let n3 = Int.repr (Int64.unsigned n2) in Some (Vint (Int.add n1 (Int.mul (Int.repr (sizeof cenv ty)) n3)))
        | _ -> None)
     | Vptr (b1, ofs1) ->
       (match v2 with
        | Vlong n2 ->
          let n3 = Int.repr (Int64.unsigned n2) in Some (Vptr (b1, (Int.add ofs1 (Int.mul (Int.repr (sizeof cenv ty)) n3))))
        | _ -> None)
     | _ -> None)
  | Add_case_lp ty ->
    (match v1 with
     | Vlong n1 ->
       (match v2 with
        | Vint n2 -> let n3 = Int.repr (Int64.unsigned n1) in Some (Vint (Int.add n2 (Int.mul (Int.repr (sizeof cenv ty)) n3)))
        | Vptr (b2, ofs2) ->
          let n2 = Int.repr (Int64.unsigned n1) in Some (Vptr (b2, (Int.add ofs2 (Int.mul (Int.repr (sizeof cenv ty)) n2))))
        | _ -> None)
     | _ -> None)
  | Add_default ->
    sem_binarith (fun _ n1 n2 -> Some (Vint (Int.add n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.add n1 n2))) (fun n1 n2 -> Some
      (Vfloat (Float.add n1 n2))) (fun n1 n2 -> Some (Vsingle (Float32.add n1 n2))) v1 t1 v2 t2 m

type classify_sub_cases =
| Sub_case_pi of type0
| Sub_case_pp of type0
| Sub_case_pl of type0
| Sub_default

(** val classify_sub : type0 -> type0 -> classify_sub_cases **)

let classify_sub ty1 ty2 =
  match typeconv ty1 with
  | Tpointer (ty, _) ->
    (match typeconv ty2 with
     | Tint0 (_, _, _) -> Sub_case_pi ty
     | Tlong0 (_, _) -> Sub_case_pl ty
     | Tpointer (_, _) -> Sub_case_pp ty
     | _ -> Sub_default)
  | _ -> Sub_default

(** val sem_sub : composite_env -> val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_sub cenv v1 t1 v2 t2 m =
  match classify_sub t1 t2 with
  | Sub_case_pi ty ->
    (match v1 with
     | Vint n1 -> (match v2 with
                   | Vint n2 -> Some (Vint (Int.sub n1 (Int.mul (Int.repr (sizeof cenv ty)) n2)))
                   | _ -> None)
     | Vptr (b1, ofs1) ->
       (match v2 with
        | Vint n2 -> Some (Vptr (b1, (Int.sub ofs1 (Int.mul (Int.repr (sizeof cenv ty)) n2))))
        | _ -> None)
     | _ -> None)
  | Sub_case_pp ty ->
    (match v1 with
     | Vptr (b1, ofs1) ->
       (match v2 with
        | Vptr (b2, ofs2) ->
          if eq_block b1 b2
          then let sz = sizeof cenv ty in
               if (&&) ((fun x -> x) (zlt Z0 sz)) ((fun x -> x) (zle sz Int.max_signed))
               then Some (Vint (Int.divs (Int.sub ofs1 ofs2) (Int.repr sz)))
               else None
          else None
        | _ -> None)
     | _ -> None)
  | Sub_case_pl ty ->
    (match v1 with
     | Vint n1 ->
       (match v2 with
        | Vlong n2 -> let n3 = Int.repr (Int64.unsigned n2) in Some (Vint (Int.sub n1 (Int.mul (Int.repr (sizeof cenv ty)) n3)))
        | _ -> None)
     | Vptr (b1, ofs1) ->
       (match v2 with
        | Vlong n2 ->
          let n3 = Int.repr (Int64.unsigned n2) in Some (Vptr (b1, (Int.sub ofs1 (Int.mul (Int.repr (sizeof cenv ty)) n3))))
        | _ -> None)
     | _ -> None)
  | Sub_default ->
    sem_binarith (fun _ n1 n2 -> Some (Vint (Int.sub n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.sub n1 n2))) (fun n1 n2 -> Some
      (Vfloat (Float.sub n1 n2))) (fun n1 n2 -> Some (Vsingle (Float32.sub n1 n2))) v1 t1 v2 t2 m

(** val sem_mul : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_mul v1 t1 v2 t2 m =
  sem_binarith (fun _ n1 n2 -> Some (Vint (Int.mul n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.mul n1 n2))) (fun n1 n2 -> Some
    (Vfloat (Float.mul n1 n2))) (fun n1 n2 -> Some (Vsingle (Float32.mul n1 n2))) v1 t1 v2 t2 m

(** val sem_div : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_div v1 t1 v2 t2 m =
  sem_binarith (fun sg n1 n2 ->
    match sg with
    | Signed ->
      if (||) (Int.eq n2 Int.zero) ((&&) (Int.eq n1 (Int.repr Int.min_signed)) (Int.eq n2 Int.mone))
      then None
      else Some (Vint (Int.divs n1 n2))
    | Unsigned -> if Int.eq n2 Int.zero then None else Some (Vint (Int.divu n1 n2))) (fun sg n1 n2 ->
    match sg with
    | Signed ->
      if (||) (Int64.eq n2 Int64.zero) ((&&) (Int64.eq n1 (Int64.repr Int64.min_signed)) (Int64.eq n2 Int64.mone))
      then None
      else Some (Vlong (Int64.divs n1 n2))
    | Unsigned -> if Int64.eq n2 Int64.zero then None else Some (Vlong (Int64.divu n1 n2))) (fun n1 n2 -> Some (Vfloat
    (Float.div n1 n2))) (fun n1 n2 -> Some (Vsingle (Float32.div n1 n2))) v1 t1 v2 t2 m

(** val sem_mod : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_mod v1 t1 v2 t2 m =
  sem_binarith (fun sg n1 n2 ->
    match sg with
    | Signed ->
      if (||) (Int.eq n2 Int.zero) ((&&) (Int.eq n1 (Int.repr Int.min_signed)) (Int.eq n2 Int.mone))
      then None
      else Some (Vint (Int.mods n1 n2))
    | Unsigned -> if Int.eq n2 Int.zero then None else Some (Vint (Int.modu n1 n2))) (fun sg n1 n2 ->
    match sg with
    | Signed ->
      if (||) (Int64.eq n2 Int64.zero) ((&&) (Int64.eq n1 (Int64.repr Int64.min_signed)) (Int64.eq n2 Int64.mone))
      then None
      else Some (Vlong (Int64.mods n1 n2))
    | Unsigned -> if Int64.eq n2 Int64.zero then None else Some (Vlong (Int64.modu n1 n2))) (fun _ _ -> None) (fun _ _ -> None)
    v1 t1 v2 t2 m

(** val sem_and : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_and v1 t1 v2 t2 m =
  sem_binarith (fun _ n1 n2 -> Some (Vint (Int.coq_and n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.coq_and n1 n2))) (fun _ _ ->
    None) (fun _ _ -> None) v1 t1 v2 t2 m

(** val sem_or : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_or v1 t1 v2 t2 m =
  sem_binarith (fun _ n1 n2 -> Some (Vint (Int.coq_or n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.coq_or n1 n2))) (fun _ _ ->
    None) (fun _ _ -> None) v1 t1 v2 t2 m

(** val sem_xor : val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_xor v1 t1 v2 t2 m =
  sem_binarith (fun _ n1 n2 -> Some (Vint (Int.xor n1 n2))) (fun _ n1 n2 -> Some (Vlong (Int64.xor n1 n2))) (fun _ _ -> None)
    (fun _ _ -> None) v1 t1 v2 t2 m

type classify_shift_cases =
| Shift_case_ii of signedness
| Shift_case_ll of signedness
| Shift_case_il of signedness
| Shift_case_li of signedness
| Shift_default

(** val classify_shift : type0 -> type0 -> classify_shift_cases **)

let classify_shift ty1 ty2 =
  match typeconv ty1 with
  | Tint0 (i, s, _) ->
    (match i with
     | I32 -> (match typeconv ty2 with
               | Tint0 (_, _, _) -> Shift_case_ii s
               | Tlong0 (_, _) -> Shift_case_il s
               | _ -> Shift_default)
     | _ ->
       (match typeconv ty2 with
        | Tint0 (_, _, _) -> Shift_case_ii Signed
        | Tlong0 (_, _) -> Shift_case_il Signed
        | _ -> Shift_default))
  | Tlong0 (s, _) ->
    (match typeconv ty2 with
     | Tint0 (_, _, _) -> Shift_case_li s
     | Tlong0 (_, _) -> Shift_case_ll s
     | _ -> Shift_default)
  | _ -> Shift_default

(** val sem_shift :
    (signedness -> Int.int -> Int.int -> Int.int) -> (signedness -> Int64.int -> Int64.int -> Int64.int) -> val0 -> type0 -> val0
    -> type0 -> val0 option **)

let sem_shift sem_int sem_long v1 t1 v2 t2 =
  match classify_shift t1 t2 with
  | Shift_case_ii sg ->
    (match v1 with
     | Vint n1 -> (match v2 with
                   | Vint n2 -> if Int.ltu n2 Int.iwordsize then Some (Vint (sem_int sg n1 n2)) else None
                   | _ -> None)
     | _ -> None)
  | Shift_case_ll sg ->
    (match v1 with
     | Vlong n1 ->
       (match v2 with
        | Vlong n2 -> if Int64.ltu n2 Int64.iwordsize then Some (Vlong (sem_long sg n1 n2)) else None
        | _ -> None)
     | _ -> None)
  | Shift_case_il sg ->
    (match v1 with
     | Vint n1 ->
       (match v2 with
        | Vlong n2 ->
          if Int64.ltu n2 (Int64.repr (Zpos (XO (XO (XO (XO (XO XH)))))))
          then Some (Vint (sem_int sg n1 (Int64.loword n2)))
          else None
        | _ -> None)
     | _ -> None)
  | Shift_case_li sg ->
    (match v1 with
     | Vlong n1 ->
       (match v2 with
        | Vint n2 -> if Int.ltu n2 Int64.iwordsize' then Some (Vlong (sem_long sg n1 (Int64.repr (Int.unsigned n2)))) else None
        | _ -> None)
     | _ -> None)
  | Shift_default -> None

(** val sem_shl : val0 -> type0 -> val0 -> type0 -> val0 option **)

let sem_shl v1 t1 v2 t2 =
  sem_shift (fun _ -> Int.shl) (fun _ -> Int64.shl) v1 t1 v2 t2

(** val sem_shr : val0 -> type0 -> val0 -> type0 -> val0 option **)

let sem_shr v1 t1 v2 t2 =
  sem_shift (fun sg n1 n2 -> match sg with
                             | Signed -> Int.shr n1 n2
                             | Unsigned -> Int.shru n1 n2) (fun sg n1 n2 ->
    match sg with
    | Signed -> Int64.shr n1 n2
    | Unsigned -> Int64.shru n1 n2) v1 t1 v2 t2

type classify_cmp_cases =
| Cmp_case_pp
| Cmp_case_pl
| Cmp_case_lp
| Cmp_default

(** val classify_cmp : type0 -> type0 -> classify_cmp_cases **)

let classify_cmp ty1 ty2 =
  match typeconv ty1 with
  | Tint0 (_, _, _) -> (match typeconv ty2 with
                        | Tpointer (_, _) -> Cmp_case_pp
                        | _ -> Cmp_default)
  | Tlong0 (_, _) -> (match typeconv ty2 with
                      | Tpointer (_, _) -> Cmp_case_lp
                      | _ -> Cmp_default)
  | Tpointer (_, _) ->
    (match typeconv ty2 with
     | Tint0 (_, _, _) -> Cmp_case_pp
     | Tlong0 (_, _) -> Cmp_case_pl
     | Tpointer (_, _) -> Cmp_case_pp
     | _ -> Cmp_default)
  | _ -> Cmp_default

(** val sem_cmp : comparison0 -> val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_cmp c v1 t1 v2 t2 m =
  match classify_cmp t1 t2 with
  | Cmp_case_pp -> option_map Val.of_bool (Val.cmpu_bool (Mem.valid_pointer m) c v1 v2)
  | Cmp_case_pl ->
    (match v2 with
     | Vlong n2 ->
       let n3 = Int.repr (Int64.unsigned n2) in option_map Val.of_bool (Val.cmpu_bool (Mem.valid_pointer m) c v1 (Vint n3))
     | _ -> None)
  | Cmp_case_lp ->
    (match v1 with
     | Vlong n1 ->
       let n2 = Int.repr (Int64.unsigned n1) in option_map Val.of_bool (Val.cmpu_bool (Mem.valid_pointer m) c (Vint n2) v2)
     | _ -> None)
  | Cmp_default ->
    sem_binarith (fun sg n1 n2 -> Some (Val.of_bool (match sg with
                                                     | Signed -> Int.cmp c n1 n2
                                                     | Unsigned -> Int.cmpu c n1 n2))) (fun sg n1 n2 -> Some
      (Val.of_bool (match sg with
                    | Signed -> Int64.cmp c n1 n2
                    | Unsigned -> Int64.cmpu c n1 n2))) (fun n1 n2 -> Some (Val.of_bool (Float.cmp c n1 n2))) (fun n1 n2 -> Some
      (Val.of_bool (Float32.cmp c n1 n2))) v1 t1 v2 t2 m

type classify_fun_cases =
| Fun_case_f of typelist * type0 * calling_convention
| Fun_default

(** val classify_fun : type0 -> classify_fun_cases **)

let classify_fun = function
| Tpointer (t0, _) -> (match t0 with
                       | Tfunction (args, res0, cc) -> Fun_case_f (args, res0, cc)
                       | _ -> Fun_default)
| Tfunction (args, res0, cc) -> Fun_case_f (args, res0, cc)
| _ -> Fun_default

type classify_switch_cases =
| Switch_case_i
| Switch_case_l
| Switch_default

(** val classify_switch : type0 -> classify_switch_cases **)

let classify_switch = function
| Tint0 (_, _, _) -> Switch_case_i
| Tlong0 (_, _) -> Switch_case_l
| _ -> Switch_default

(** val sem_switch_arg : val0 -> type0 -> z option **)

let sem_switch_arg v ty =
  match classify_switch ty with
  | Switch_case_i -> (match v with
                      | Vint n0 -> Some (Int.unsigned n0)
                      | _ -> None)
  | Switch_case_l -> (match v with
                      | Vlong n0 -> Some (Int64.unsigned n0)
                      | _ -> None)
  | Switch_default -> None

(** val sem_unary_operation : unary_operation -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_unary_operation op v ty m =
  match op with
  | Onotbool -> sem_notbool v ty m
  | Onotint -> sem_notint v ty
  | Oneg -> sem_neg v ty
  | Oabsfloat -> sem_absfloat v ty

(** val sem_binary_operation : composite_env -> binary_operation -> val0 -> type0 -> val0 -> type0 -> Mem.mem -> val0 option **)

let sem_binary_operation cenv op v1 t1 v2 t2 m =
  match op with
  | Oadd -> sem_add cenv v1 t1 v2 t2 m
  | Osub -> sem_sub cenv v1 t1 v2 t2 m
  | Omul -> sem_mul v1 t1 v2 t2 m
  | Odiv -> sem_div v1 t1 v2 t2 m
  | Omod -> sem_mod v1 t1 v2 t2 m
  | Oand -> sem_and v1 t1 v2 t2 m
  | Oor -> sem_or v1 t1 v2 t2 m
  | Oxor -> sem_xor v1 t1 v2 t2 m
  | Oshl -> sem_shl v1 t1 v2 t2
  | Oshr -> sem_shr v1 t1 v2 t2
  | Oeq -> sem_cmp Ceq v1 t1 v2 t2 m
  | One -> sem_cmp Cne v1 t1 v2 t2 m
  | Olt -> sem_cmp Clt v1 t1 v2 t2 m
  | Ogt -> sem_cmp Cgt v1 t1 v2 t2 m
  | Ole -> sem_cmp Cle v1 t1 v2 t2 m
  | Oge -> sem_cmp Cge v1 t1 v2 t2 m

(** val store_zeros : Mem.mem -> block -> z -> z -> Mem.mem option **)

let rec store_zeros m b p n0 =
  if zle n0 Z0
  then Some m
  else (match Mem.store Mint8unsigned m b p vzero with
        | Some m' -> store_zeros m' b (Z.add p (Zpos XH)) (Z.sub n0 (Zpos XH))
        | None -> None)

module Genv =
 struct
  type ('f, 'v) t = { genv_public : ident0 list; genv_symb : block PTree.t; genv_defs : ('f, 'v) globdef PTree.t;
                      genv_next : block }

  (** val genv_public : ('a1, 'a2) t -> ident0 list **)

  let genv_public x = x.genv_public

  (** val genv_symb : ('a1, 'a2) t -> block PTree.t **)

  let genv_symb x = x.genv_symb

  (** val genv_defs : ('a1, 'a2) t -> ('a1, 'a2) globdef PTree.t **)

  let genv_defs x = x.genv_defs

  (** val genv_next : ('a1, 'a2) t -> block **)

  let genv_next x = x.genv_next

  (** val find_symbol : ('a1, 'a2) t -> ident0 -> block option **)

  let find_symbol ge id =
    PTree.get id ge.genv_symb

  (** val find_def : ('a1, 'a2) t -> block -> ('a1, 'a2) globdef option **)

  let find_def ge b =
    PTree.get b ge.genv_defs

  (** val find_funct_ptr : ('a1, 'a2) t -> block -> 'a1 option **)

  let find_funct_ptr ge b =
    match find_def ge b with
    | Some g -> (match g with
                 | Gfun f -> Some f
                 | Gvar _ -> None)
    | None -> None

  (** val find_funct : ('a1, 'a2) t -> val0 -> 'a1 option **)

  let find_funct ge = function
  | Vptr (b, ofs) -> if Int.eq_dec ofs Int.zero then find_funct_ptr ge b else None
  | _ -> None

  (** val add_global : ('a1, 'a2) t -> (ident0 * ('a1, 'a2) globdef) -> ('a1, 'a2) t **)

  let add_global ge idg =
    { genv_public = ge.genv_public; genv_symb = (PTree.set (fst idg) ge.genv_next ge.genv_symb); genv_defs =
      (PTree.set ge.genv_next (snd idg) ge.genv_defs); genv_next = (Coq_Pos.succ ge.genv_next) }

  (** val add_globals : ('a1, 'a2) t -> (ident0 * ('a1, 'a2) globdef) list -> ('a1, 'a2) t **)

  let add_globals ge gl =
    fold_left add_global gl ge

  (** val empty_genv : ident0 list -> ('a1, 'a2) t **)

  let empty_genv pub =
    { genv_public = pub; genv_symb = PTree.empty; genv_defs = PTree.empty; genv_next = XH }

  (** val globalenv : ('a1, 'a2) program -> ('a1, 'a2) t **)

  let globalenv p =
    add_globals (empty_genv p.prog_public) p.prog_defs

  (** val store_init_data : ('a1, 'a2) t -> Mem.mem -> block -> z -> init_data -> Mem.mem option **)

  let store_init_data ge m b p = function
  | Init_int8 n0 -> Mem.store Mint8unsigned m b p (Vint n0)
  | Init_int16 n0 -> Mem.store Mint16unsigned m b p (Vint n0)
  | Init_int32 n0 -> Mem.store Mint32 m b p (Vint n0)
  | Init_int64 n0 -> Mem.store Mint64 m b p (Vlong n0)
  | Init_float32 n0 -> Mem.store Mfloat32 m b p (Vsingle n0)
  | Init_float64 n0 -> Mem.store Mfloat64 m b p (Vfloat n0)
  | Init_space _ -> Some m
  | Init_addrof (symb, ofs) -> (match find_symbol ge symb with
                                | Some b' -> Mem.store Mint32 m b p (Vptr (b', ofs))
                                | None -> None)

  (** val store_init_data_list : ('a1, 'a2) t -> Mem.mem -> block -> z -> init_data list -> Mem.mem option **)

  let rec store_init_data_list ge m b p = function
  | [] -> Some m
  | id :: idl' ->
    (match store_init_data ge m b p id with
     | Some m' -> store_init_data_list ge m' b (Z.add p (init_data_size id)) idl'
     | None -> None)

  (** val perm_globvar : 'a1 globvar -> permission **)

  let perm_globvar gv =
    if gv.gvar_volatile then Nonempty else if gv.gvar_readonly then Readable else Writable

  (** val alloc_global : ('a1, 'a2) t -> Mem.mem -> (ident0 * ('a1, 'a2) globdef) -> Mem.mem option **)

  let alloc_global ge m = function
  | (_, g) ->
    (match g with
     | Gfun _ -> let (m1, b) = Mem.alloc m Z0 (Zpos XH) in Mem.drop_perm m1 b Z0 (Zpos XH) Nonempty
     | Gvar v ->
       let init0 = v.gvar_init in
       let sz = init_data_list_size init0 in
       let (m1, b) = Mem.alloc m Z0 sz in
       (match store_zeros m1 b Z0 sz with
        | Some m2 ->
          (match store_init_data_list ge m2 b Z0 init0 with
           | Some m3 -> Mem.drop_perm m3 b Z0 sz (perm_globvar v)
           | None -> None)
        | None -> None))

  (** val alloc_globals : ('a1, 'a2) t -> Mem.mem -> (ident0 * ('a1, 'a2) globdef) list -> Mem.mem option **)

  let rec alloc_globals ge m = function
  | [] -> Some m
  | g :: gl' -> (match alloc_global ge m g with
                 | Some m' -> alloc_globals ge m' gl'
                 | None -> None)

  (** val init_mem : ('a1, 'a2) program -> Mem.mem option **)

  let init_mem p =
    alloc_globals (globalenv p) Mem.empty p.prog_defs
 end

type expr =
| Econst_int of Int.int * type0
| Econst_float of float * type0
| Econst_single of float32 * type0
| Econst_long of Int64.int * type0
| Evar of ident0 * type0
| Etempvar of ident0 * type0
| Ederef of expr * type0
| Eaddrof of expr * type0
| Eunop of unary_operation * expr * type0
| Ebinop of binary_operation * expr * expr * type0
| Ecast of expr * type0
| Efield of expr * ident0 * type0
| Esizeof of type0 * type0
| Ealignof of type0 * type0

(** val typeof : expr -> type0 **)

let typeof = function
| Econst_int (_, ty) -> ty
| Econst_float (_, ty) -> ty
| Econst_single (_, ty) -> ty
| Econst_long (_, ty) -> ty
| Evar (_, ty) -> ty
| Etempvar (_, ty) -> ty
| Ederef (_, ty) -> ty
| Eaddrof (_, ty) -> ty
| Eunop (_, _, ty) -> ty
| Ebinop (_, _, _, ty) -> ty
| Ecast (_, ty) -> ty
| Efield (_, _, ty) -> ty
| Esizeof (_, ty) -> ty
| Ealignof (_, ty) -> ty

type label = ident0

type statement =
| Sskip
| Sassign of expr * expr
| Sset of ident0 * expr
| Scall of ident0 option * expr * expr list
| Sbuiltin of ident0 option * external_function * typelist * expr list
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Sloop of statement * statement
| Sbreak
| Scontinue
| Sreturn of expr option
| Sswitch of expr * labeled_statements
| Slabel of label * statement
| Sgoto of label
and labeled_statements =
| LSnil
| LScons of z option * statement * labeled_statements

type function0 = { fn_return : type0; fn_callconv : calling_convention; fn_params : (ident0 * type0) list;
                   fn_vars : (ident0 * type0) list; fn_temps : (ident0 * type0) list; fn_body : statement }

(** val fn_return : function0 -> type0 **)

let fn_return x = x.fn_return

(** val fn_callconv : function0 -> calling_convention **)

let fn_callconv x = x.fn_callconv

(** val fn_params : function0 -> (ident0 * type0) list **)

let fn_params x = x.fn_params

(** val fn_vars : function0 -> (ident0 * type0) list **)

let fn_vars x = x.fn_vars

(** val fn_temps : function0 -> (ident0 * type0) list **)

let fn_temps x = x.fn_temps

(** val fn_body : function0 -> statement **)

let fn_body x = x.fn_body

type fundef0 = function0 fundef

(** val type_of_function : function0 -> type0 **)

let type_of_function f =
  Tfunction ((type_of_params f.fn_params), f.fn_return, f.fn_callconv)

(** val type_of_fundef : fundef0 -> type0 **)

let type_of_fundef = function
| Internal fd -> type_of_function fd
| External (_, args, res0, cc) -> Tfunction (args, res0, cc)

type program1 = function0 program0

type genv = { genv_genv : (fundef0, type0) Genv.t; genv_cenv : composite_env }

(** val genv_genv : genv -> (fundef0, type0) Genv.t **)

let genv_genv x = x.genv_genv

(** val genv_cenv : genv -> composite_env **)

let genv_cenv x = x.genv_cenv

(** val globalenv0 : program1 -> genv **)

let globalenv0 p =
  { genv_genv = (Genv.globalenv (program_of_program p)); genv_cenv = p.prog_comp_env }

type env = (block * type0) PTree.t

(** val empty_env : env **)

let empty_env =
  PTree.empty

type temp_env = val0 PTree.t

(** val create_undef_temps : (ident0 * type0) list -> temp_env **)

let rec create_undef_temps = function
| [] -> PTree.empty
| p :: temps' -> let (id, _) = p in PTree.set id Vundef (create_undef_temps temps')

(** val bind_parameter_temps : (ident0 * type0) list -> val0 list -> temp_env -> temp_env option **)

let rec bind_parameter_temps formals args le =
  match formals with
  | [] -> (match args with
           | [] -> Some le
           | _ :: _ -> None)
  | p :: xl -> let (id, _) = p in (match args with
                                   | [] -> None
                                   | v :: vl -> bind_parameter_temps xl vl (PTree.set id v le))

(** val block_of_binding : genv -> (ident0 * (block * type0)) -> (block * z) * z **)

let block_of_binding ge = function
| (_, p) -> let (b, ty) = p in ((b, Z0), (sizeof ge.genv_cenv ty))

(** val blocks_of_env : genv -> env -> ((block * z) * z) list **)

let blocks_of_env ge e =
  map (block_of_binding ge) (PTree.elements e)

(** val set_opttemp : ident0 option -> val0 -> temp_env -> val0 PTree.t **)

let set_opttemp optid v le =
  match optid with
  | Some id -> PTree.set id v le
  | None -> le

(** val select_switch_default : labeled_statements -> labeled_statements **)

let rec select_switch_default sl = match sl with
| LSnil -> sl
| LScons (o, _, sl') -> (match o with
                         | Some _ -> select_switch_default sl'
                         | None -> sl)

(** val select_switch_case : z -> labeled_statements -> labeled_statements option **)

let rec select_switch_case n0 sl = match sl with
| LSnil -> None
| LScons (o, _, sl') ->
  (match o with
   | Some c -> if zeq c n0 then Some sl else select_switch_case n0 sl'
   | None -> select_switch_case n0 sl')

(** val select_switch : z -> labeled_statements -> labeled_statements **)

let select_switch n0 sl =
  match select_switch_case n0 sl with
  | Some sl' -> sl'
  | None -> select_switch_default sl

(** val seq_of_labeled_statement : labeled_statements -> statement **)

let rec seq_of_labeled_statement = function
| LSnil -> Sskip
| LScons (_, s, sl') -> Ssequence (s, (seq_of_labeled_statement sl'))

type cont =
| Kstop
| Kseq of statement * cont
| Kloop1 of statement * statement * cont
| Kloop2 of statement * statement * cont
| Kswitch of cont
| Kcall of ident0 option * function0 * env * temp_env * cont

(** val call_cont : cont -> cont **)

let rec call_cont k = match k with
| Kseq (_, k0) -> call_cont k0
| Kloop1 (_, _, k0) -> call_cont k0
| Kloop2 (_, _, k0) -> call_cont k0
| Kswitch k0 -> call_cont k0
| _ -> k

type state =
| State of function0 * statement * cont * env * temp_env * Mem.mem
| Callstate of fundef0 * val0 list * cont * Mem.mem
| Returnstate of val0 * cont * Mem.mem

(** val find_label : label -> statement -> cont -> (statement * cont) option **)

let rec find_label lbl s k =
  match s with
  | Ssequence (s1, s2) -> (match find_label lbl s1 (Kseq (s2, k)) with
                           | Some sk -> Some sk
                           | None -> find_label lbl s2 k)
  | Sifthenelse (_, s1, s2) -> (match find_label lbl s1 k with
                                | Some sk -> Some sk
                                | None -> find_label lbl s2 k)
  | Sloop (s1, s2) ->
    (match find_label lbl s1 (Kloop1 (s1, s2, k)) with
     | Some sk -> Some sk
     | None -> find_label lbl s2 (Kloop2 (s1, s2, k)))
  | Sswitch (_, sl) -> find_label_ls lbl sl (Kswitch k)
  | Slabel (lbl', s') -> if ident_eq lbl lbl' then Some (s', k) else find_label lbl s' k
  | _ -> None

(** val find_label_ls : label -> labeled_statements -> cont -> (statement * cont) option **)

and find_label_ls lbl sl k =
  match sl with
  | LSnil -> None
  | LScons (_, s, sl') ->
    (match find_label lbl s (Kseq ((seq_of_labeled_statement sl'), k)) with
     | Some sk -> Some sk
     | None -> find_label_ls lbl sl' k)

(** val print : char list -> unit **)

let print =  (fun l ->      let implode l =      let result = String.create (List.length l) in       let rec imp i = function        | [] -> result        | c :: l -> result.[i] <- c; imp (i + 1) l in       imp 0 l in      print_string (implode l))

(** val memcpy_check_args : z -> z -> block -> z -> block -> z -> bool **)

let memcpy_check_args sz al bdst odst bsrc osrc =
  let x =
    let s = zeq al (Zpos XH) in
    if s
    then true
    else let s0 = zeq al (Zpos (XO XH)) in
         if s0 then true else let s1 = zeq al (Zpos (XO (XO XH))) in if s1 then true else zeq al (Zpos (XO (XO (XO XH))))
  in
  if x
  then let s = zle Z0 sz in
       if s
       then let s0 = zdivide_dec al sz in
            if s0
            then let u = fun x0 -> let s1 = zeq sz Z0 in if s1 then true else zdivide_dec al x0 in
                 let s1 = u osrc in
                 if s1
                 then let s2 = u odst in
                      if s2
                      then let s3 = eq_block bsrc bdst in
                           if s3
                           then let s4 = zeq osrc odst in
                                if s4
                                then true
                                else let s5 = zle (Z.add osrc sz) odst in if s5 then true else zle (Z.add odst sz) osrc
                           else true
                      else false
                 else false
            else false
       else false
  else false

module Coq_M = PTree

(** val show_positive' : positive -> char list -> char list **)

let rec show_positive' p acc =
  match p with
  | XI p' -> show_positive' p' (append ('1'::[]) acc)
  | XO p' -> show_positive' p' (append ('0'::[]) acc)
  | XH -> append ('1'::[]) acc

(** val show_positive : positive -> char list **)

let show_positive p =
  show_positive' p []

(** val show_var : positive -> name Coq_M.t -> char list **)

let show_var x nenv =
  match Coq_M.get x nenv with
  | Some y ->
    (match y with
     | NAnon -> append ('x'::[]) (show_positive x)
     | NNamed s -> append s (append ('_'::[]) (show_positive x)))
  | None -> append ('x'::[]) (show_positive x)

(** val print_errcode : name Coq_M.t option -> errcode -> char list **)

let print_errcode on = function
| MSG err -> err
| CTX id ->
  (match on with
   | Some nenv -> show_var id nenv
   | None -> append ('c'::('t'::('x'::('_'::('n'::('o'::('E'::('n'::('v'::[]))))))))) (show_positive id))
| POS p -> show_positive p

(** val print_error : errmsg -> name Coq_M.t option -> char list **)

let rec print_error errs os =
  let errstr = map (print_errcode os) errs in fold_left append errstr []

(** val force_opt : 'a1 option -> errmsg -> 'a1 res **)

let force_opt v msg =
  match v with
  | Some x -> OK x
  | None -> Error0 msg

(** val compos : genv -> positive -> composite res **)

let compos ge id =
  match PTree.get id ge.genv_cenv with
  | Some x -> OK x
  | None ->
    Error0 ((MSG ('I'::('d'::('e'::('n'::('t'::('i'::('f'::('i'::('e'::('r'::(' '::[])))))))))))) :: ((CTX id) :: ((MSG
      (' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::[])))))))))))))))))))))))))) :: [])))

(** val eval_field : genv -> type0 -> ident0 -> val0 -> (block * Int.int) res **)

let eval_field ge ty fld = function
| Vptr (l, ofs) ->
  (match ty with
   | Tstruct (id, _) ->
     bind (compos ge id) (fun co ->
       bind (field_offset ge.genv_cenv fld co.co_members) (fun delta -> OK (l, (Int.add ofs (Int.repr delta)))))
   | Tunion (id, _) -> bind (compos ge id) (fun _ -> OK (l, ofs))
   | _ ->
     Error0 ((MSG
       ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('a'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::[]))))))))))))))))))))))))))) :: []))
| _ ->
  Error0 ((MSG
    ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::(' '::('o'::('n'::(' '::('a'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))) :: [])

(** val eval_var : genv -> env -> ident0 -> type0 -> (block * Int.int) res **)

let eval_var ge ve id ty =
  match PTree.get id ve with
  | Some p ->
    let (b, ty') = p in
    if type_eq ty ty'
    then OK (b, Int.zero)
    else Error0 ((MSG
           ('T'::('y'::('p'::('e'::('-'::('c'::('h'::('e'::('c'::('k'::(' '::('f'::('a'::('i'::('l'::('u'::('r'::('e'::(' '::('i'::('n'::(' '::('e'::('v'::('a'::('l'::('_'::('v'::('a'::('r'::[]))))))))))))))))))))))))))))))) :: [])
  | None ->
    (match Genv.find_symbol ge.genv_genv id with
     | Some b -> OK (b, Int.zero)
     | None ->
       Error0 ((MSG ('V'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::[])))))))))) :: ((CTX id) :: ((MSG
         (' '::('n'::('o'::('t'::(' '::('i'::('n'::(' '::('s'::('c'::('o'::('p'::('e'::[])))))))))))))) :: []))))

(** val deref_loc : Mem.mem -> type0 -> block -> Int.int -> val0 res **)

let deref_loc m ty b ofs =
  match access_mode ty with
  | By_value chunk ->
    (match Mem.loadv chunk m (Vptr (b, ofs)) with
     | Some v -> OK v
     | None ->
       Error0 ((MSG
         ('D'::('e'::('r'::('e'::('f'::('_'::('l'::('o'::('c'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(','::(' '::('n'::('o'::(' '::('v'::('a'::('l'::('u'::('e'::(':'::('c'::('h'::('u'::('n'::('k'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::(' '::('m'::('e'::('m'::(' '::('a'::('t'::(' '::('l'::('o'::('c'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))) :: []))
  | By_nothing ->
    Error0 ((MSG
      ('D'::('e'::('r'::('e'::('f'::('_'::('l'::('o'::('c'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::(','::(' '::('A'::('c'::('c'::('e'::('s'::('s'::(' '::('m'::('o'::('d'::('e'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::[])))))))))))))))))))))))))))))))))))))) :: [])
  | _ -> OK (Vptr (b, ofs))

(** val eval_expr : genv -> env -> temp_env -> Mem.mem -> expr -> val0 res **)

let eval_expr ge ve le m =
  let rec eval_expr0 a0 = match a0 with
  | Econst_int (i, _) -> OK (Vint i)
  | Econst_float (f, _) -> OK (Vfloat f)
  | Econst_single (f, _) -> OK (Vsingle f)
  | Econst_long (i, _) -> OK (Vlong i)
  | Evar (id, ty) -> bind2 (eval_var ge ve id ty) (fun l ofs -> deref_loc m (typeof a0) l ofs)
  | Etempvar (id, _) ->
    force_opt (PTree.get id le) ((MSG ('T'::('e'::('m'::('p'::('v'::('a'::('r'::(' '::[]))))))))) :: ((CTX id) :: ((MSG
      (' '::('n'::('o'::('t'::(' '::('i'::('n'::(' '::('s'::('c'::('o'::('p'::('e'::[])))))))))))))) :: [])))
  | Ederef (a, _) ->
    (match eval_expr0 a with
     | OK v ->
       (match v with
        | Vptr (l, ofs) -> deref_loc m (typeof a0) l ofs
        | _ ->
          Error0 ((MSG
            ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
     | Error0 _ ->
       Error0 ((MSG
         ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
  | Eaddrof (a, _) -> bind2 (eval_lvalue0 a) (fun l ofs -> OK (Vptr (l, ofs)))
  | Eunop (op, a, _) ->
    bind (eval_expr0 a) (fun v ->
      force_opt (sem_unary_operation op v (typeof a) m) ((MSG
        ('U'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))) :: []))
  | Ebinop (op, a1, a2, _) ->
    bind (eval_expr0 a1) (fun v1 ->
      bind (eval_expr0 a2) (fun v2 ->
        force_opt (sem_binary_operation ge.genv_cenv op v1 (typeof a1) v2 (typeof a2) m) ((MSG
          ('B'::('i'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))))))) :: [])))
  | Ecast (a, ty) ->
    bind (eval_expr0 a) (fun v ->
      force_opt (sem_cast v (typeof a) ty m) ((MSG
        ('C'::('a'::('s'::('t'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))) :: []))
  | Efield (a, i, _) ->
    bind (eval_expr0 a) (fun v -> bind2 (eval_field ge (typeof a) i v) (fun l ofs -> deref_loc m (typeof a0) l ofs))
  | Esizeof (t0, _) -> OK (Vint (Int.repr (sizeof ge.genv_cenv t0)))
  | Ealignof (t0, _) -> OK (Vint (Int.repr (alignof ge.genv_cenv t0)))
  and eval_lvalue0 = function
  | Evar (id, ty) -> eval_var ge ve id ty
  | Ederef (a, _) ->
    (match eval_expr0 a with
     | OK v ->
       (match v with
        | Vptr (l, ofs) -> OK (l, ofs)
        | _ ->
          Error0 ((MSG
            ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
     | Error0 _ ->
       Error0 ((MSG
         ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
  | Efield (a, i, _) -> bind (eval_expr0 a) (fun v -> eval_field ge (typeof a) i v)
  | _ ->
    Error0 ((MSG
      ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('r'::('-'::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::(' '::('i'::('n'::(' '::('e'::('v'::('a'::('l'::('_'::('l'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))))))))))))))))))))))))))) :: [])
  in eval_expr0

(** val eval_lvalue : genv -> env -> temp_env -> Mem.mem -> expr -> (block * Int.int) res **)

let eval_lvalue ge ve le m =
  let rec eval_expr0 a0 = match a0 with
  | Econst_int (i, _) -> OK (Vint i)
  | Econst_float (f, _) -> OK (Vfloat f)
  | Econst_single (f, _) -> OK (Vsingle f)
  | Econst_long (i, _) -> OK (Vlong i)
  | Evar (id, ty) -> bind2 (eval_var ge ve id ty) (fun l ofs -> deref_loc m (typeof a0) l ofs)
  | Etempvar (id, _) ->
    force_opt (PTree.get id le) ((MSG ('T'::('e'::('m'::('p'::('v'::('a'::('r'::(' '::[]))))))))) :: ((CTX id) :: ((MSG
      (' '::('n'::('o'::('t'::(' '::('i'::('n'::(' '::('s'::('c'::('o'::('p'::('e'::[])))))))))))))) :: [])))
  | Ederef (a, _) ->
    (match eval_expr0 a with
     | OK v ->
       (match v with
        | Vptr (l, ofs) -> deref_loc m (typeof a0) l ofs
        | _ ->
          Error0 ((MSG
            ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
     | Error0 _ ->
       Error0 ((MSG
         ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
  | Eaddrof (a, _) -> bind2 (eval_lvalue0 a) (fun l ofs -> OK (Vptr (l, ofs)))
  | Eunop (op, a, _) ->
    bind (eval_expr0 a) (fun v ->
      force_opt (sem_unary_operation op v (typeof a) m) ((MSG
        ('U'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))) :: []))
  | Ebinop (op, a1, a2, _) ->
    bind (eval_expr0 a1) (fun v1 ->
      bind (eval_expr0 a2) (fun v2 ->
        force_opt (sem_binary_operation ge.genv_cenv op v1 (typeof a1) v2 (typeof a2) m) ((MSG
          ('B'::('i'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))))))) :: [])))
  | Ecast (a, ty) ->
    bind (eval_expr0 a) (fun v ->
      force_opt (sem_cast v (typeof a) ty m) ((MSG
        ('C'::('a'::('s'::('t'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))) :: []))
  | Efield (a, i, _) ->
    bind (eval_expr0 a) (fun v -> bind2 (eval_field ge (typeof a) i v) (fun l ofs -> deref_loc m (typeof a0) l ofs))
  | Esizeof (t0, _) -> OK (Vint (Int.repr (sizeof ge.genv_cenv t0)))
  | Ealignof (t0, _) -> OK (Vint (Int.repr (alignof ge.genv_cenv t0)))
  and eval_lvalue0 = function
  | Evar (id, ty) -> eval_var ge ve id ty
  | Ederef (a, _) ->
    (match eval_expr0 a with
     | OK v ->
       (match v with
        | Vptr (l, ofs) -> OK (l, ofs)
        | _ ->
          Error0 ((MSG
            ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
     | Error0 _ ->
       Error0 ((MSG
         ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('n'::('o'::('n'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('b'::('e'::('i'::('n'::('g'::(' '::('d'::('e'::('r'::('e'::('f'::[]))))))))))))))))))))))))))))))))))) :: []))
  | Efield (a, i, _) -> bind (eval_expr0 a) (fun v -> eval_field ge (typeof a) i v)
  | _ ->
    Error0 ((MSG
      ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('r'::('-'::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::(' '::('i'::('n'::(' '::('e'::('v'::('a'::('l'::('_'::('l'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))))))))))))))))))))))))))) :: [])
  in eval_lvalue0

(** val eval_exprlist : genv -> env -> temp_env -> Mem.mem -> typelist -> expr list -> val0 list res **)

let rec eval_exprlist ge ve le m tl0 al0 =
  match tl0 with
  | Tnil ->
    (match al0 with
     | [] -> OK []
     | _ :: _ ->
       Error0 ((MSG ('e'::('v'::('a'::('l'::('_'::('e'::('x'::('p'::('r'::('l'::('i'::('s'::('t'::[])))))))))))))) :: []))
  | Tcons (ty, tyl) ->
    (match al0 with
     | [] -> Error0 ((MSG ('e'::('v'::('a'::('l'::('_'::('e'::('x'::('p'::('r'::('l'::('i'::('s'::('t'::[])))))))))))))) :: [])
     | a :: al ->
       bind (eval_expr ge ve le m (Ecast (a, ty))) (fun v -> bind (eval_exprlist ge ve le m tyl al) (fun vl -> OK (v :: vl))))

(** val do_ef_malloc : val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_ef_malloc vargs m =
  match vargs with
  | [] ->
    Error0 ((MSG
      ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: [])
  | v :: l ->
    (match v with
     | Vint n0 ->
       (match l with
        | [] ->
          let (m', b) = Mem.alloc m (Zneg (XO (XO XH))) (Int.unsigned n0) in
          bind
            (force_opt (Mem.store Mint32 m' b (Zneg (XO (XO XH))) (Vint n0)) ((MSG
              ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))) :: []))
            (fun m'' -> OK ((Vptr (b, Int.zero)), m''))
        | _ :: _ ->
          Error0 ((MSG
            ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: []))
     | _ ->
       Error0 ((MSG
         ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: []))

(** val do_ef_is_ptr : val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_ef_is_ptr vargs m =
  match vargs with
  | [] ->
    Error0 ((MSG
      ('N'::('o'::('t'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::(' '::('n'::('o'::('r'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('o'::('r'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))) :: [])
  | v :: l ->
    (match v with
     | Vint n0 ->
       (match l with
        | [] ->
          if Int.eq (Int.modu n0 (Int.repr (Zpos (XO XH)))) (Int.repr Z0)
          then Error0 ((MSG
                 ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('d'::('d'::(' '::('i'::('n'::('t'::[]))))))))))))))))) :: [])
          else OK ((Vint (Int.repr Z0)), m)
        | _ :: _ ->
          Error0 ((MSG
            ('N'::('o'::('t'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::(' '::('n'::('o'::('r'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('o'::('r'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))) :: []))
     | Vptr (_, _) ->
       (match l with
        | [] -> OK ((Vint (Int.repr (Zpos XH))), m)
        | _ :: _ ->
          Error0 ((MSG
            ('N'::('o'::('t'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::(' '::('n'::('o'::('r'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('o'::('r'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))) :: []))
     | _ ->
       Error0 ((MSG
         ('N'::('o'::('t'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::(' '::('n'::('o'::('r'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('o'::('r'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))) :: []))

(** val do_ef_gcmalloc : val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_ef_gcmalloc vargs m =
  match vargs with
  | [] ->
    Error0 ((MSG
      ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: [])
  | v :: l ->
    (match v with
     | Vptr (bnum, onum) ->
       (match l with
        | [] ->
          Error0 ((MSG
            ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: [])
        | v0 :: l0 ->
          (match v0 with
           | Vptr (bsrc, osrc) ->
             (match l0 with
              | [] ->
                (match deref_loc m (Tint0 (I32, Unsigned, noattr)) bnum onum with
                 | OK v1 ->
                   (match v1 with
                    | Vint n0 ->
                      let (m', b) = Mem.alloc m Z0 (Z.mul (Int.unsigned n0) (Zpos (XO (XO XH)))) in
                      let olimit = Int.repr (Z.add (Int.unsigned osrc) (size_chunk Mint32)) in
                      bind
                        (force_opt (Mem.store Mint32 m' bsrc (Int.unsigned osrc) (Vptr (b, (Int.repr Z0)))) ((MSG
                          ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))) :: []))
                        (fun m'' ->
                        bind
                          (force_opt
                            (Mem.store Mint32 m'' bsrc (Int.unsigned olimit) (Vptr (b,
                              (Int.repr (Z.mul (Int.unsigned n0) (Zpos (XO (XO XH)))))))) ((MSG
                            ('m'::('a'::('l'::('l'::('o'::('c'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))) :: []))
                          (fun m''' -> OK (Vundef, m''')))
                    | _ ->
                      Error0 ((MSG
                        ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('f'::('i'::('r'::('s'::('t'::(' '::('a'::('r'::('g'::(' '::('w'::('a'::('s'::('n'::('\''::('t'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('t'::('o'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::[])))))))))))))))))))))))))))))))))))))))))))))) :: []))
                 | Error0 _ ->
                   Error0 ((MSG
                     ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('f'::('i'::('r'::('s'::('t'::(' '::('a'::('r'::('g'::(' '::('w'::('a'::('s'::('n'::('\''::('t'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::(' '::('t'::('o'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::[])))))))))))))))))))))))))))))))))))))))))))))) :: []))
              | _ :: _ ->
                Error0 ((MSG
                  ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: []))
           | _ ->
             Error0 ((MSG
               ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: [])))
     | _ ->
       Error0 ((MSG
         ('g'::('c'::('m'::('a'::('l'::('l'::('o'::('c'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: []))

(** val do_ef_free : val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_ef_free vargs m =
  match vargs with
  | [] ->
    Error0 ((MSG ('f'::('r'::('e'::('e'::(':'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[]))))))))))))))) :: [])
  | v :: l ->
    (match v with
     | Vptr (b, lo) ->
       (match l with
        | [] ->
          bind
            (force_opt (Mem.load Mint32 m b (Z.sub (Int.unsigned lo) (Zpos (XO (XO XH))))) ((MSG
              ('f'::('r'::('e'::('e'::(':'::(' '::('l'::('o'::('a'::('d'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))) :: []))
            (fun vsz ->
            match vsz with
            | Vint sz ->
              if zlt Z0 (Int.unsigned sz)
              then bind
                     (force_opt
                       (Mem.free m b (Z.sub (Int.unsigned lo) (Zpos (XO (XO XH)))) (Z.add (Int.unsigned lo) (Int.unsigned sz)))
                       ((MSG ('f'::('r'::('e'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))) :: [])) (fun m' ->
                     OK (Vundef, m'))
              else Error0 ((MSG
                     ('f'::('r'::('e'::('e'::(':'::(' '::('h'::('e'::('a'::('d'::('e'::('r'::(' '::('c'::('o'::('r'::('r'::('u'::('p'::('t'::('e'::('d'::[]))))))))))))))))))))))) :: [])
            | _ ->
              Error0 ((MSG
                ('f'::('r'::('e'::('e'::(':'::(' '::('h'::('e'::('a'::('d'::('e'::('r'::(' '::('c'::('o'::('r'::('r'::('u'::('p'::('t'::('e'::('d'::[]))))))))))))))))))))))) :: []))
        | _ :: _ ->
          Error0 ((MSG
            ('f'::('r'::('e'::('e'::(':'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[]))))))))))))))) :: []))
     | _ ->
       Error0 ((MSG ('f'::('r'::('e'::('e'::(':'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[]))))))))))))))) :: []))

(** val do_ef_memcpy : z -> z -> val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_ef_memcpy sz al vargs m =
  match vargs with
  | [] ->
    Error0 ((MSG
      ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: [])
  | v :: l ->
    (match v with
     | Vptr (bdst, odst) ->
       (match l with
        | [] ->
          Error0 ((MSG
            ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: [])
        | v0 :: l0 ->
          (match v0 with
           | Vptr (bsrc, osrc) ->
             (match l0 with
              | [] ->
                if memcpy_check_args sz al bdst (Int.unsigned odst) bsrc (Int.unsigned osrc)
                then bind
                       (force_opt (Mem.loadbytes m bsrc (Int.unsigned osrc) sz) ((MSG
                         ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('l'::('o'::('a'::('d'::('b'::('y'::('t'::('e'::('s'::[]))))))))))))))))) :: []))
                       (fun bytes ->
                       bind
                         (force_opt (Mem.storebytes m bdst (Int.unsigned odst) bytes) ((MSG
                           ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('s'::('t'::('o'::('r'::('e'::('b'::('y'::('t'::('e'::('s'::[])))))))))))))))))) :: []))
                         (fun m' -> OK (Vundef, m')))
                else Error0 ((MSG
                       ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('g'::('s'::[])))))))))))))))))) :: [])
              | _ :: _ ->
                Error0 ((MSG
                  ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: []))
           | _ ->
             Error0 ((MSG
               ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: [])))
     | _ ->
       Error0 ((MSG
         ('m'::('e'::('m'::('c'::('p'::('y'::(' '::('b'::('a'::('d'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))))) :: []))

(** val do_external : external_function -> val0 list -> Mem.mem -> (val0 * Mem.mem) res **)

let do_external ef vargs m =
  match ef with
  | EF_external (name0, _) ->
    if string_eq_bool name0 ('g'::('c'::[]))
    then do_ef_gcmalloc vargs m
    else if string_eq_bool name0 ('i'::('s'::('_'::('p'::('t'::('r'::[]))))))
         then do_ef_is_ptr vargs m
         else Error0 ((MSG
                ('c'::('a'::('l'::('l'::('e'::('d'::(' '::('e'::('x'::('t'::('e'::('r'::('n'::('a'::('l'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[])))))))))))))))))))))))))) :: ((MSG
                name0) :: []))
  | EF_builtin (name0, _) ->
    Error0 ((MSG
      ('c'::('a'::('l'::('l'::('e'::('d'::(' '::('b'::('u'::('i'::('l'::('t'::('i'::('n'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[]))))))))))))))))))))))))) :: ((MSG
      name0) :: []))
  | EF_runtime (name0, _) ->
    Error0 ((MSG
      ('c'::('a'::('l'::('l'::('e'::('d'::(' '::('r'::('u'::('n'::('t'::('i'::('m'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[]))))))))))))))))))))))))) :: ((MSG
      name0) :: []))
  | EF_malloc -> do_ef_malloc vargs m
  | EF_free -> do_ef_free vargs m
  | EF_memcpy (sz, al) -> do_ef_memcpy sz al vargs m
  | EF_annot (text, _) -> Error0 ((MSG ('a'::('n'::('n'::('o'::('t'::(' '::[]))))))) :: ((MSG text) :: []))
  | EF_annot_val (text, _) ->
    Error0 ((MSG ('a'::('n'::('n'::('o'::('t'::('_'::('v'::('a'::('l'::(' '::[]))))))))))) :: ((MSG text) :: []))
  | EF_inline_asm (text, _, _) ->
    Error0 ((MSG ('i'::('n'::('l'::('i'::('n'::('e'::('_'::('a'::('s'::('m'::(' '::[])))))))))))) :: ((MSG text) :: []))
  | EF_debug (kind, text, _) -> Error0 ((MSG ('d'::('e'::('b'::('u'::('g'::(' '::[]))))))) :: ((CTX kind) :: ((CTX text) :: [])))
  | _ -> Error0 ((MSG ('v'::('o'::('l'::('a'::('t'::('i'::('l'::('e'::(' '::('l'::('o'::('a'::('d'::(' '::[]))))))))))))))) :: [])

(** val check_assign_copy : genv -> type0 -> block -> Int.int -> block -> Int.int -> bool **)

let check_assign_copy ge ty b ofs b' ofs' =
  let s = zdivide_dec (alignof_blockcopy ge.genv_cenv ty) (Int.unsigned ofs') in
  if s
  then let s0 = zdivide_dec (alignof_blockcopy ge.genv_cenv ty) (Int.unsigned ofs) in
       if s0
       then let s1 = eq_block b' b in
            if s1
            then let s2 = zeq (Int.unsigned ofs') (Int.unsigned ofs) in
                 if s2
                 then true
                 else let s3 = zle (Z.add (Int.unsigned ofs') (sizeof ge.genv_cenv ty)) (Int.unsigned ofs) in
                      if s3 then true else zle (Z.add (Int.unsigned ofs) (sizeof ge.genv_cenv ty)) (Int.unsigned ofs')
            else true
       else false
  else false

(** val do_assign_loc : genv -> type0 -> Mem.mem -> val0 -> val0 -> Mem.mem res **)

let do_assign_loc ge ty m vp v =
  match vp with
  | Vptr (b, ofs) ->
    (match access_mode ty with
     | By_value chunk ->
       if type_is_volatile ty
       then Error0 ((MSG
              ('V'::('o'::('l'::('a'::('t'::('i'::('l'::('e'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('a'::('t'::(' '::('b'::('l'::('o'::('c'::('k'::(' '::[]))))))))))))))))))))))))) :: ((POS
              b) :: []))
       else force_opt (Mem.storev chunk m (Vptr (b, ofs)) v) ((MSG
              ('s'::('t'::('o'::('r'::('e'::('v'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))) :: [])
     | By_copy ->
       (match v with
        | Vptr (b', ofs') ->
          if check_assign_copy ge ty b ofs b' ofs'
          then bind
                 (force_opt (Mem.loadbytes m b' (Int.unsigned ofs') (sizeof ge.genv_cenv ty)) ((MSG
                   ('l'::('o'::('a'::('d'::('b'::('y'::('t'::('e'::('s'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))) :: []))
                 (fun bytes ->
                 force_opt (Mem.storebytes m b (Int.unsigned ofs) bytes) ((MSG
                   ('s'::('t'::('o'::('r'::('e'::('b'::('y'::('t'::('e'::('s'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))) :: []))
          else Error0 ((MSG
                 ('c'::('h'::('e'::('c'::('k'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('_'::('c'::('o'::('p'::('y'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))))) :: [])
        | _ ->
          Error0 ((MSG
            ('m'::('e'::('m'::(' '::('c'::('o'::('p'::('y'::(' '::('a'::('t'::(' '::('n'::('o'::('t'::('-'::('a'::('-'::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[])))))))))))))))))))))))))) :: []))
     | _ ->
       Error0 ((MSG
         ('i'::('l'::('l'::('e'::('g'::('a'::('l'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::('_'::('m'::('o'::('d'::('e'::(' '::('f'::('o'::('r'::(' '::('a'::('s'::('s'::('i'::('g'::('n'::('m'::('e'::('n'::('t'::[]))))))))))))))))))))))))))))))))))) :: []))
  | _ ->
    Error0 ((MSG
      ('l'::('h'::('s'::(' '::('o'::('f'::(' '::('a'::('s'::('s'::('i'::('g'::('n'::('m'::('e'::('n'::('t'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[])))))))))))))))))))))))))))))))) :: [])

(** val alloc_variables : genv -> env -> Mem.mem -> (ident0 * type0) list -> (env * Mem.mem) res **)

let rec alloc_variables ge e m = function
| [] -> OK (e, m)
| p :: vars' ->
  let (id, ty) = p in
  let (m1, b1) = Mem.alloc m Z0 (sizeof ge.genv_cenv ty) in let e' = PTree.set id (b1, ty) e in alloc_variables ge e' m1 vars'

(** val step : genv -> state -> state res **)

let step ge = function
| State (f, s0, k, e, le, m) ->
  (match s0 with
   | Sskip ->
     (match k with
      | Kseq (s1, k0) -> OK (State (f, s1, k0, e, le, m))
      | Kloop1 (s1, s2, k0) -> OK (State (f, s2, (Kloop2 (s1, s2, k0)), e, le, m))
      | Kloop2 (s1, s2, k0) -> OK (State (f, (Sloop (s1, s2)), k0, e, le, m))
      | Kswitch k0 -> OK (State (f, Sskip, k0, e, le, m))
      | x ->
        bind
          (force_opt (Mem.free_list m (blocks_of_env ge e)) ((MSG
            ('S'::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('N'::('o'::('n'::('e'::(' '::('f'::('r'::('e'::('e'::('_'::('l'::('i'::('s'::('t'::[]))))))))))))))))))))))) :: []))
          (fun m' -> OK (Returnstate (Vundef, x, m'))))
   | Sassign (a1, a2) ->
     bind2 (eval_lvalue ge e le m a1) (fun l ofs ->
       bind (eval_expr ge e le m (Ecast (a2, (typeof a1)))) (fun v2 ->
         bind (do_assign_loc ge (typeof a1) m (Vptr (l, ofs)) v2) (fun m' -> OK (State (f, Sskip, k, e, le, m')))))
   | Sset (id, a) -> bind (eval_expr ge e le m a) (fun v -> OK (State (f, Sskip, k, e, (PTree.set id v le), m)))
   | Scall (optid, a, al) ->
     (match classify_fun (typeof a) with
      | Fun_case_f (tyargs, tyres, cconv) ->
        bind (eval_expr ge e le m a) (fun vf ->
          bind (eval_exprlist ge e le m tyargs al) (fun vargs ->
            bind
              (force_opt (Genv.find_funct ge.genv_genv vf) ((MSG
                ('S'::('c'::('a'::('l'::('l'::(' '::('G'::('e'::('n'::('v'::('.'::('f'::('i'::('n'::('d'::('_'::('f'::('u'::('n'::('c'::('t'::[])))))))))))))))))))))) :: []))
              (fun fd ->
              if type_eq (type_of_fundef fd) (Tfunction (tyargs, tyres, cconv))
              then OK (Callstate (fd, vargs, (Kcall (optid, f, e, le, k)), m))
              else Error0 ((MSG
                     ('S'::('c'::('a'::('l'::('l'::(' '::('t'::('y'::('p'::('e'::('_'::('o'::('f'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))) :: []))))
      | Fun_default ->
        Error0 ((MSG
          ('S'::('c'::('a'::('l'::('l'::(' '::('c'::('l'::('a'::('s'::('s'::('i'::('f'::('y'::('_'::('f'::('u'::('n'::[]))))))))))))))))))) :: []))
   | Sbuiltin (optid, ef, tyargs, al) ->
     bind (eval_exprlist ge e le m tyargs al) (fun vl ->
       bind2 (do_external ef vl m) (fun vres m' -> OK (State (f, Sskip, k, e, (set_opttemp optid vres le), m'))))
   | Ssequence (s1, s2) -> OK (State (f, s1, (Kseq (s2, k)), e, le, m))
   | Sifthenelse (a, s1, s2) ->
     bind (eval_expr ge e le m a) (fun v1 ->
       bind
         (force_opt (bool_val v1 (typeof a) m) ((MSG
           ('S'::('i'::('f'::('t'::('h'::('e'::('n'::('e'::('l'::('s'::('e'::(' '::('b'::('o'::('o'::('l'::('_'::('v'::('a'::('l'::[]))))))))))))))))))))) :: []))
         (fun b -> OK (State (f, (if b then s1 else s2), k, e, le, m))))
   | Sloop (s1, s2) -> OK (State (f, s1, (Kloop1 (s1, s2, k)), e, le, m))
   | Sbreak ->
     (match k with
      | Kseq (_, k0) -> OK (State (f, Sbreak, k0, e, le, m))
      | Kloop1 (_, _, k0) -> OK (State (f, Sskip, k0, e, le, m))
      | Kloop2 (_, _, k0) -> OK (State (f, Sskip, k0, e, le, m))
      | Kswitch k0 -> OK (State (f, Sskip, k0, e, le, m))
      | _ ->
        Error0 ((MSG
          ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('s'::('t'::('a'::('t'::('e'::('m'::('e'::('n'::('t'::[]))))))))))))))))))))) :: []))
   | Scontinue ->
     (match k with
      | Kseq (_, k0) -> OK (State (f, Scontinue, k0, e, le, m))
      | Kloop1 (s1, s2, k0) -> OK (State (f, s2, (Kloop2 (s1, s2, k0)), e, le, m))
      | Kswitch k0 -> OK (State (f, Scontinue, k0, e, le, m))
      | _ ->
        Error0 ((MSG
          ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('s'::('t'::('a'::('t'::('e'::('m'::('e'::('n'::('t'::[]))))))))))))))))))))) :: []))
   | Sreturn o ->
     (match o with
      | Some a ->
        bind (eval_expr ge e le m (Ecast (a, f.fn_return))) (fun v ->
          bind
            (force_opt (Mem.free_list m (blocks_of_env ge e)) ((MSG
              ('S'::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('N'::('o'::('n'::('e'::(' '::('f'::('r'::('e'::('e'::('_'::('l'::('i'::('s'::('t'::[]))))))))))))))))))))))) :: []))
            (fun m' -> OK (Returnstate (v, (call_cont k), m'))))
      | None ->
        bind
          (force_opt (Mem.free_list m (blocks_of_env ge e)) ((MSG
            ('S'::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('N'::('o'::('n'::('e'::(' '::('f'::('r'::('e'::('e'::('_'::('l'::('i'::('s'::('t'::[]))))))))))))))))))))))) :: []))
          (fun m' -> OK (Returnstate (Vundef, (call_cont k), m'))))
   | Sswitch (a, sl) ->
     bind (eval_expr ge e le m a) (fun v ->
       bind
         (force_opt (sem_switch_arg v (typeof a)) ((MSG
           ('s'::('w'::('i'::('t'::('c'::('h'::(' '::('a'::('r'::('g'::[]))))))))))) :: [])) (fun n0 -> OK (State (f,
         (seq_of_labeled_statement (select_switch n0 sl)), (Kswitch k), e, le, m))))
   | Slabel (_, s1) -> OK (State (f, s1, k, e, le, m))
   | Sgoto lbl ->
     bind2
       (force_opt (find_label lbl f.fn_body (call_cont k)) ((MSG
         ('f'::('i'::('n'::('d'::('_'::('l'::('a'::('b'::('e'::('l'::[]))))))))))) :: [])) (fun s' k' -> OK (State (f, s', k', e,
       le, m))))
| Callstate (fd, vargs, k, m) ->
  (match fd with
   | Internal f ->
     bind2 (alloc_variables ge empty_env m f.fn_vars) (fun e m' ->
       bind
         (force_opt (bind_parameter_temps f.fn_params vargs (create_undef_temps f.fn_temps)) ((MSG
           ('b'::('i'::('n'::('d'::('_'::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('_'::('t'::('e'::('m'::('p'::('s'::[]))))))))))))))))))))) :: []))
         (fun le -> OK (State (f, f.fn_body, k, e, le, m'))))
   | External (ef, _, _, _) -> bind2 (do_external ef vargs m) (fun vres m' -> OK (Returnstate (vres, k, m'))))
| Returnstate (v, k0, m) ->
  (match k0 with
   | Kcall (optid, f, e, le, k) -> OK (State (f, Sskip, k, e, (set_opttemp optid v le), m))
   | _ ->
     Error0 ((MSG
       ('U'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('s'::('t'::('a'::('t'::('e'::('m'::('e'::('n'::('t'::[]))))))))))))))))))))) :: []))

(** val is_stopped : state -> bool **)

let is_stopped = function
| Returnstate (_, k, _) -> (match k with
                            | Kstop -> true
                            | _ -> false)
| _ -> false

(** val stepstar_n : genv -> state -> name Coq_M.t -> int -> (state, state) bigStepResult **)

let rec stepstar_n ge s nenv fuel =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> OutOfTime s)
    (fun n0 ->
    match step ge s with
    | OK s' -> if is_stopped s' then Result s' else stepstar_n ge s' nenv n0
    | Error0 err -> Error ((print_error err (Some nenv)), (Some s)))
    fuel

(** val do_initial_state_wo_main : ident0 -> ident0 -> program1 -> ((genv * state) * block) res **)

let do_initial_state_wo_main threadInfIdent0 bodyIdent0 p =
  let ge = globalenv0 p in
  bind
    (force_opt (Genv.init_mem (program_of_program p)) ((MSG ('i'::('n'::('i'::('t'::('_'::('m'::('e'::('m'::[]))))))))) :: []))
    (fun m0 ->
    bind
      (force_opt (Genv.find_symbol ge.genv_genv bodyIdent0) ((MSG
        ('c'::('a'::('n'::('\''::('t'::(' '::('f'::('i'::('n'::('d'::(' '::('b'::('o'::('d'::('y'::(' '::('2'::[])))))))))))))))))) :: []))
      (fun b ->
      bind
        (force_opt (Genv.find_funct_ptr ge.genv_genv b) ((MSG
          ('c'::('a'::('n'::('\''::('t'::(' '::('f'::('i'::('n'::('d'::(' '::('b'::('o'::('d'::('y'::(','::(' '::('p'::('a'::('r'::('t'::(' '::('3'::[])))))))))))))))))))))))) :: []))
        (fun f ->
        let (m, b0) = Mem.alloc m0 Z0 (sizeof ge.genv_cenv (Tstruct (threadInfIdent0, noattr))) in
        let (m', b1) = Mem.alloc m Z0 (size_chunk Mint32) in
        bind
          (force_opt (Mem.store Mint32 m' b1 Z0 (Vint (Int.repr Z0))) ((MSG
            ('0'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))) :: []))
          (fun m'' ->
          bind
            (force_opt (Mem.store Mint32 m'' b0 Z0 (Vptr (b1, (Int.repr Z0)))) ((MSG
              ('a'::('l'::('l'::('o'::('c'::(' '::('p'::('t'::('r'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))) :: []))
            (fun m3 ->
            bind
              (force_opt (Mem.store Mint32 m3 b0 (size_chunk Mint32) (Vptr (b1, (Int.repr Z0)))) ((MSG
                ('l'::('i'::('m'::('i'::('t'::(' '::('p'::('t'::('r'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))) :: []))
              (fun m4 -> OK ((ge, (Callstate (f, ((Vptr (b0, (Int.repr Z0))) :: []), Kstop, m4))), b0)))))))

(** val at_final_state_wo_main : state -> unit res **)

let at_final_state_wo_main = function
| Returnstate (res0, k, _) ->
  (match res0 with
   | Vundef ->
     (match k with
      | Kstop -> OK ()
      | _ ->
        Error0 ((MSG
          ('m'::('a'::('i'::('n'::(' '::('d'::('i'::('d'::(' '::('n'::('o'::('t'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('n'::(' '::('v'::('o'::('i'::('d'::[])))))))))))))))))))))))))))) :: []))
   | _ ->
     Error0 ((MSG
       ('m'::('a'::('i'::('n'::(' '::('d'::('i'::('d'::(' '::('n'::('o'::('t'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('n'::(' '::('v'::('o'::('i'::('d'::[])))))))))))))))))))))))))))) :: []))
| _ ->
  Error0 ((MSG
    ('m'::('a'::('i'::('n'::(' '::('d'::('i'::('d'::(' '::('n'::('o'::('t'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('n'::(' '::('v'::('o'::('i'::('d'::[])))))))))))))))))))))))))))) :: [])

(** val run_wo_main : ident0 -> ident0 -> (name Coq_M.t * program1) -> int -> (state, Int.int) bigStepResult **)

let run_wo_main threadInfIdent0 bodyIdent0 e fuel =
  let (nenv, p) = e in
  (match do_initial_state_wo_main threadInfIdent0 bodyIdent0 p with
   | OK p0 ->
     let (p1, _) = p0 in
     let (ge, s) = p1 in
     (match stepstar_n ge s nenv fuel with
      | Result s0 ->
        (match at_final_state_wo_main s0 with
         | OK _ -> Result (Int.repr (Zpos XH))
         | Error0 err ->
           Error
             ((append
                ('E'::('r'::('r'::('o'::('r'::(' '::('i'::('n'::(' '::('a'::('t'::('_'::('f'::('i'::('n'::('a'::('l'::('_'::('s'::('t'::('a'::('t'::('e'::(':'::(' '::[])))))))))))))))))))))))))
                (print_error err (Some nenv))), (Some s0)))
      | OutOfTime s0 -> OutOfTime s0
      | Error (e0, os) -> Error (e0, os))
   | Error0 err ->
     Error
       ((append
          ('E'::('r'::('r'::('o'::('r'::(' '::('w'::('h'::('i'::('l'::('e'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('i'::('n'::('g'::(' '::('s'::('t'::('a'::('t'::('e'::(':'::(' '::[]))))))))))))))))))))))))))))))))
          (print_error err (Some nenv))), None))

(** val show_nat : int -> char list **)

let show_nat =
  nat2string10

(** val bodyIdent : positive **)

let bodyIdent =
  XO (XI (XO (XI (XI (XO XH)))))

(** val threadInfIdent : positive **)

let threadInfIdent =
  XI (XI (XI (XI XH)))

(** val ofib7 : name M.tree * function0 program0 option **)

let ofib7 =
  ((M.Node ((M.Node ((M.Node ((M.Node ((M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('0'::('0'::('0'::('0'::('1'::[])))))))))))))))), (M.Node ((M.Node
    (M.Leaf, (Some (NNamed
    ('g'::('a'::('r'::('b'::('a'::('g'::('e'::('_'::('c'::('o'::('l'::('l'::('e'::('c'::('t'::[]))))))))))))))))), M.Leaf)),
    None, M.Leaf)))), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('0'::('0'::('0'::('1'::[])))))))))))))))), M.Leaf)), None,
    (M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('0'::('0'::('1'::('1'::[]))))))))))))))), (M.Node (M.Leaf, (Some
    (NNamed ('a'::('l'::('l'::('o'::('c'::[]))))))), M.Leaf)))))), None, (M.Node ((M.Node ((M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('0'::('1'::('0'::('0'::('1'::[])))))))))))))))), (M.Node ((M.Node
    (M.Leaf, (Some (NNamed ('i'::('s'::('_'::('p'::('t'::('r'::[])))))))), M.Leaf)), None, M.Leaf)))), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('0'::('1'::('0'::('1'::[])))))))))))))))), (M.Node (M.Leaf,
    (Some (NNamed ('a'::('r'::('g'::('s'::[])))))), (M.Node ((M.Node (M.Leaf, (Some (NNamed ('b'::('o'::('d'::('y'::[])))))),
    M.Leaf)), None, M.Leaf)))))), None, (M.Node ((M.Node ((M.Node (M.Leaf, None, (M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('i'::('n'::('f'::('o'::('_'::('0'::('1'::('1'::('0'::('0'::('1'::('1'::[])))))))))))))))))))))))),
    M.Leaf)))), None, M.Leaf)), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('0'::('1'::('1'::('1'::[]))))))))))))))), M.Leaf)))))), None, (M.Node
    ((M.Node ((M.Node ((M.Node ((M.Node (M.Leaf, None, (M.Node (M.Leaf, (Some (NNamed
    ('n'::('u'::('m'::('_'::('a'::('r'::('g'::('s'::[])))))))))), M.Leaf)))), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('1'::('0'::('0'::('0'::('1'::[]))))))))))))))))), (M.Node
    ((M.Node (M.Leaf, (Some (NNamed ('m'::('a'::('i'::('n'::[])))))), M.Leaf)), None, M.Leaf)))), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('1'::('0'::('0'::('1'::[])))))))))))))))), M.Leaf)), None,
    (M.Node ((M.Node ((M.Node (M.Leaf, None, (M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('i'::('n'::('f'::('o'::('_'::('1'::('0'::('1'::('0'::('0'::('1'::('1'::[])))))))))))))))))))))))),
    M.Leaf)))), None, M.Leaf)), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('1'::('0'::('1'::('1'::[])))))))))))))))), (M.Node (M.Leaf,
    (Some (NNamed ('l'::('i'::('m'::('i'::('t'::[]))))))), M.Leaf)))))), None, (M.Node ((M.Node ((M.Node (M.Leaf, (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('1'::('1'::('0'::('0'::('1'::[])))))))))))))))), M.Leaf)), (Some
    (NNamed ('a'::('n'::('o'::('n'::('_'::('e'::('n'::('v'::('_'::('1'::('1'::('0'::('1'::[]))))))))))))))), (M.Node (M.Leaf,
    None, (M.Node ((M.Node (M.Leaf, (Some (NNamed ('t'::('i'::('n'::('f'::('o'::[]))))))), M.Leaf)), None, M.Leaf)))))), (Some
    (NNamed ('e'::('n'::('v'::('_'::('1'::('1'::('1'::[]))))))))), (M.Node ((M.Node ((M.Node (M.Leaf, None, (M.Node (M.Leaf,
    (Some (NNamed
    ('b'::('o'::('d'::('y'::('_'::('i'::('n'::('f'::('o'::('_'::('1'::('1'::('1'::('0'::('0'::('1'::('1'::[]))))))))))))))))))),
    M.Leaf)))), None, M.Leaf)), (Some (NNamed
    ('a'::('n'::('o'::('n'::('_'::('c'::('o'::('d'::('e'::('_'::('1'::('1'::('1'::('1'::[])))))))))))))))), (M.Node (M.Leaf,
    (Some (NNamed ('t'::('h'::('r'::('e'::('a'::('d'::('_'::('i'::('n'::('f'::('o'::[]))))))))))))), (M.Node ((M.Node (M.Leaf,
    (Some (NNamed ('h'::('e'::('a'::('p'::[])))))), M.Leaf)), None, M.Leaf)))))))))))), (Some { prog_defs0 = (((XO (XO (XO (XO
    (XI (XO XH)))))), (Gfun (External ((EF_external (('g'::('c'::[])), { sig_args = (Tany32 :: []); sig_res = None; sig_cc =
    { cc_vararg = false; cc_unproto = false; cc_structret = false } })), (Tcons ((Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), (Tcons ((Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })), Tnil)))), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret = false })))) :: (((XO (XI (XO (XO (XI (XO
    XH)))))), (Gfun (External ((EF_external (('i'::('s'::('_'::('p'::('t'::('r'::[])))))), { sig_args = (Tany32 :: []); sig_res =
    None; sig_cc = { cc_vararg = false; cc_unproto = false; cc_structret = false } })), (Tcons ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint0 (IBool, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = false; cc_unproto = false; cc_structret = false })))) :: (((XI (XI (XI (XO (XO (XI XH)))))), (Gvar
    { gvar_info = (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO XH)),
    { attr_volatile = false; attr_alignas = None })); gvar_init = ((Init_int32
    (Int.repr (Zpos (XO (XI (XO XH)))))) :: ((Init_int32 (Int.repr Z0)) :: [])); gvar_readonly = true; gvar_volatile =
    false })) :: (((XO (XI (XI (XO (XO (XI XH)))))), (Gvar { gvar_info = (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Zpos (XI (XO XH))), { attr_volatile = false; attr_alignas = None })); gvar_init = ((Init_int32
    (Int.repr (Zpos (XO XH)))) :: ((Init_int32 (Int.repr (Zpos (XI XH)))) :: ((Init_int32 (Int.repr Z0)) :: ((Init_int32
    (Int.repr (Zpos XH))) :: ((Init_int32 (Int.repr (Zpos (XO XH)))) :: []))))); gvar_readonly = true; gvar_volatile =
    false })) :: (((XI (XO (XI (XO (XO (XI XH)))))), (Gvar { gvar_info = (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Zpos (XO (XO XH))), { attr_volatile = false; attr_alignas = None })); gvar_init = ((Init_int32
    (Int.repr Z0)) :: ((Init_int32 (Int.repr (Zpos (XO XH)))) :: ((Init_int32 (Int.repr Z0)) :: ((Init_int32
    (Int.repr (Zpos XH))) :: [])))); gvar_readonly = true; gvar_volatile = false })) :: (((XI (XO (XO XH))), (Gfun (Internal
    { fn_return = Tvoid; fn_callconv = { cc_vararg = false; cc_unproto = false; cc_structret = false }; fn_params = (((XI (XI (XO
    (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None }))) :: []); fn_vars = (((XI (XI (XO (XO XH)))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: ((XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: (((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XO (XI (XO (XI
    XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None }))) :: (((XI (XI (XO (XO (XI (XO XH)))))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])))))); fn_temps = []; fn_body = (Ssequence ((Ssequence ((Ssequence ((Ssequence ((Ssequence
    ((Sset ((XO (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI
    (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))))), (Sset ((XI (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XI (XO (XI (XI XH)))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))))))), (Sset ((XO (XI (XO (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XI (XO (XI XH)))),
    (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO XH))))))))))), { attr_volatile = false; attr_alignas = None })))))))), (Sifthenelse ((Eunop (Onotbool, (Ebinop (Ole,
    (Ederef ((Evar ((XI (XO (XI (XO (XO (XI XH)))))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Zpos (XO (XO XH))), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Ebinop (Osub, (Etempvar ((XI (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO (XI
    (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint0 (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Scall (None, (Evar ((XO
    (XO (XO (XO (XI (XO XH)))))), (Tfunction ((Tcons ((Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })), (Tcons ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), Tnil)))), Tvoid,
    { cc_vararg = false; cc_unproto = false; cc_structret = false })))), ((Evar ((XI (XO (XI (XO (XO (XI XH)))))), (Tarray
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO (XO XH))), { attr_volatile = false;
    attr_alignas = None })))) :: ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))) :: [])))), (Sset ((XO
    (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI
    XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI
    (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))),
    Sskip)))), (Ssequence ((Sset ((XI (XI (XO (XO XH)))), (Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr Z0), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sset (XH, (Ederef ((Ebinop (Oadd,
    (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), Sskip)))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar (XH, (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Sreturn None))))) }))) :: (((XO (XO (XO XH))), (Gfun (Internal
    { fn_return = Tvoid; fn_callconv = { cc_vararg = false; cc_unproto = false; cc_structret = false }; fn_params = (((XI (XI (XO
    (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None }))) :: []); fn_vars = (((XO (XO (XI XH))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((XI XH), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: ((XH,
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO (XI XH))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((XO (XI (XI XH))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((XI (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: ((XH,
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: ((XH, (Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None }))) :: (((XI (XI (XI XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: (((XO (XO (XO (XO XH)))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO
    (XO (XO XH)))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XO (XI (XO (XO XH)))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO
    (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None }))) :: (((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI (XI (XO (XO (XI (XO XH)))))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: []))))))))))))))))); fn_temps = []; fn_body = (Ssequence
    ((Ssequence ((Ssequence ((Ssequence ((Ssequence ((Sset ((XO (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI
    (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))))), (Sset ((XI (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO
    (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XI (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))))))), (Sset ((XO (XI (XO (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI
    (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false; attr_alignas = None })))))))), (Sifthenelse
    ((Eunop (Onotbool, (Ebinop (Ole, (Ederef ((Evar ((XO (XI (XI (XO (XO (XI XH)))))), (Tarray ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Zpos (XI (XO XH))), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Ebinop (Osub, (Etempvar ((XI (XO (XI (XI XH)))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Scall
    (None, (Evar ((XO (XO (XO (XO (XI (XO XH)))))), (Tfunction ((Tcons ((Tpointer ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), (Tcons ((Tpointer ((Tstruct ((XI (XI (XI
    (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), Tnil)))),
    Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret = false })))), ((Evar ((XO (XI (XI (XO (XO (XI XH)))))), (Tarray
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XI (XO XH))), { attr_volatile = false;
    attr_alignas = None })))) :: ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))) :: [])))), (Sset ((XO
    (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI
    XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI
    (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))),
    Sskip)))), (Ssequence ((Sset ((XO (XO (XI XH))), (Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr Z0), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sset ((XI XH), (Ederef ((Ebinop (Oadd,
    (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Sset ((XO (XO XH)), (Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr (Zpos (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), Sskip)))))))), (Ssequence ((Scall ((Some (XI (XI
    (XO (XO (XI (XO XH))))))), (Evar ((XO (XI (XO (XO (XI (XO XH)))))), (Tfunction ((Tcons ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint0 (IBool, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = false; cc_unproto = false; cc_structret = false })))), ((Ecast ((Etempvar ((XI XH), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))) :: []))), (Sifthenelse ((Etempvar ((XI (XI (XO (XO (XI (XO XH)))))), (Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Sswitch ((Ebinop (Oand, (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XI XH), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zneg XH)), (Tint0 (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zpos (XI (XI (XI (XI (XI (XI (XI XH))))))))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (LScons (None, (Ssequence ((Sset ((XI (XO XH)), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XI XH), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Scall ((Some (XI (XI (XO (XO (XI (XO XH))))))), (Evar ((XO (XI (XO (XO (XI (XO XH)))))),
    (Tfunction ((Tcons ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint0 (IBool, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = false; cc_unproto = false; cc_structret = false })))),
    ((Ecast ((Etempvar ((XI (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))) :: []))), (Sifthenelse ((Etempvar ((XI (XI (XO (XO (XI (XO
    XH)))))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Sswitch ((Ebinop (Oand, (Ederef
    ((Ebinop (Oadd, (Ecast ((Etempvar ((XI (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Econst_int ((Int.repr (Zneg XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos (XI (XI (XI (XI (XI (XI (XI XH))))))))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (LScons (None, (Ssequence ((Sset ((XI
    (XO (XO (XO XH)))), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Sset ((XO (XI (XO (XO XH)))), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO XH)), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0
    (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))), (Ssequence ((Ssequence ((Sset ((XO (XI (XO (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO
    (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false; attr_alignas = None })))))), (Ssequence
    ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Etempvar ((XO (XI (XO (XO XH)))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XI XH), (Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))))), (Sassign ((Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))))))))))), (Scall (None, (Ecast ((Etempvar ((XI (XO (XO (XO
    XH)))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tfunction ((Tcons ((Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })), Tnil)), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret = false })), { attr_volatile = false;
    attr_alignas = None })))), ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))) :: []))))))))),
    LSnil)))), (Sswitch ((Ebinop (Oshr, (Etempvar ((XI (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (LScons (None, (Ssequence ((Sset (XH, (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Ssequence
    ((Ssequence ((Ssequence ((Sset (XH, (Ecast ((Ebinop (Oadd, (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Sset ((XO (XO (XI (XI XH)))), (Ebinop (Oadd, (Etempvar ((XO
    (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH, (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zneg XH)), (Tint0 (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH,
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Etempvar (XH, (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))))), (Ssequence ((Sset ((XI (XI (XI XH))), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO XH)),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sset ((XO (XO (XO (XO XH)))), (Ederef ((Ebinop (Oadd,
    (Ecast ((Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Ssequence ((Sset ((XO (XI (XO (XI XH)))),
    (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })))), (XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false;
    attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr Z0), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO (XO (XO XH)))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO
    (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar (XH, (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Sassign ((Efield ((Ederef ((Etempvar ((XI (XI (XO (XI
    (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))))))), (Scall (None,
    (Ecast ((Etempvar ((XI (XI (XI XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tfunction ((Tcons ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })), Tnil)), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret =
    false })), { attr_volatile = false; attr_alignas = None })))), ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))) :: []))))))))))))), LSnil)))))))))), LSnil)))), (Sswitch ((Ebinop (Oshr, (Etempvar ((XI XH), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (LScons (None, (Ssequence ((Sset (XH, (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))))), (Ssequence ((Sset ((XI (XO (XI XH))), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO
    XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sset ((XO (XI (XI XH))), (Ederef ((Ebinop (Oadd, (Ecast
    ((Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Ssequence ((Sset ((XO (XI (XO (XI XH)))),
    (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })))), (XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false;
    attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr Z0), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XI (XI XH))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO
    (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar (XH, (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Sassign ((Efield ((Ederef ((Etempvar ((XI (XI (XO (XI
    (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas =
    None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))))))), (Scall (None,
    (Ecast ((Etempvar ((XI (XO (XI XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tfunction ((Tcons ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })), Tnil)), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret =
    false })), { attr_volatile = false; attr_alignas = None })))), ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))) :: []))))))))))), LSnil)))))))))) }))) :: (((XO (XI (XO (XI (XI (XO XH)))))), (Gfun (Internal { fn_return = Tvoid;
    fn_callconv = { cc_vararg = false; cc_unproto = false; cc_structret = false }; fn_params = (((XI (XI (XO (XI (XI (XO
    XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None }))) :: []); fn_vars = ((XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: ((XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: ((XH, (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((XI (XI XH)), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((XO XH), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XO (XO
    XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((XO (XI (XO XH))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((XI (XI (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO (XI (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XO
    (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None }))) :: []))))))))))); fn_temps = []; fn_body = (Ssequence ((Ssequence ((Ssequence ((Ssequence
    ((Sset ((XO (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI
    (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))))), (Sset ((XI (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XI (XO (XI (XI XH)))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))))))), (Sset ((XO (XI (XO (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XI (XO (XI XH)))),
    (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO XH))))))))))), { attr_volatile = false; attr_alignas = None })))))))), (Sifthenelse ((Eunop (Onotbool, (Ebinop (Ole,
    (Ederef ((Evar ((XI (XI (XI (XO (XO (XI XH)))))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Zpos (XO XH)), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ebinop (Osub, (Etempvar ((XI (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO (XI
    (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint0 (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Scall (None, (Evar ((XO
    (XO (XO (XO (XI (XO XH)))))), (Tfunction ((Tcons ((Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })), (Tcons ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), Tnil)))), Tvoid,
    { cc_vararg = false; cc_unproto = false; cc_structret = false })))), ((Evar ((XI (XI (XI (XO (XO (XI XH)))))), (Tarray
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO XH)), { attr_volatile = false;
    attr_alignas = None })))) :: ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))) :: [])))), (Sset ((XO
    (XO (XI (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI
    XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tstruct ((XI
    (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))),
    Sskip)))), (Ssequence ((Sset (XH, (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))), (Ssequence ((Ssequence ((Ssequence ((Ssequence ((Sset (XH, (Ecast ((Ebinop (Oadd, (Etempvar ((XO
    (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Sset ((XO (XO (XI (XI XH)))), (Ebinop (Oadd, (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))),
    (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zneg XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH, (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Etempvar (XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))))), (Ssequence
    ((Ssequence ((Ssequence ((Ssequence ((Sset (XH, (Ecast ((Ebinop (Oadd, (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Sset ((XO (XO (XI (XI XH)))), (Ebinop (Oadd,
    (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos (XO XH))), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH,
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zneg XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar (XH, (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Etempvar (XH, (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))))), (Ssequence ((Sset
    ((XI (XI XH)), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Ssequence ((Ssequence ((Sset ((XO XH), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO XH), (Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Evar ((XO (XO (XO XH))), (Tfunction ((Tcons ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })), Tnil)), Tvoid,
    { cc_vararg = false; cc_unproto = false; cc_structret = false })))))))), (Sassign ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar
    ((XO XH), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XI (XI XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))))), (Ssequence ((Ssequence ((Ssequence ((Sset ((XO (XO XH)), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Sassign ((Ederef
    ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Evar ((XI (XO (XO XH))), (Tfunction ((Tcons
    ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })), Tnil)), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret = false })))))))), (Sassign
    ((Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XI (XI
    XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))))), (Ssequence ((Sset ((XO (XI (XO XH))),
    (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO XH), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sset
    ((XI (XI (XO XH))), (Ederef ((Ebinop (Oadd, (Ecast ((Etempvar ((XO XH), (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr (Zpos XH)), (Tint0 (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))), (Ssequence ((Ssequence ((Sset ((XO (XI (XO (XI XH)))), (Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO
    XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO
    (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Zpos (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef
    ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int ((Int.repr Z0), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Etempvar ((XI (XI (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))))),
    (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Econst_int
    ((Int.repr (Zpos XH)), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))), (Tint0 (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar (XH, (Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))), (Ssequence ((Sassign ((Ederef ((Ebinop (Oadd, (Etempvar ((XO (XI (XO (XI XH)))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Econst_int ((Int.repr (Zpos (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer
    ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))),
    (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Etempvar ((XO (XO XH)), (Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))), (Sassign ((Efield ((Ederef ((Etempvar ((XI (XI (XO (XI (XI (XO
    XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))), (Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })))), (XO
    (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))), (Etempvar ((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None })))))))))))))), (Scall (None, (Ecast ((Etempvar
    ((XO (XI (XO XH))), (Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tpointer ((Tfunction ((Tcons
    ((Tpointer ((Tstruct ((XI (XI (XI (XI XH)))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None })), Tnil)), Tvoid, { cc_vararg = false; cc_unproto = false; cc_structret = false })), { attr_volatile =
    false; attr_alignas = None })))), ((Etempvar ((XI (XI (XO (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI XH)))),
    { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas =
    None })))) :: []))))))))))))))))))))))) }))) :: [])))))))); prog_public0 = ((XO (XI (XO (XI (XI (XO XH)))))) :: []);
    prog_main0 = (XI (XO (XO (XO (XI (XO XH)))))); prog_types = ((Composite ((XI (XI (XI (XI XH)))), Struct, (((XO (XO (XI (XI
    XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None }))) :: (((XI (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI (XI (XI (XI (XI (XO XH)))))), (Tpointer
    ((Tstruct ((XI (XI (XI (XI (XI (XO XH)))))), { attr_volatile = false; attr_alignas = None })), { attr_volatile = false;
    attr_alignas = None }))) :: (((XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))), { attr_volatile = false; attr_alignas =
    None }))) :: [])))), { attr_volatile = false; attr_alignas = None })) :: []); prog_comp_env = (PTree.Node (PTree.Leaf, None,
    (PTree.Node (PTree.Leaf, None, (PTree.Node (PTree.Leaf, None, (PTree.Node (PTree.Leaf, None, (PTree.Node (PTree.Leaf, (Some
    { co_su = Struct; co_members = (((XO (XO (XI (XI XH)))), (Tpointer ((Tint0 (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI (XO (XI (XI XH)))), (Tpointer ((Tint0
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), { attr_volatile = false; attr_alignas = None }))) :: (((XI
    (XI (XI (XI (XI (XO XH)))))), (Tpointer ((Tstruct ((XI (XI (XI (XI (XI (XO XH)))))), { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None }))) :: (((XO (XI (XO (XI XH)))), (Tarray ((Tint0 (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))),
    { attr_volatile = false; attr_alignas = None }))) :: [])))); co_attr = { attr_volatile = false; attr_alignas = None };
    co_sizeof = (Zpos (XO (XO (XI (XI (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))))); co_alignof = (Zpos (XO (XO XH)));
    co_rank = (Pervasives.succ 0) }), PTree.Leaf)))))))))) }))

(** val fib7 : (name M.tree * function0 program0) exception0 **)

let fib7 =
  let (nenv, o) = ofib7 in (match o with
                            | Some fib8 -> Ret (nenv, fib8)
                            | None -> Exc ('N'::('o'::('n'::('e'::[])))))

(** val print_BigStepResult_L7 : (name M.t * program1) -> int -> unit **)

let print_BigStepResult_L7 p n0 =
  print
    (match run_wo_main threadInfIdent bodyIdent p n0 with
     | Result v -> show_nat (nat_of_Z (Int.unsigned v))
     | OutOfTime _ -> 'O'::('u'::('t'::(' '::('o'::('f'::(' '::('t'::('i'::('m'::('e'::[]))))))))))
     | Error (s, _) -> s)

(** val print_opt_BigStepResult_L7 : (name M.t * program1) exception0 -> int -> unit **)

let print_opt_BigStepResult_L7 po n0 =
  match po with
  | Exc _ -> ()
  | Ret p0 -> print_BigStepResult_L7 p0 n0

(** val testColor : unit **)

let testColor =
  print_opt_BigStepResult_L7 fib7 (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
