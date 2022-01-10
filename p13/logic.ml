type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp

(* val eval : (string * bool) list -> log_exp -> bool *)

(* BiCond (Cond (Var "p", Var "q"), Disj (Neg (Var "p"), Var "q")) *)

(* let rec eval ctx = function
 *     Const b -> b
 *   | Var s -> List.assoc s ctx
 *   | Neg e -> not (eval ctx e)
 *   | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
 *   | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
 *   | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
 *   | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);; *)

type oper = Not

type biOper = Or | And | If | Iff

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop

(* BiOp (Iff, BiOp (If, V "p", V "q"), BiOP (Or, Op (Not, V "p"), V "q")) *)

(* log_exp -> prop *)
let rec prop_of_log_exp =
  let pole = prop_of_log_exp in function
	  Const x -> C x
	| Var x -> V x
	| Neg x -> Op (Not, pole x)
	| Disj (x, y) -> BiOp (Or, pole x, pole y)
	| Conj (x, y) -> BiOp (And, pole x, pole y)
	| Cond (x, y) -> BiOp (If, pole x, pole y)
	| BiCond (x, y) -> BiOp (Iff, pole x, pole y);;

(* prop -> log_exp *)
let rec log_exp_of_prop =
  let leop = log_exp_of_prop in function
	  C x -> Const x
	| V x -> Var x
	| Op (_, x) -> Neg (leop x)
	| BiOp (op, x, y) ->
      match op with
	      Or  -> Disj   (leop x, leop y)
	    | And -> Conj   (leop x, leop y)
	    | If  -> Cond   (leop x, leop y)
	    | Iff -> BiCond (leop x, leop y);;

let opval = function
	Not -> not;;

let biopval = function
	Or -> (||)
  | And -> (&&)
  | If -> (fun x y -> x || (not y) )
  | Iff -> (fun x y -> (not (x || y)) || (x && y) );;

(* (string * bool) list -> prop -> bool *)
let rec peval ctx = function
	C x -> x
  | V x -> List.assoc x ctx
  | Op (op, x) -> (opval op) (peval ctx x)
  | BiOp (op, x, y) -> (biopval op) (peval ctx x) (peval ctx y);;

(* prop -> bool *)
let is_tau prop =
  peval [ ("p", false); ("q", false) ] prop &&
  peval [ ("p", true ); ("q", false) ] prop &&
  peval [ ("p", false); ("q", true ) ] prop &&
  peval [ ("p", true ); ("q", true ) ] prop;;
