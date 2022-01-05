
type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp

(* val eval : (string * bool) list -> log_exp -> bool *)

type oper = Not

type biOper = Or | And | If | Iff

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop

let rec prop_of_log_exp =
	let pole = prop_of_log_exp in function
	  Const x -> C x
	| Var x -> V x
	| Neg x -> Op (Not, pole x)
	| Disj (x, y) -> BiOp (Or, pole x, pole y)
	| Conj (x, y) -> BiOp (And, pole x, pole y)
	| Cond (x, y) -> BiOp (If, pole x, pole y)
	| BiCond (x, y) -> BiOp (Iff, pole x, pole y);;

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

let rec peval ctx = function
	  C x -> x
	| V x -> List.assoc x ctx
	| Op (op, x) -> (opval op) (peval ctx x)
	| BiOp (op, x, y) -> (biopval op) (peval ctx x) (peval ctx y);;

let is_tau prop =
	(*let rec lista_valores = function
		  C x -> []
		| V x -> [x]
		| Op (_, x) -> lista_valores x
		| BiOp (_, x, y) -> (lista_valores x) @ (lista_valores y)
	in*)
		peval [ ("p", false); ("q", false) ] prop &&
		peval [ ("p", true ); ("q", false) ] prop &&
		peval [ ("p", false); ("q", true ) ] prop &&
		peval [ ("p", true ); ("q", true ) ] prop;;
