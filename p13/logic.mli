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

val prop_of_log_exp : log_exp -> prop

val log_exp_of_prop : prop -> log_exp

val peval : (string * bool) list -> prop -> bool

val is_tau : prop -> bool
