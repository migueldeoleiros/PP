open List;;

type log_exp =
      Const of bool
    | Var of string
    | Neg of log_exp
    | Disj of log_exp * log_exp
    | Conj of log_exp * log_exp
    | Cond of log_exp * log_exp
    | BiCond of log_exp * log_exp;;

let rec eval ctx = function
      Const b -> b
    | Var s -> List.assoc s ctx
    | Neg e -> not (eval ctx e)
    | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
    | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
    | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
    | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;


(*******************************************************)

type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
      C of bool
    | V of string
    | Op of oper * prop
    | BiOp of biOper * prop * prop;;


(* A *)

let rec prop_of_log_exp = function 
      Const b -> C b     
    | Var s -> V s
    | Neg n -> Op (Not,prop_of_log_exp (n))
    | Disj (l1,l2) -> BiOp (Or,prop_of_log_exp(l1),prop_of_log_exp(l1))
    | Conj (l1,l2) -> BiOp (And,prop_of_log_exp(l1),prop_of_log_exp(l1))
    | Cond (l1,l2) -> BiOp (If,prop_of_log_exp(l1),prop_of_log_exp(l1))
    | BiCond (l1,l2) -> BiOp (Iff,prop_of_log_exp(l1),prop_of_log_exp(l1))
;;

let rec log_exp_of_prop = function
      C b -> Const b
    | V s -> Var s
    | Op (o,p) -> (let rec log_exp_of_Op o p= match o,p with
                  (Not,p) -> Neg (log_exp_of_prop p)
              in log_exp_of_Op o p)
    | BiOp (b,p1,p2) -> (let rec log_exp_of_BiOp b p1 p2 = match b,p1,p2 with
                  (Or,p1,p2)  -> Disj (log_exp_of_prop p1,log_exp_of_prop p2)
                | (And,p1,p2) -> Conj (log_exp_of_prop p1,log_exp_of_prop p2)
                | (If,p1,p2)  -> Cond (log_exp_of_prop p1,log_exp_of_prop p2)
                | (Iff,p1,p2) -> BiCond (log_exp_of_prop p1,log_exp_of_prop p2)
              in log_exp_of_BiOp b p1 p2 )
;;

(*B*)

let opval = function
    Not -> not
;;


let biopval = function
    Or  -> (||)
  | And -> (&&)
  | If  -> (fun p q -> ((not p) || q))
  | Iff -> (=)
;;


let rec peval ctx = function
    C b -> b
  | V s -> List.assoc s ctx
  | Op (o, p) -> (opval o) (peval ctx p)
  | BiOp (b, p1, p2) -> (biopval b) (peval ctx p1) (peval ctx p2)
;;



(*C*)

let rec simple_props = function 
    C b -> []
  | V s -> [s]
  | Op (o, p) -> simple_props p
  | BiOp (b, p1, p2) -> (simple_props p1) @ (simple_props p2)
;;



let rec set = function
    [] -> []
  | h::t -> if mem h t
            then set t
        else h :: (set t)
;;
    
let rec aux = function
    [] -> [[]]
  | h::t -> let aux2 = aux t in
            (List.map (function c -> (h,true)::c) aux2) @ (List.map (function c -> (h,false)::c) aux2)
;;

let is_tau p =
    let l = aux (set( simple_props p) ) in
    List.for_all (function c -> peval c p) l
;;