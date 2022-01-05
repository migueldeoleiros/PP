type 'a g_tree =
    Gt of 'a * 'a g_tree list;;


(* val size : 'a g_tree -> int *)
(* devuelve el número de nodos de un g_tree *)
let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;


(* val height : 'a g_tree -> int *)
(* devuelve la "altura", como número de niveles, de un g_tree *)
let rec height =
  let rec list_max n = function
	  [] -> n
	| h::t -> let hh = height h in
        if hh > n then list_max hh t
		else list_max n t
  in function
	  Gt (_,[]) -> 1
    | Gt (_,l) -> 1 + list_max 0 l;;

	
(* val leaves : 'a g_tree -> 'a list *)
(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)
let rec leaves = function 
    Gt (r,[]) -> [r]
  | Gt (r,l) -> List.flatten (List.map leaves l);;

	
(* val mirror : 'a g_tree -> 'a g_tree *)
(* devuelve la imagen especular de un g_tree *)
let rec mirror = function 
  Gt (r,l) -> Gt(r, List.map mirror (List.rev l));;


(* val preorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "preorden" *)
let rec preorder = function 
    Gt (r,[]) -> [r] 
  | Gt (r,l) -> r::List.flatten (List.map preorder l);;


(* val postorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "postorden" *)
let rec postorder = function 
    Gt (r,[]) -> [r]
  | Gt (r,l) -> List.flatten (List.map postorder l) @ [r];;


(*
let t = Gt (2, [Gt (7, [Gt (2,[]); Gt (10,[]);
  Gt (6, [Gt (5,[]); Gt (11,[])])]);
  Gt (5, [Gt (9, [Gt (4, [])])])]);; 
*)
