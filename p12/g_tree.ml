type 'a g_tree =
    Gt of 'a * 'a g_tree list;;

(* type 'a bin_tree =
 *     Empty
 *   | Node of 'a * 'a bin_tree * 'a bin_tree;; *)

(* val size : 'a g_tree -> int *)
(* devuelve el número de nodos de un g_tree *)
let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;

let rec size = function
	Empty -> 0
  | Node (r,i,d) -> (size i) + (size d) + 1;;

(* val height : 'a g_tree -> int *)
(* devuelve la "altura", como número de niveles, de un g_tree *)

let rec height = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> (max (height h) (height (Gt (r,t))));;

let rec height = function
	Empty -> 0
  | Node (r,i,d) -> (max (height i) (height d)) +1;;
	
(* val leaves : 'a g_tree -> 'a list *)
(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)

let rec leaves = function 
    Gt (_,[]) -> []
  | Gt (r,[]::[]) -> [r]
  | Gt (r,h::t) -> leaves h + leaves (Gt (r,t));;

let rec leaves = function
	Empty -> []
  | Node (r,Empty,Empty) -> [r]
  | Node (r,i,d) -> leaves i @ leaves d;;
	
(* val mirror : 'a g_tree -> 'a g_tree *)
(* devuelve la imagen especular de un g_tree *)

let rec mirror = function
	Empty -> Empty
  | Node (r,i,d) -> Node (r,d,i);;

(* val preorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "preorden" *)

let rec preorder = function
	Empty -> []
  | Node (r,i,d) -> r::(preorder i) @ (preorder d);;

(* val postorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "postorden" *)

let rec postorder = function
	Empty -> []
  | Node (r,i,d) -> (postorder i) @ (postorder d) @ [r];;
