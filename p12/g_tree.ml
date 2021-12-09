type 'a g_tree = Gt of 'a * 'a g_tree list;;

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

(* val leaves : 'a g_tree -> 'a list *)
(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)

(* val mirror : 'a g_tree -> 'a g_tree *)
(* devuelve la imagen especular de un g_tree *)

(* val preorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "preorden" *)

(* val postorder : 'a g_tree -> 'a list *)
(* devuelve la lista de nodos de un g_tree en "postorden" *)


let rec sum = function
    Empty -> 0
  | Node (r, lb, rb) -> r + (sum lb) + (sum rb);;

let rec prod = function
    Empty -> 1.0
  | Node (r, lb, rb) -> r *. (prod lb) *. (prod rb);;

let rec size = function
	Empty -> 0
	| Node (r,i,d) -> (size i) + (size d) + 1;;

let rec inorder = function
    Empty -> []
  | Node (r, lb, rb) -> inorder lb @ (r :: inorder rb);;

let rec mirror = function
	Empty -> Empty
	| Node (r,i,d) -> Node (r,d,i);;
