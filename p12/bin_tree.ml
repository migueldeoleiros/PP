type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Implementar sum, prod, size, inorder y mirror usando fold_tree *)

(* let rec sum = function
 *     Empty -> 0
 *   | Node (x, l, r) -> x + (sum l) + (sum r);; *)

let sum = 
  let aux x l r =
    x + l + r
  in fold_tree aux 0;;


(* let rec prod = function
 *     Empty -> 1.0
 *   | Node (x, l, r) -> x *. (prod l) *. (prod r);; *)

let prod = 
  let aux x l r =
    x *. l *. r
  in fold_tree aux 1.0;;


(* let rec size = function
 * 	Empty -> 0
 *   | Node (x, l, r) -> (size l) + (size r) + 1;; *)

let size = 
  let aux x l r =
    l + r + 1
  in fold_tree aux 0;; 


(* let rec inorder = function
 * 	Empty -> []
 *   | Node (x,l,r) -> (inorder r) @ [x] @ (inorder l);; *)

let inorder =
  let aux x l r =
    l @ [x] @ r
  in fold_tree aux [];;


(* let rec mirror = function
 * 	Empty -> Empty
 * 	| Node (x, l, r) -> Node (x, r, l);; *)

let mirror =
  let aux x l r =
    Node (x, r, l)
  in fold_tree aux Empty;;


(*
let t = Node (3, Node (8, Empty, Empty),
                 Node (2, Node (5, Empty, Empty),
                          Node (1, Empty, Empty)));;

let rec map_tree f = function
    Empty -> Empty
  | Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r)
in prod (map_tree float t);;

*)
