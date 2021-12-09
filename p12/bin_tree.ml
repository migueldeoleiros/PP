type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Implemente sum, prod, size, inorder y mirror usando fold_tree *)

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
