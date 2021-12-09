type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Implemente sum, prod, size, inorder y mirror usando fold_tree *)

let sum = ... ;;

let prod = ... ;;

let size = ... ;;

let inorder = ...;;

let mirror = ... ;;
