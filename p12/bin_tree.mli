type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree

val fold_tree : ('a -> 'b -> 'b -> 'b) -> 'b -> 'a bin_tree -> 'b
(* generaliza operaciones de reducción sobre valores de tipo bin_tree *)

val sum : int bin_tree -> int
(* devuelve la suma de los nodos de un int bin_tree *)

val prod : float bin_tree -> float
(* devuelve el producto de los nodos de un float bin_tree *)

val size : 'a bin_tree -> int
(* devuelve el número de nodos de un bin_tree *)

val inorder : 'a bin_tree -> 'a list
(* devuelve la lista de nodos de un bin_tree en "orden" *)

val mirror : 'a bin_tree -> 'a bin_tree
(* devuelve la imagen especular de un bin_tree *)
