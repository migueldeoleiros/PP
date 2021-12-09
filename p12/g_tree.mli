type 'a g_tree = Gt of 'a * 'a g_tree list

val size : 'a g_tree -> int
(* devuelve el número de nodos de un g_tree *)

val height : 'a g_tree -> int
(* devuelve la "altura", como número de niveles, de un g_tree *)

val leaves : 'a g_tree -> 'a list
(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)

val mirror : 'a g_tree -> 'a g_tree
(* devuelve la imagen especular de un g_tree *)

val preorder : 'a g_tree -> 'a list
(* devuelve la lista de nodos de un g_tree en "preorden" *)

val postorder : 'a g_tree -> 'a list
(* devuelve la lista de nodos de un g_tree en "postorden" *)
