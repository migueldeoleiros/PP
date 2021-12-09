let rec qsort1 ord = function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
    qsort1 ord before @ h :: qsort1 ord after;;

(* cuando la lista esté desbalanceada este algoritmo no será bueno *)

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              append' (qsort2 ord before) (h :: qsort2 ord after);;

(* Podemos usar funciones terminales como rev y rev_append  
   qsort2 es más rápido cuando la lista ya está inicialmente ordenada 
   qsort2 permite ordenar listas más grandes que qsort1*)

let init n f =
  let rec aux (i, l) =
    if i = n
      then l
    else aux(i + 1, f i::l)
  in List.rev(aux(0, []));;

let l1 = init 600000 (function x -> Random.int 5000);;

(* qsort2 es más lento que qsort1 cuando la lista está inicializada 
   aleatoriamente o inversamente. 
   En estos casos qsort2 fue un 117.45% más lento *)
