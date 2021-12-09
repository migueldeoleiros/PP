open List;;

let rec qsort1 ord = function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
            qsort1 ord before @ h :: qsort1 ord after;;

(* No será buena esta implementación para casos *)
   (* en los que la lista esté desbalanceada *)

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              append' (qsort2 ord before) (h :: qsort2 ord after);;

(* La ventaja de no usar @ es usar funciones terminales como son: rev_append y rev *)
(* además qsort2 es más rápido cuando la lista ya está inicialmente ordenada *)
(* qsort2 permite ordenar listas más grandes que qsort1 sin dar Stack Overflow *)

let init n f =
  let rec aux (i, l) =
    if i = n
      then l
      else aux(i + 1, f i::l)
    in rev(aux(0, []));;

let l1 = init 600000 (function x -> Random.int 5000);;

   (* qsort2 es más lento que qsort1 cuando: *)
(* - la lista está inicializada aleatoriamente *)
(* - la lista está inicializada inversamente *)
   (* En estos casos qsort2 fue un 117.45% más lento *)