let rec divide = function
  [] -> [], []
  | h::[] -> [h], []
  | h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2);;

let rec merge ord = function
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if (ord h1) h2 then h1 :: merge ord (t1, h2::t2)
                      else h2 :: merge ord (h1::t1, t2);;

let rec msort1 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
        merge ord (msort1 ord l1, msort1 ord l2);;

(* Puede ocurrir Stack Overflow, por ejemplo con: divide [1..256000] *)

let fromto m n =
  let rec auxl m n l =
    if n < m
      then l
      else auxl m (n - 1) (n::l)
  in auxl m n [];;

let l2 = fromto 1 256000;;

let divide' l =
  let rec aux dvd1 dvd2 = function
    [] -> (List.rev dvd1, List.rev dvd2)
    | h::[] -> (List.rev (h::dvd1), List.rev dvd2)
    | h1::h2::t -> aux (h1::dvd1) (h2::dvd2) t
  in aux [] [] l;;

let merge' ord (l1, l2) =
  let rec aux (a1, a2) mer = match a1, a2 with
    [], l | l, [] -> List.rev_append mer l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux (t1, h2::t2) (h1::mer)
                        else aux (h1::t1, t2) (h2::mer)
  in aux (l1, l2) [];;

let rec msort2 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide' l
in merge' ord (msort2 ord l1, msort2 ord l2);;

(* msort2 es un poco más lento que msort1 pero mucho más rápido que qsort2 *)
(* - para msort2 ordenar [1..10000] le lleva sobre 0.008646 *)
(* - para msort1 ordenar [1..10000] le lleva sobre 0.007983 *)
(* - para qsort2 ordenar [1..10000] le lleva sobre 2.428965 *)
(* Además para [1..100000] msort1 produce Stack Overflow y msort2 tarda sobre 0.13245 *)