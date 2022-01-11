type 'a g_tree =
    Gt of 'a * 'a g_tree list;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

(* version terminal de breadth_first *)
let rec breadth_first_t t =
  let rec aux acum = function
      Gt (x, []) -> List.rev (x::acum)
    | Gt (x, (Gt (y, t2))::t1) -> aux (x::acum) (Gt (y, List.rev_append (List.rev t1) t2))
  in aux [] t;;
