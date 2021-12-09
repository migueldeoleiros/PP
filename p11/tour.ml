open List;;

let notmem l e =
  not (mem e l);;

let is_in_board m n (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n;;

let legal_moves m n (x,y) visited =
  let all_moves = [x+1,y+2; x+2,y+1; x-1,y+2; x-2,y+1;
                   x+1,y-2; x+2,y-1; x-1,y-2; x-2,y-2]
in filter (notmem visited) (filter(is_in_board m n) all_moves);;

let tour m n x y =
  if not (is_in_board m n (x) && is_in_board m n (y))
    then raise (Invalid_argument "tour")
    else let rec aux solution = function
      [] -> raise Not_found
      | h::t -> if (h = y)
        then rev (h::solution)
        else try aux (h::solution) (legal_moves m n h (h::solution))
          with Not_found -> aux solution t
        in if x = y then [x]
          else aux [x] (legal_moves m n x [x]);;