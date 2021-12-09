open List;;

let notmem l e =
  not (mem e l);;

let is_in_board m n (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n;;

let legal_moves m n (x,y) visited =
  let all_moves = [x+1,y+2; x+2,y+1; x-1,y+2; x-2,y+1;
                   x+1,y-2; x+2,y-1; x-1,y-2; x-2,y-2]
in filter (notmem visited) (filter(is_in_board m n) all_moves);;

let knight_tour m n (x,y) =
  if (m < 0) || (n < 0) || (not (is_in_board m n (x,y)))
    then raise (Invalid_argument "knight_tour")
    else let num_cells = m*n
  in let rec aux i visited moves = match (i, visited, moves) with
      (0, _, _) -> raise Not_found
    | (i, vs, _) when i = num_cells -> rev vs
    | (i, _::vs, []::ms) -> aux(i-1) vs ms
    | (i, vs, (c::cs)::ms) -> aux (i+1) (c::vs) ((legal_moves m n c vs)::cs::ms)
in aux 1 [(x,y)] [(legal_moves m n (x,y) [])];;