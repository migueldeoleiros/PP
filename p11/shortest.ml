open List;;

let notmem l e =
  not (mem e l);;

let is_in_board m n (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n;;

let legal_moves m n (x,y) visited =
  let all_moves = [x+1,y+2; x+2,y+1; x-1,y+2; x-2,y+1;
                   x+1,y-2; x+2,y-1; x-1,y-2; x-2,y-2]
in filter (notmem visited) (filter(is_in_board m n) all_moves);;
