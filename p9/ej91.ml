let rec to0from n =
    if n < 0 then []
    else n :: to0from (n-1);;

let to0from n =
    if n < 0 then []
    else  
    


let rec fromto m n =
    if m > n then []
    else m :: fromto (m+1) n;;
    

let rec from1to n =
    if n < 1 then []
    else from1to (n-1) @ [n];;


let map =
    List.map;;

let rec map f = function
      [] -> []
    | h::t -> (f h) :: (map f t);;


let power x y =
    let rec innerpower x y =
        if y = 0 then 1
        else x * innerpower x (y-1)
    in
        if y >= 0 then innerpower x y
        else invalid_arg "power";;


let incseg l =
    List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;
    

let rec remove x = function
    [] -> []
    | h::t -> if x = h then t
        else h :: remove x t;;


let divide l =
    let rec aux acc acc2 = function
					  [h] -> (List.rev (h::acc), List.rev acc2)
					| [] -> (List.rev acc, List.rev acc2)
					| h::t -> aux (h::acc) (List.hd t::acc2) (List.tl t)
				in aux [] [] l;;


let rec compress = function
    | h1::h2::t -> if h1 = h2 then compress (h2::t)
        else h1 :: compress (h2::t)
    | l -> l;;

