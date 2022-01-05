(* let rec to0from n =
 *   if n < 0 then []
 *   else n :: to0from (n-1);; *)
   
let to0from n = 
  let rec aux l i =
    if i < 0 then List.rev l
    else aux (i::l) (i - 1)
  in aux [] n;;


(* let rec fromto m n =
 *   if m > n then []
 *   else m :: fromto (m+1) n;; *)

let fromto m n = 
  let rec aux l i =
    if i < m then l
	else aux (i::l) (i - 1)
  in aux [] n;;


(* let rec from1to n =
 *   if n < 1 then []
 *   else from1to (n-1) @ [n];; *)


(* let map =
 *   List.map;; *)

let map f l =
  let rec aux f l acc = match l with
      [] -> List.rev acc
	| h::t -> aux f t ((f h)::acc)		
  in aux f l [];;


(* let power x y =
 *   let rec innerpower x y =
 *     if y = 0 then 1 
 *     else x * innerpower x (y-1)
 *   in
 * 	if y >= 0 then innerpower x y
 * 	else invalid_arg "power";; *)

let power x y =
  let rec innerpower x y acc =
    if y = 0 then acc
    else innerpower x (y - 1) (x * acc)
  in
	if y >= 0 then innerpower x y 1
	else invalid_arg "power";;


(* let incseg l =
 *   List.fold_right (fun x t -> x::List.map ((+) x) t) l [];; *)

let incseg l = 
  let rec aux l acc l2 = match l with
      [] -> []
	| [h] -> List.rev ((h + acc)::l2)
    | h::t -> aux t (h + acc) ((h + acc)::l2)
  in aux l 0 [];;


(* let rec remove x = function
 *     [] -> []
 *   | h::t -> if x = h then t
 *             else h :: remove x t;; *)

let rec remove x l =
  let rec aux acc = function
      [] -> l
    | h::t -> if x = h then List.rev_append acc t
              else aux (h::acc) t 
  in aux [] l ;;


(* let rec divide = function
 *     h1::h2::t -> let  l1, l2 = divide t in h1::l1,h2::l2
 *   | l -> l, [];; *)

(* TODO *)
let rec divide = function
    h1::h2::t -> let  l1, l2 = divide t in h1::l1,h2::l2
  | l -> l, [];;


(* let rec compress = function
 *   | h1::h2::t -> if h1 = h2 then compress (h2::t)
 *                  else h1 :: compress (h2::t)
 *   | l -> l;; *)

let compress l =
  let rec aux acc l = match l with
        | h1::h2::t -> if h1=h2 then aux acc (h2::t)
                       else aux (h1::acc) (h2::t)
        | [h] -> aux (h::acc) []
        | []-> List.rev acc
  in (aux [] l);;

(*--------------------------------------*)
(* IN-PROCESS *)
let fact n =
	let rec innerfact n acc =
		if n=0 then acc
		else innerfact (n-1) (acc *. float n)
	in
	if n>=0 then innerfact n 1.
	else invalid_arg "fact";;
