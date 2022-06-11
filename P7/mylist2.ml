let hd l = match l with
    | h::_ -> h
    | [] -> raise (Failure "hd");;


let tl l = match l with
    | _::t-> t
    | [] -> raise (Failure "tl");;


let length l =
    let rec aux a = function
        [] -> a
        | h::t -> aux (a+1) t
    in aux 0 l;;


let rec compare_lengths  l1 l2 = 
    match l1,l2 with 
        ([],[]) -> 0
        |(_,[]) -> 1
        |([],_) -> -1
        |(_::t1,_::t2) -> 
            compare_lengths t1 t2;;


let rec nth l n = 
    if(n == 0) then hd l 
    else if(n > 0) then nth (tl l)(n-1)
    else if(n < 0) then raise (Invalid_argument "List.nth")
    else raise (Failure "nth");;


let rec append l1 l2 = 
    if l1=[] then l2
    else (hd l1)::(append(tl l1) l2);;

(*-------------------------------*)
let rev l =
    let rec aux l a = match l with
        [] -> a
        | h::t -> aux t (h::a)
    in aux l [];; 


let init n f= 
    if n<0 then raise (Invalid_argument "init")
    else let rec aux acc i=
        if i=n then rev acc
        else aux (f i::acc) (i+1)
    in aux[] 0;;


let rev_append l1 l2 =
  let rec aux l1 l = match l1 with
    [] -> l
    | h::t -> aux t (h::l)
  in aux l1 l2;;


let rec concat = function
    [] -> []
    | h::t -> append h (concat t);;


let flatten = concat;;


let rec map f = function
    [] -> []
    | h::t -> (f h) :: (map f t);;


let rev_map f l =
    let rec aux l auxl = match l with
        [] -> auxl
        | h::t -> aux t (f(h)::auxl)
    in aux l [];;


let rec map2 f l1 l2 =
    if (length l1 != length l2)
        then raise (Invalid_argument"map2")
    else if (length l1 == 0) then []
    else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;


let rec fold_left f a = function
    [] -> a
    | h::t -> fold_left f (f a h) t;;


let rec fold_right f l a =
    match l with
        [] -> a
        | h::t -> f h (fold_right f t a);;

(*----------------------------------*)

let rec find p = function
    [] -> raise Not_found
    | h::t -> if p h then h 
              else find p t;;


let rec for_all p = function
    [] -> true
    | h::t -> (p h) && (for_all p t);;


let rec exists p = function
    [] -> false
    | h::t -> (p h) || (exists p t);;


let rec mem p = function
    [] -> false
    | h::t -> if (p = h) then true
              else (mem p t);;


let filter p l =
  let rec aux si no = function
      [] -> rev si
      | h::t -> if (p h) then aux (h::si) no t
                else aux si no t
  in aux [] [] l;;


let find_all = filter;;


let partition p l =
  let rec aux si no = function
      [] -> (rev si, rev no)
      | h::t -> if (p h) then aux (h::si) no t
                else aux si (h::no) t
  in aux [] [] l;;


let rec split = function
    [] -> ([],[])
    | (h1,h2)::t -> let t1,t2 = 
        split t in h1::t1,h2::t2;;


let rec combine l1 l2 =
    match (l1,l2) with
      [], [] -> []
      | h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
      | _ -> raise (Invalid_argument "combine");;
