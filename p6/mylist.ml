let hd l = match l with
    | h::_ -> h
    | [] -> raise (Failure "hd");;
    

let tl l = match l with
    | _::t-> t
    | [] -> raise (Failure "tl");;


let rec length = function
    [] -> 0
    | _::t -> 1 + length t;;


let rec compare_lengths  l1 l2 = 
    match l1,l2 with 
        ([],[]) -> 0
        |(_,[]) -> 1
        |([],_) -> -1
        |(_::t1,_::t2) -> 
            compare_lengths t1 t2;;


let nth l n =  
    if n<0 then raise(Invalid_argument "nth")
    else let rec aux = function
        ([],_) -> raise(Failure "uth")
        |(h::_,0) -> h 
        |(_::t,i) -> aux(t,i-1) in aux(l,n);;


let rec append l1 l2 = 
    if l1=[] then l2
    else (hd l1)::(append(tl l1) l2);;


(*-----------------------*)

let rec find p = function
    [] -> raise Not_found
    | h::t -> if p h then h else find p t;;		


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

let rec filter p = function
    [] -> []
    | h::t -> if (p h) then h::(filter p t)
			else (filter p t);;


let find_all = filter;;


let rec partition p = function
    [] -> ([],[])
    | h::t -> let (si,no) = partition p t
			in if (p h) then (h::si,no)
				else (si,h::no);;


let rec split = function
    [] -> ([],[])
    | (h1,h2)::t -> let t1,t2 = split t in
		h1::t1,h2::t2;;


let rec combine l1 l2 =
    match (l1,l2) with
        [], [] -> []
        | h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
        | _ -> raise (Invalid_argument "combine");;

(*--------------------------------*)
let rec rev = function
    [] -> []
    | h::t -> append (rev t) [h];;


let init n f= 
    if n<0 then raise (Invalid_argument "init")
    else let rec aux acc i=
        if i=n then rev acc
        else aux (f i::acc) (i+1)
    in aux[] 0;;


let rev_append l1 l2 =
    if l1=[] then []
    else append (rev l1) l2;;


let rec concat = function
    [] -> []
    | h::t -> append h (concat t);;


let flatten = concat;;


let rec map f = function
    [] -> []
    | h::t -> (f h) :: (map f t);;


let rev_map f l = rev (map f l);;


let rec fold_left f a = function
    [] -> a
    | h::t -> fold_left f (f a h) t;;


let rec fold_right f l a =
    match l with
        [] -> a
        | h::t -> f h (fold_right f t a);;

