(*Necesitaba rev, hd y tl para las otras funciones*)
let rev l =
	let rec aux l a = match l with
	      [] -> a
	    | h::t -> aux t (h::a)
    in aux l [];; 

let hd l = match l with
    | h::_ -> h
    | [] -> raise (Failure "hd");;


let tl l = match l with
    | _::t-> t
    | [] -> raise (Failure "tl");;

(*-----------------------------------*)


let rec remove a l = match l with 
  [] -> l
| h::t -> if (a==h) then t
	  else h::(remove a t);;


let rec remove_all a l = match l with
	[] -> []
| h::t -> if (a==h) then (remove_all a t)
	  else h::(remove_all a t);;



let rec ldif l1 l2 = match (l1,l2) with
  h1::t1, h2::t2 -> ldif (remove_all h2 l1) t2
| _ -> l1;;


let lprod l1 l2 =	
	let rec aux acc = function
		  [],_ -> rev acc
		| _::t1,[] -> aux acc (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::acc) (h1::t1,t2)
	in aux [] (l1,l2);;


let divide l =
  let rec aux acc acc2 = function
					  [h] -> (rev (h::acc), rev acc2)
					| [] -> (rev acc, rev acc2)
					| h::t -> aux (h::acc) (hd t::acc2) (tl t)
				in aux [] [] l;;
