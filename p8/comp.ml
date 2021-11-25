(* comp: ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b) *)
let comp f g x = 
    f (g x);;


(* ---------------------------------------------- *)

let f = 
    let square x = x * x 
    in comp square ((+) 1);;
(* val f : int -> int = <fun> *)


f 1, f 2, f 3;;
(* - : int * int * int = (4, 9, 16) *)
