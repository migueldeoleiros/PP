(* para tres primeros tipos hay solo una solución*)

(* 'a -> 'a *)
let f a = a;;

(* 'a * 'b -> 'a *)
let h (a,b)= a;;

(* 'a * 'b -> 'b *)
let i (a,b)= b;;

(* 'a -> 'a list *)
let j a = a :: [];; 
(* para este tipo hay infinitas respuestas,
   ya que puedes hacer una lista de infinitos elementos 'a*)
