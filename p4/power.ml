let rec power x n = 
    if n = 0 then 1 
    else x * power x (n-1)

let rec power' x n = 
    if n = 0 then 1 
    else if (n mod 2 = 0) then power' (x*x) (n/2)
    else x * power' (x*x) (n/2)

(*
La función power' realiza menos operaciones dado que reduce n a la mitad recursivamente.
Esto será notable en caso de números grandes, que con int se salen de rango
 *)

let rec powerf x n = 
    if n = 0 then 1. 
    else if (n mod 2 = 0) then powerf' (x*.x) (n/2)
    else x *. powerf' (x*.x) (n/2)
