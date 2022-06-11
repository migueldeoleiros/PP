let is_prime n = 
    let rec check_from i =
        i >= n ||
        (n mod i <> 0 && check_from (i+1))
    in check_from 2;;


let rec next_prime n =
    if is_prime (n+1) then n+1
    else next_prime (n+1);;


let rec last_prime_to n =
    if is_prime (n) then n
    else last_prime_to (n-1);;


let is_prime2 n = 
    let rec check_from i =
        (float_of_int i) >= (sqrt (float_of_int n))+.1. ||
        (n mod i <> 0 && check_from (i+1))
    in check_from 2;;

(*is_prime2 1000000007;;*)
(*
is_prime2 se ejecuta instantaneamente mientras que 
su contraparte menos eficiente tarda 20 segundo para
este ejemplo en mi máquina
*)
