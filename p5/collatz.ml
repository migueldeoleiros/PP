let f n =
    if n mod 2 = 0 then n / 2
    else 3 * n + 1;;

let rec orbit f n =
    print_int n;
    if n = 1 then print_newline()
    else (print_string", "; 
        orbit (f n));;

let rec lenght n = 
    if orbit
;;
    
let rec length'n'top n =
    if n = 1  then (0,1)
    else let l,t = length'n'top (f n)
    in (l+1, max n t);;


