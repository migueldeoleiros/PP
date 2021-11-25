let f n =
    if n mod 2 = 0 then n / 2
    else 3 * n + 1;;


let rec orbit f n =
    print_int n;
    if n = 1 then print_newline()
    else (print_string", "; 
        orbit (f n));;


let rec length n=
    if n=1 then 0
    else 1+length(f n);;


let rec top n=
    if n=1 then 0
    else max n (top(f n));;
    

let rec length'n'top n =
    if n = 1  then (0,1)
    else let l,t = length'n'top (f n)
    in (l+1, max n t);;

(*-----------------------------------------*)

let rec longest_in m n=
    let rec aux i=
        if i<=m then (m,length m)
        else let (j,lj)= aux (i-1) in
        let li = length i in
        if lj >= li then (j,lj) else (i,li)
    in fst (aux n);;


let rec highest_in m n=
    let rec aux i=
        if i<=m then (m,top m)
        else let (j,tj)= aux (i-1) in
        let ti = top i in
        if ti > tj then (i,ti) else (j,tj)
      in fst (aux n);;
