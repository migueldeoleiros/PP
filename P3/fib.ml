let rec fib n = 
    if n <= 1 then n 
    else fib(n-1) + fib(n-2);;

let rec traza n =
    if n = 0
    then "0"
    else traza(n-1)^"\n"^string_of_int(fib(n));;	

let rec mensaje = 
    if (Array.length Sys.argv)=2
    then (traza (int_of_string(Sys.argv.(1))))
    else ("excedido el número de argumentos") in
    print_endline mensaje;;
