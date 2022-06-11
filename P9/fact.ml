let rec fact = function
	0 -> 1 
  | n -> n * fact (n - 1);;
try
	print_endline (string_of_int (fact (int_of_string Sys.argv.(1))))
with
    Failure _ 
  | Stack_overflow 
  | Invalid_argument _ -> print_endline "argumento invalido"
