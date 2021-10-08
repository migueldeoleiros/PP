let rec fact = function
  0 -> 1
  | n -> n * fact (n- 1)
let size = Array.length Sys.argv
let _ =
  if size <> 2 then print_endline "error"
  else print_endline(string_of_int(fact(int_of_string(Array.get Sys.argv(1)))))