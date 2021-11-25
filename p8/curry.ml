(* curry : (('a * 'b) -> 'c) -> ('a -> ('b -> 'c)) *)
let curry =  function f -> function a -> function b -> f (a,b);;
let curry f a b = f (a,b);;

(* uncurry : (('a -> ('b -> 'c) -> ('a * 'b) -> 'c)) *)
let uncurry =  function f -> function (a,b) -> f a b;;
let uncurry f(a,b) = f a b;;

(* ---------------------------------------------------- *)

uncurry (+);;
(* - : int * int -> int = <fun> *)

let sum = (uncurry (+));;
(* val sum : int * int -> int = <fun> *)

(* sum 1;; *)
(* Error: expresión de tipo int, debería ser (int * int) *)

sum (2, 1);;
(* - : int = 3 *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(* g (2, 5);; *)
(* Error: expresión de tipo ('a * 'b), debería ser int *)

let h = g 2;;
(* val h : int -> int = <fun> *)

h 1, h 2, h 3;;
(* - : int * int * int = (7, 10, 13) *)
