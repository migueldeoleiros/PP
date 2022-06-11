let e1 =
    let pi = 2. *. asin 1. in pi *. (pi +. 1.);;

let e2 =
    let lg2 = log 2. in
    let log2 = function x -> log x /. lg2
    in log2 (float (1024 * 1024));;

let e3 =
    let pi_2 = 4. *. asin 1. in
    function r -> pi_2 *. r;;

let e4 =
    let sqr = function x -> x *. x in
    let pi = 2. *. asin 1. in
    function r -> pi *. sqr r;;
