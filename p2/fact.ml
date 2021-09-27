let rec factorial n =
    if n = 0.0 then 1.0 else n *. factorial (n -. 1.0) 
