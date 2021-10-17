let rec mcd (x, y) =
    if y = 0 then x
    else mcd (x mod y) y
