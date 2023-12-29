let rev xs= 
    let rec rev_inner xs ys = 
        match xs with
        | [] -> ys
        | [a] -> a::ys
        | a :: xs' -> rev_inner xs' (a::ys)
    in
    rev_inner xs []
