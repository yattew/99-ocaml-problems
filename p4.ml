let  length xs = 
    let rec length_inner n = function
        | [] -> n
        | _ :: xs' -> length_inner (n + 1) xs' 
    in 
        length_inner 0 xs 
