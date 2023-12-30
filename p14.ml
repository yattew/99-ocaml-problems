let duplicate xs = 
    let rec aux acc = function
        | [] -> acc
        | [x] -> x::x::acc
        | x::xs -> aux (x::x::acc) xs
    in
    aux [] xs |> List.rev
