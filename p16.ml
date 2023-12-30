let drop xs n =
    let rec aux curr acc = function
        | [] -> acc
        | x::xs -> 
                if curr = 1 then aux n acc xs
                else aux (curr-1) (x::acc) xs
    in
    aux n [] xs |> List.rev
