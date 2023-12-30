let replicate xs n = 
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (
                let rec dup n x acc =
                    if n = 0 then acc
                    else
                        dup (n-1) x (x :: acc)
                in dup n x acc)
                xs
    in 
    aux [] xs |> List.rev
