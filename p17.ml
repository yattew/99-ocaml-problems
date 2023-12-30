let split xs n =
    let rec aux ct acc = function
        | [] -> (List.rev acc, [])
        | x::xs' as xs -> 
                if ct = 0 then (List.rev acc, xs)
                else aux (ct-1) (x::acc) xs'
    in
    aux n [] xs
