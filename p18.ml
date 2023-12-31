let slice xs s e =
    let rec aux ct1 ct2 acc = function
        | [] -> acc
        | x::xs ->
                if ct1 = 0 then
                    if ct2 > 0 then aux ct1 (ct2-1) (x::acc) xs
                    else acc
                else
                    aux (ct1-1) ct2 acc xs
                
    in
    aux s (e-s+1) [] xs |> List.rev
