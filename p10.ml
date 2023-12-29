let encode xs =
    let rec encode_inner ct curr acc = function
        | [] when ct = 0 -> acc
        | [] -> (ct,curr)::acc
        | a::xs when a = curr -> encode_inner (ct+1) curr acc xs
        | a::xs -> encode_inner 1 a ((ct, curr)::acc) xs
    in
    match xs with
    | [] -> []
    | a::xs'  -> encode_inner 1 a [] xs' 
    |> List.rev
