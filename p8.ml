let compress xs = 
    let rec compress_inner acc = function
        | [a] -> a::acc
        | a::b::xs when a = b -> compress_inner acc (b::xs)
        | a::b::xs when a != b -> compress_inner (a::acc) (b::xs) 
        | _ -> acc
    in
    compress_inner [] xs |> List.rev
