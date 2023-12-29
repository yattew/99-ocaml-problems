let pack xs =
    let rec aux current acc = function
        | [] -> []
        | [x] -> (x :: current) :: acc
        | a::(b::_ as t) when a = b -> aux (a::current) acc t
        | a::(b::_ as t)  -> aux [] ((a::current)::acc) t
    in 
    aux [] [] xs |> List.rev
