let rec split acc n = function
    | [] -> List.rev acc, []
    | x::xs' as xs -> 
            if n = 0 then List.rev acc, xs
            else split (x::acc) (n-1) xs'

let rotate xs n = 
    let len = List.length xs in
    let pivot = ((n mod len) + len) mod len in
    if n = 0 then xs
    else let a, b = split [] pivot xs in b @ a
