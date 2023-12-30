type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode xs = 
    let rec aux acc = function
        | [] -> acc
        | One x :: xs -> aux (x::acc) xs
        | Many (ct, a) :: xs -> aux (
            let rec aux' ct a acc =
                if ct = 0 then acc
                else aux' (ct-1) a (a :: acc)
            in
            aux' ct a acc
        ) xs
    in 
    aux [] xs |> List.rev
