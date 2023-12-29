type 'a node =
    | One of 'a
    | Many of 'a node list;;

let rec flatten xs =
    let rec flatten_inner acc = function
        | [] -> acc
        | One x :: xs -> flatten_inner (x :: acc) xs
        | Many x :: xs -> flatten_inner (flatten_inner acc x) xs 
    in
    List.rev (flatten_inner [] xs)
