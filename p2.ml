let rec last_two = function
    | [] -> None
    | a :: [b] -> Some (a,b)
    | _ :: xs -> last_two xs
