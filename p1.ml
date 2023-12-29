let rec last = function
    | [] -> None
    | [a] -> Some a
    | _::xs -> last xs
