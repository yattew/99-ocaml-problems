let rec at n = function
    | a :: _ when n = 1 -> Some a
    | _ :: xs when n > 1 -> at (n-1) xs
    | _ -> None
