let rec remove_at n = function
    | [] -> []
    | x::xs ->
            if n = 0 then xs
            else x::(remove_at (n-1) xs) 
