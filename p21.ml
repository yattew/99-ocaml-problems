let rec insert_at el n = function
    | [] -> [el]
    | x::xs ->
            if n = 0 then el::x::xs
            else x::(insert_at el (n-1) xs)
