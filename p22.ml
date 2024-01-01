let range a b =
    let rec aux a b = 
        if a <= b then a::(aux (a+1) b)
        else []
    in
    if a > b then aux b a |> List.rev
    else aux a b
