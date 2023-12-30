type 'a rle =
    | One of 'a
    | Many of int * 'a;;


let encode xs = 
    let rec aux ct acc = function
        | [] -> acc
        | [x] -> 
                if ct = 0 then 
                    One x :: acc
                else
                    Many (ct + 1,x) :: acc
        | a :: (b::_ as t) ->
                if a = b then
                    aux (ct + 1) acc t
                else if ct = 0 then
                    aux 0 (One a :: acc) t
                else
                    aux 0 (Many (ct + 1, a) :: acc) t
    in
    aux 0 [] xs |> List.rev
