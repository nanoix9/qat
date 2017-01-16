let rec join ?(sep=" ") x =
    match x with
        | []      -> ""
        | [x]     -> x
        | x::xs   -> x ^ " "    ^ join xs
