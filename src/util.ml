let rec join ?(sep=" ") x =
    match x with
    | []      -> ""
    | [x]     -> x
    | x::xs   -> x ^ sep ^ join ~sep:sep xs

let rec joina ?(sep=" ") x =
    String.concat sep (Array.to_list x)

module StrMap = Map.Make(String)
let str_of_strmap str_of_value smap =
    "{" ^ (join ~sep:", " (StrMap.fold (fun k v lst ->
            (k ^ ":" ^ (str_of_value v))::lst)
            smap [])) ^ "}"
;;

let opt_equal cmp o1 o2 =
    match o1, o2 with
    | None, None -> true
    | Some v1, Some v2 -> cmp v1 v2
    | _ -> false

