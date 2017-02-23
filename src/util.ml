let println = Printf.printf "%s\n"
;;

let rec join ?(sep=" ") x =
    match x with
    | []      -> ""
    | [x]     -> x
    | x::xs   -> x ^ sep ^ join ~sep:sep xs

let rec joina ?(sep=" ") x =
    String.concat sep (Array.to_list x)

let join_da sep da =
    String.concat sep (DynArray.to_list da)

module StrMap = Map.Make(String)
;;

let merge_str_map a b :'a StrMap.t =
    StrMap.union (fun s a1 a2 -> Some a1) a b
;;

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

