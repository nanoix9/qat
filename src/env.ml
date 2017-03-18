type sym = string
type fullname = string list
type typ = Typ of fullname
type value =
    | ValNone
    | ValInt of int
    | ValStr of string
type obj = {t: typ; v: value}

type env = {dict: (sym, obj) Hashtbl.t;
    parent: env option}

let typ_obj = Typ ["Object"]
let none = {t=typ_obj; v=ValNone}

let make_env parent :env =
    {dict=Hashtbl.create 1; parent=parent}
;;

let set env sym obj :unit =
    Hashtbl.replace env.dict sym obj
;;

let get env sym :obj =
    Hashtbl.find env.dict sym
;;

let mem env sym :bool =
    Hashtbl.mem env.dict sym
;;

