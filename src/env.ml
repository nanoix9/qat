type sym = string
type fullname = string list

type typ = {name: fullname; super: obj option}
and obj = {mutable t: obj; mutable v: value}
and value =
    | ValNone
    | ValInt of int
    | ValFloat of float
    | ValStr of string
    | ValBool of bool
    | ValArr of obj array
    | ValDict of (obj, obj) Hashtbl.t
    | ValType of typ
    (*| ValClosure of closure*)
;;

type env = {dict: (sym, obj) Hashtbl.t;
    outer: env option}

let make_env outer :env =
    {dict=Hashtbl.create 1; outer=outer}
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

