type sym = string
type fullname = string list

type typ = {name: fullname; super: obj option}
and obj = {mutable t: obj; mutable v: value}
and value =
    | ValNil
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
    outer: env option;
    ns: fullname}

let name_root = [];;

let make_fullname name parent :fullname =
    match name with
    | "" -> parent
    | n -> n::parent
;;

let get_fullname ns =
    ns
;;

let str_of_fullname nm :string =
    String.concat "." (List.rev nm)
;;

let make_env name outer :env =
    let ns = match outer with
        | None -> make_fullname name name_root
        | Some e -> make_fullname name e.ns
    in
    {dict=Hashtbl.create 1; outer=outer; ns=ns}
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

let get_ns env :fullname =
    env.ns
;;
