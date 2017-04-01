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

let builtin = "builtin";;

let rec obj_o = {t=type_o; v=ValType {name=[builtin; "object"]; super=None}}
and type_o = {t=type_o; v=ValType {name=[builtin; "type"]; super=Some obj_o}}
;;

let make_obj (t :obj) (v :value) :obj =
    assert (t.t == type_o);
    {t=t; v=v}
;;

type env = {dict: (sym, obj) Hashtbl.t;
    outer: env option}

let none = make_obj type_o ValNone
;;

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

