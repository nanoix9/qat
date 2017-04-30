type sym = string
type fullname = string list

type q_type = {name: fullname; super: q_obj option}
and q_obj = {mutable t: q_obj; mutable v: value}
and value =
    | ValNil
    | ValInt of int
    | ValFloat of float
    | ValStr of string
    | ValBool of bool
    | ValArr of q_obj array
    | ValDict of (q_obj, q_obj) Hashtbl.t
    | ValType of q_type
    (*| ValClosure of closure*)
;;

let eq_value v1 v2 :bool =
    match v1, v2 with
    | ValNil, ValNil -> true
    | ValInt i1, ValInt i2 -> i1 = i2
    | ValFloat f1, ValFloat f2 -> f1 = f2
    | ValStr s1, ValStr s2 -> s1 = s2
    | ValBool b1, ValBool b2 -> b1 = b2
    (*| ValArr a1, ValArr a2 -> *)

;;

let eq_q_obj o1 o2 :bool =
    o1.t == o2.t && eq_value o1.v o2.v
;;

type env = {dict: (sym, q_obj) Hashtbl.t;
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

let make_sub_env name outer :env =
    make_env name (Some outer)
;;

let set env sym q_obj :unit =
    Hashtbl.replace env.dict sym q_obj
;;

let get env sym :q_obj =
    Hashtbl.find env.dict sym
;;

let mem env sym :bool =
    Hashtbl.mem env.dict sym
;;

let get_ns env :fullname =
    env.ns
;;
