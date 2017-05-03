open Ast

type sym = string
type fullname = string list

type env = {dict: (sym, q_obj) Hashtbl.t;
    outer: env option;
    ns: fullname}
and q_obj = {mutable t: q_obj; mutable v: value}
and q_type = {name: fullname; super: q_obj option}
and func_impl = {params: (q_obj * sym) list; env :env; body :ast}
and impl_tbl =
    | FuncTbl of (fullname, impl_tbl) Hashtbl.t
    | FuncImpl of func_impl
and closure = {name: fullname; impls :impl_tbl}
and value =
    | ValNil
    | ValInt of int
    | ValFloat of float
    | ValStr of string
    | ValBool of bool
    | ValArr of q_obj array
    | ValDict of (q_obj, q_obj) Hashtbl.t
    | ValType of q_type
    | ValClosure of closure
;;

let rec eq_q_obj o1 o2 :bool =
    o1.t == o2.t && eq_value o1.v o2.v
and eq_q_type t1 t2 :bool =
    t1.name = t2.name &&
    (match t1.super, t2.super with
    | None, None -> true
    | Some o1, Some o2 -> eq_q_obj o1 o2
    | _ -> false)
and eq_value v1 v2 :bool =
    match v1, v2 with
    | ValNil, ValNil -> true
    | ValInt i1, ValInt i2 -> i1 = i2
    | ValFloat f1, ValFloat f2 -> f1 = f2
    | ValStr s1, ValStr s2 -> s1 = s2
    | ValBool b1, ValBool b2 -> b1 = b2
    (*| ValArr a1, ValArr a2 -> *)
    | ValType t1, ValType t2 -> eq_q_type t1 t2
;;

let name_root = [];;

let make_fullname name parent :fullname =
    match name with
    | "" -> parent
    | n -> n::parent
;;

let rec fullname_of_list lst :fullname =
    match lst with
    | [] -> name_root
    | x::xs -> make_fullname x (fullname_of_list xs)
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

let make_func name =
    {name=name; impls=FuncTbl (Hashtbl.create 100)}
;;

let make_func_impl params body env =
    {params=params; env=env; body=body}
;;

let rec get_impl_tbl impls types :func_impl option =
    match impls, types with
    | Some (FuncTbl tbl), tp::ts -> get_impl_with_supers tbl tp ts
    | Some (FuncImpl imp), [] -> Some imp
    | _ -> None
and get_impl_with_supers tbl tp ts :func_impl option =
    let name, super = match tp.v with
        | ValType v -> v.name, v.super
    in
    let imp = if Hashtbl.mem tbl name then
            get_impl_tbl (Some (Hashtbl.find tbl name)) ts
        else
            None
    in
    match imp with
    | None ->
        (match super with
        | None -> None
        | Some sup -> get_impl_with_supers tbl sup ts)
    | _ -> imp
;;

let get_func_impl func param_types =
    get_impl_tbl (Some func.impls) param_types
;;

(*let add_impl_to_func func impl =*)
    (*func.impls*)
