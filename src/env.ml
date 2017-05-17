open Ast
open Big_int

exception EnvErr of string;;

type sym = string
type fullname = string list

type env = {dict: (sym, q_obj) Hashtbl.t;
    outer: env option;
    ns: fullname}
and q_obj = {mutable t: q_obj; mutable v: value}
and q_type = {name: fullname; super: q_obj option}
and func_impl = {name: fullname;
    params: (q_obj * sym) list;
    env :env;
    body :func_body}
and func_body =
    | FuncBodyEstmt of estmt
    | FuncBodyInst of (q_obj list -> q_obj)
and impl_tbl = {tbl :(fullname, impl_tbl) Hashtbl.t;
    mutable impl :func_impl option}
and closure = {name: fullname; impls :impl_tbl}
(*TODO: can the type `value` be simplified, i.e. less number of types of values?*)
and value =
    | ValNil
    | ValInt of big_int
    | ValFloat of float
    | ValStr of string
    | ValBool of bool
    | ValArr of q_obj array
    | ValDict of (q_obj, q_obj) Hashtbl.t
    | ValType of q_type
    | ValClosure of closure
    | ValScope of env
and eatom =
    | Sym of sym
    | Obj of q_obj
and estmt = eatom abs_tree
;;

let obj_to_int (j :q_obj) :big_int =
    match j.v with
    | ValInt i -> i
    | _ -> raise (EnvErr "not a int")
;;

let obj_to_float (j :q_obj) :float =
    match j.v with
    | ValFloat i -> i
    | _ -> raise (EnvErr "not a float")
;;

let obj_to_str (j :q_obj) :string =
    match j.v with
    | ValStr i -> i
    | _ -> raise (EnvErr "not a string")
;;

let obj_to_bool (j :q_obj) :bool =
    match j.v with
    | ValBool i -> i
    | _ -> raise (EnvErr "not a bool")
;;

let obj_to_type (j :q_obj) :q_type =
    match j.v with
    | ValType i -> i
    | _ -> raise (EnvErr "not a q_type")
;;

let obj_to_closure (j :q_obj) :closure =
    match j.v with
    | ValClosure i -> i
    | _ -> raise (EnvErr "not a function")
;;

let obj_to_scope (j :q_obj) :env =
    match j.v with
    | ValScope i -> i
    | _ -> raise (EnvErr "not a scope")
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
    | ValInt i1, ValInt i2 -> eq_big_int i1 i2
    | ValFloat f1, ValFloat f2 -> f1 = f2
    | ValStr s1, ValStr s2 -> s1 = s2
    | ValBool b1, ValBool b2 -> b1 = b2
    (*| ValArr a1, ValArr a2 -> *)
    | ValType t1, ValType t2 -> eq_q_type t1 t2
    | _ -> false
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

let get_basename n =
    List.hd n
;;

let get_parent_name n =
    List.tl n
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

let mem env sym :bool =
    Hashtbl.mem env.dict sym
;;

let get env sym :q_obj =
    Hashtbl.find env.dict sym
;;

let rec get_deep env sym :q_obj =
    if mem env sym then
        get env sym
    else
        match env.outer with
        | None -> raise Not_found
        | Some e -> get_deep e sym
;;

let iter f env :unit =
    Hashtbl.fold (fun k v acc -> f k v) env.dict ()
;;

let rec iter_deep f env :unit =
    let _ = iter f env in
    match env.outer with
    | None -> ()
    | Some e -> iter_deep f e
;;

let get_ns env :fullname =
    env.ns
;;

let make_impl_tbl impl =
    {tbl=(Hashtbl.create 100); impl=impl}
;;

let make_func name =
    {name=name; impls=make_impl_tbl None}
;;

let str_of_type_list types :string =
    let names = String.concat "," (List.map
        (fun tp ->
            let name = (obj_to_type tp).name in
            str_of_fullname name)
        types)
    in
    "(" ^ names ^ ")"
;;

let make_func_impl name params body env =
    let type_names = str_of_type_list (List.map
        (fun (tp, s) -> tp)
        params)
    in
    let name_with_types = make_fullname
        (get_basename name ^ type_names)
        (get_parent_name name)
    in
    {name=name_with_types; params=params; env=env; body=body}
;;

let rec get_impl_tbl impls types :func_impl option =
    match types with
    | tp::ts -> get_impl_with_supers impls.tbl tp ts
    | [] -> (match impls.impl with
        | Some imp -> Some imp
        | None -> None)
    | _ -> None
and get_impl_with_supers tbl tp_o ts :func_impl option =
    let tp = obj_to_type tp_o in
    let imp = if Hashtbl.mem tbl tp.name then
            get_impl_tbl (Hashtbl.find tbl tp.name) ts
        else
            None
    in
    match imp with
    | None ->
        (match tp.super with
        | None -> None
        | Some sup -> get_impl_with_supers tbl sup ts)
    | _ -> imp
;;

let get_func_impl func param_types =
    (*match func.v with*)
    (*| ValClosure f -> *)
    (*| _ -> None*)
    get_impl_tbl func.impls param_types
;;

let get_func_impl_for_params func params =
    let types = List.map (fun p -> p.t) params in
    get_func_impl func types
;;

let add_impl_to_func func impl :unit =
    let rec add_impl_to_func_helper impls names :unit =
        match names with
        | tp::ts -> let tbl =
            if Hashtbl.mem impls.tbl tp then
                Hashtbl.find impls.tbl tp
            else begin
                let tbl = make_impl_tbl None in
                Hashtbl.add impls.tbl tp tbl;
                tbl
            end
            in
            add_impl_to_func_helper tbl ts
        | [] when impls.impl = None -> impls.impl <- Some impl
        | _ -> raise (EnvErr "Exist")
    in
    let param_type_names = List.map
        (fun (x, s) -> (obj_to_type x).name)
        impl.params
    in
    try
        add_impl_to_func_helper func.impls param_type_names
    with EnvErr "Exist" ->
        raise (EnvErr ("function already has an implementation at ("
            ^ (String.concat ", " (List.map str_of_fullname param_type_names))
            ^ ")"))
;;

let str_of_func_impl (impl :func_impl) :string =
    str_of_fullname impl.name ^ "\n    " ^
    String.concat "\n    " (List.map
        (fun (tp, s) -> let name = (obj_to_type tp).name in
            s ^ ": " ^ (str_of_fullname name))
        impl.params)
;;

let str_of_func (func :closure) :string =
    let rec helper impls prefix :string list =
        (*Printf.printf "%s\n" (str_of_fullname prefix);*)
        let curr = match impls.impl with
            | Some impl -> [str_of_func_impl impl]
            | None -> []
        in
        Hashtbl.fold (fun k v acc -> acc @ (helper v k)) impls.tbl curr
    in
    str_of_fullname func.name ^ "\n  " ^
    String.concat "\n  " (helper func.impls [])
;;

