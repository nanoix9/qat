open Env

let env_builtin = make_env "builtin" None;;

let builtin = get_ns env_builtin;;

let rec obj_o = {t=type_o; v=ValType
    {name=make_fullname "object" builtin; super=None}}
and type_o = {t=type_o; v=ValType
    {name=make_fullname "type" builtin; super=Some obj_o}}
;;

let make_obj (t :q_obj) (v :value) :q_obj =
    assert (t.t == type_o);
    {t=t; v=v}
;;

let rec str_of_value (v :value) :string =
    match v with
    | ValNil -> "nil"
    | ValInt i -> "int(" ^ string_of_int i ^ ")"
    | ValFloat f -> "float(" ^ string_of_float f ^ ")"
    | ValStr s -> "str(\"" ^ s ^ "\")"
    | ValBool b -> "bool(" ^ string_of_bool b ^ ")"
    | ValType t -> let sup_info = match t.super with
            | None -> ""
            | Some s -> "<:" ^ str_of_value s.v
            in
            str_of_fullname t.name ^ sup_info
    | ValClosure c -> "function(" ^ str_of_fullname c.name ^ ")"
;;

let str_of_obj (j :q_obj) :string =
    assert (j.t.t == type_o);
    match j.t.v with
    | ValType t ->
            "OBJ(type=" ^ str_of_fullname t.name ^ ", value="
            ^ str_of_value j.v ^ ")"
;;

let nil = make_obj type_o ValNil
;;

let make_type name sup :q_obj =
    assert (sup.t == type_o);
    make_obj type_o (ValType {name=name; super=Some sup})
;;

let make_builtin_type (sname :string) (sup :q_obj) :q_obj =
    make_type (make_fullname sname builtin) sup
;;

let num_t = make_builtin_type "numeric" obj_o
let int_t = make_builtin_type "int" num_t
let float_t = make_builtin_type "float" num_t
let str_t = make_builtin_type "str" obj_o
let bool_t = make_builtin_type "bool" obj_o
let func_t = make_builtin_type "function" obj_o
let module_t = make_builtin_type "module" obj_o

let make_int n :q_obj = make_obj int_t (ValInt n)
let make_float f :q_obj = make_obj float_t (ValFloat f)
let make_str s :q_obj = make_obj str_t (ValStr s)
let make_bool b :q_obj = make_obj bool_t (ValBool b)

let make_func_o name =
    make_obj func_t (ValClosure (make_func name))
;;

let add_impl_to_func_o func_o impl :unit =
    match func_o.v with
    | ValClosure f -> add_impl_to_func f impl
    (*| _ -> None*)
;;

let get_func_impl_o func_o param_types =
    match func_o.v with
    | ValClosure f -> get_func_impl f param_types
;;

let make_builtin_func name =
    make_func_o (make_fullname name builtin)
;;

(*let _wrap_inst (f :q_obj list -> q_obj) :q_obj =*)
    (*let *)
let op_add = let f = make_builtin_func "+" in
    add_impl_to_func_o f (make_func_impl ["+"]
        [(int_t, "_"); (int_t, "_")]
        (FuncBodyInst (function
            | a::b::[] -> make_int ((obj_to_int a) + (obj_to_int b))))
        env_builtin);
    f
;;

let make_module_on_env env :q_obj =
    make_obj module_t (ValScope env)
;;

let make_module name env :q_obj =
    make_module_on_env (make_env name env)
;;

(*TODO: can we change import module to a function?*)
let import_module env mdl =
    let name = match mdl.v with
    | ValScope e -> get_ns e
    in
    name

;;

let module_builtin = let b = make_module_on_env env_builtin
    in
    b
;;

let _set = Env.set env_builtin in
let _set_obj j =
    let name = match j.v with
    | ValType t -> get_basename t.name
    | ValClosure c -> get_basename c.name
    in
    _set name j
in
_set_obj obj_o;
_set_obj type_o;

_set_obj num_t;
_set_obj int_t;
_set_obj float_t;
_set_obj str_t;
_set_obj bool_t;
_set_obj func_t;
_set_obj module_t;

_set_obj op_add;
;;

