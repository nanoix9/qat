open Env

let builtin = make_fullname "builtin" name_root;;

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

let make_int n :q_obj = make_obj int_t (ValInt n)
let make_float f :q_obj = make_obj float_t (ValFloat f)
let make_str s :q_obj = make_obj str_t (ValStr s)
let make_bool b :q_obj = make_obj bool_t (ValBool b)

