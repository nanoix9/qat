open Env

let builtin = make_fullname "builtin" name_root;;

let rec qobj = {t=qtype; v=ValType
    {name=make_fullname "object" builtin; super=None}}
and qtype = {t=qtype; v=ValType
    {name=make_fullname "type" builtin; super=Some qobj}}
;;

let make_obj (t :obj) (v :value) :obj =
    assert (t.t == qtype);
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

let str_of_obj (j :obj) :string =
    assert (j.t.t == qtype);
    match j.t.v with
    | ValType t ->
            "OBJ(type=" ^ str_of_fullname t.name ^ ", value="
            ^ str_of_value j.v ^ ")"
;;

let nil = make_obj qtype ValNil
;;

let make_type name sup :obj =
    assert (sup.t == qtype);
    make_obj qtype (ValType {name=name; super=Some sup})
;;

let make_builtin_type (sname :string) (sup :obj) :obj =
    make_type (make_fullname sname builtin) sup
;;

let num_t = make_builtin_type "numeric" qobj
let int_t = make_builtin_type "int" num_t
let float_t = make_builtin_type "float" num_t
let str_t = make_builtin_type "str" qobj
let bool_t = make_builtin_type "bool" qobj

let make_int n :obj = make_obj int_t (ValInt n)
let make_float f :obj = make_obj float_t (ValFloat f)
let make_str s :obj = make_obj str_t (ValStr s)
let make_bool b :obj = make_obj bool_t (ValBool b)

