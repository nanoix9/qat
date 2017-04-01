open Env

let builtin = "builtin";;

let rec obj_o = {t=type_o; v=ValType {name=[builtin; "object"]; super=None}}
and type_o = {t=type_o; v=ValType {name=[builtin; "type"]; super=Some obj_o}}
;;

let make_obj (t :obj) (v :value) :obj =
    assert (t.t == type_o);
    {t=t; v=v}
;;

let none = make_obj type_o ValNone
;;

let make_type name sup :obj =
    assert (sup.t == type_o);
    make_obj type_o (ValType {name=name; super=Some sup})
;;

let make_builtin_type (sname :string) (sup :obj) :obj =
    make_type [builtin; sname] sup
;;

let num_t = make_builtin_type "numeric" obj_o
let int_t = make_builtin_type "int" num_t
let float_t = make_builtin_type "float" num_t
let str_t = make_builtin_type "str" obj_o
let bool_t = make_builtin_type "bool" obj_o

let make_int n :obj = make_obj int_t (ValInt n)
let make_float f :obj = make_obj float_t (ValFloat f)
let make_str s :obj = make_obj str_t (ValStr s)
let make_bool b :obj = make_obj bool_t (ValBool b)
