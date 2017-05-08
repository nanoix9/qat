open Printf
open Env
open Builtin

let env_io = make_env "io" None;;

let name_io = get_ns env_io;;

let module_io =
    let b = make_module_on_env env_io in
    b
;;

let _op_helper name =
    let f = make_func_o (make_fullname name name_io) in
    let _adder = (make_func_impl_adder name_io env_io) in
    let add = _adder f name in
    f, add
;;

let print =
    let f, add = _op_helper "print" in
    add (make_params [str_t]) (_make_un_cmd obj_to_str (fun a -> printf "%s" a; nil));
    f
;;

let println =
    let f, add = _op_helper "println" in
    add (make_params [str_t]) (_make_un_cmd obj_to_str (fun a -> printf "%s\n" a; nil));
    f
;;

let _set_obj j = add_obj_to_env env_io j in
_set_obj print;
_set_obj println;
;;

