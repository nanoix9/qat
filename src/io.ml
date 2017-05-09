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
    let _adder = (make_func_impl_inst_adder name_io env_io) in
    let _estmt_adder = (make_func_impl_estmt_adder name_io env_io) in
    let add = _adder f name in
    let eadd = _estmt_adder f name in
    f, add, eadd
;;

let ee lst :estmt = NodeList lst;;
let sym s :estmt = Atom (Sym s);;
(*let i n :estmt = Atom (Obj (make_int n));;*)
(*let f n :estmt = Atom (Obj (make_float n));;*)
(*let s t :estmt = Atom (Obj (make_str t));;*)
(*let b x :estmt = Atom (Obj (make_bool x));;*)

let print =
    let f, add, eadd = _op_helper "print" in
    add (make_params [str_t]) (_make_un_cmd obj_to_str (fun a -> printf "%s" a));
    (*eadd [obj_o, "x"] (ee [sym "print"; ee [sym "show"; sym "x"]]);*)
    f
;;

let println =
    let f, add, _ = _op_helper "println" in
    add (make_params [str_t]) (_make_un_cmd obj_to_str (fun a -> printf "%s\n" a));
    f
;;

let _set_obj j = add_obj_to_env env_io j in
_set_obj print;
_set_obj println;
(*import_all env_io module_builtin;*)
;;

