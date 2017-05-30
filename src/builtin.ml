open Env
open Big_int

let env_builtin = make_env "builtin" None;;

let builtin = get_ns env_builtin;;

let rec obj_o :q_obj = {t=type_o; v=ValType
    {name=make_fullname "object" builtin; super=None}}
and type_o :q_obj = {t=type_o; v=ValType
    {name=make_fullname "type" builtin; super=Some obj_o}}
;;

let make_obj (t :q_obj) (v :value) :q_obj =
    assert (t.t == type_o);
    {t=t; v=v}
;;

let rec str_of_value (v :value) :string =
    match v with
    | ValNil -> "nil"
    | ValInt i -> "int(" ^ string_of_big_int i ^ ")"
    | ValFloat f -> "float(" ^ string_of_float f ^ ")"
    | ValStr s -> "str(\"" ^ s ^ "\")"
    | ValBool b -> "bool(" ^ string_of_bool b ^ ")"
    | ValType t -> let sup_info = match t.super with
            | None -> ""
            | Some s -> "<:" ^ str_of_value s.v
            in
            str_of_fullname t.name ^ sup_info
    | ValClosure c -> "function(" ^ str_of_fullname c.name ^ ")"
    | _ -> "NOT SUPPORTED"
;;

let str_of_obj (j :q_obj) :string =
    assert (j.t.t == type_o);
    "OBJ(type=" ^ str_of_fullname (obj_to_type j.t).name
        ^ ", value="
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
let stmt_t = make_builtin_type "quoted" obj_o
let module_t = make_builtin_type "module" obj_o
let var_t = make_builtin_type "variable" obj_o

let make_int n :q_obj = make_obj int_t (ValInt n)
let make_float f :q_obj = make_obj float_t (ValFloat f)
let make_str s :q_obj = make_obj str_t (ValStr s)
let make_bool b :q_obj = make_obj bool_t (ValBool b)

let make_var vt init_val :q_obj =
    make_obj var_t (ValVar {vt=vt; vv=init_val})
;;

let make_stmt_o (s :estmt) :q_obj =
    make_obj stmt_t (ValStmt s)
;;

let make_func_o name =
    make_obj func_t (ValClosure (make_func name))
;;

let is_func_o obj :bool =
    obj.t == func_t
;;

let add_impl_to_func_o func_o impl :unit =
    add_impl_to_func (obj_to_closure func_o) impl
    (*| _ -> None*)
;;

let get_func_impl_o func_o param_types =
    get_func_impl (obj_to_closure func_o) param_types
;;

let make_builtin_func name =
    make_func_o (make_fullname name builtin)
;;

(*let _wrap_inst (f :q_obj list -> q_obj) :q_obj =*)
    (*let *)

(*--------------- builtin functions -------------------*)
let make_func_impl_inst_adder module_name env =
    let f func basename params inst :unit =
        add_impl_to_func_o func (make_func_impl
            (make_fullname basename module_name)
            params
            (FuncBodyInst inst)
            env)
    in
    f
;;

let make_func_impl_estmt_adder module_name env =
    let f func basename params estmt :unit =
        add_impl_to_func_o func (make_func_impl
            (make_fullname basename module_name)
            params
            (FuncBodyEstmt estmt)
            env)
    in
    f
;;

let make_params types :(q_obj * sym) list =
    List.map (fun tp -> (tp, "_")) types
;;

let _add_builtin_func_impl = make_func_impl_inst_adder builtin env_builtin;;

let _make_unop_params tp = [(tp, "_")];;
let _int1 = _make_unop_params int_t;;
let _float1 = _make_unop_params float_t;;
let _str1 = _make_unop_params str_t;;
let _bool1 = _make_unop_params bool_t;;

let _make_binop_params tp = [(tp, "_"); (tp, "_")];;
let _int2 = _make_binop_params int_t;;
let _float2 = _make_binop_params float_t;;
let _str2 = _make_binop_params str_t;;
let _bool2 = _make_binop_params bool_t;;

let _make_un_cmd extract =
    let cmd f = function
        | a::[] -> f (extract a); nil
    in
    cmd
;;

let _make_unop pack extract =
    let ret f = function
        | a::[] -> pack (f (extract a))
    in
    ret
;;

let _make_binop pack extract =
    let ret f = function
        | a::b::[] -> pack (f (extract a) (extract b))
    in
    ret
;;

let _unop_ret_str extract = _make_unop make_str extract;;

let _unop_int = _make_unop make_int obj_to_int;;
let _unop_float = _make_unop make_float obj_to_float;;
let _unop_str = _make_unop make_str obj_to_str;;
let _unop_bool = _make_unop make_bool obj_to_bool;;

let _binop_int = _make_binop make_int obj_to_int;;
let _binop_float = _make_binop make_float obj_to_float;;
let _binop_str = _make_binop make_str obj_to_str;;
let _binop_bool = _make_binop make_bool obj_to_bool;;

let _op_helper op =
    let f = make_builtin_func op in
    let add = _add_builtin_func_impl f op in
    f, add
;;

(* `call` function should support variable parameters *)
let call =
    let f, add = _op_helper "call" in
    (*add (_make_binop_params [type_o, obj_o])*)
    f
;;

let show =
    let f, add = _op_helper "show" in
    add _int1 (_unop_ret_str obj_to_int (fun a -> string_of_big_int a));
    add _float1 (_unop_ret_str obj_to_float (fun a -> string_of_float a));
    add _str1 (_unop_ret_str obj_to_str (fun a -> a));
    add _bool1 (_unop_ret_str obj_to_bool (fun a -> string_of_bool a));
    f
;;

let op_add =
    let f, add = _op_helper "+" in
    add _int2 (_binop_int (fun a b -> add_big_int a b));
    add _float2 (_binop_float (fun a b -> a +. b));
    add _str2 (_binop_str (fun a b -> a ^ b));
    f
;;
let op_sub =
    let f, add = _op_helper "-" in
    add _int1 (_unop_int (fun a -> minus_big_int a));
    add _float1 (_unop_float (fun a -> ~-. a));
    add _int2 (_binop_int (fun a b -> sub_big_int a b));
    add _float2 (_binop_float (fun a b -> a -. b));
    f
;;
let op_mul =
    let f, add = _op_helper "*" in
    add _int2 (_binop_int (fun a b -> mult_big_int a b));
    add _float2 (_binop_float (fun a b -> a *. b));
    f
;;
let op_div =
    let f, add = _op_helper "/" in
    add _int2 (_binop_int (fun a b -> div_big_int a b));
    add _float2 (_binop_float (fun a b -> a /. b));
    f
;;
let op_mod =
    let f, add = _op_helper "%" in
    add _int2 (_binop_int (fun a b -> mod_big_int a b));
    f
;;

let _cmp_int = _make_binop make_bool obj_to_int;;
let _cmp_float = _make_binop make_bool obj_to_float;;
let _cmp_str = _make_binop make_bool obj_to_str;;

let op_eq =
    let f, add = _op_helper "==" in
    add _int2 (_cmp_int (fun a b -> eq_big_int a b));
    add _float2 (_cmp_float (fun a b -> a = b));
    add _bool2 (_binop_bool (fun a b -> a = b));
    add _str2 (_cmp_str (fun a b -> a = b));
    f
;;
let op_neq =
    let f, add = _op_helper "!=" in
    add _int2 (_cmp_int (fun a b -> not (eq_big_int a b)));
    add _float2 (_cmp_float (fun a b -> a <> b));
    add _bool2 (_binop_bool (fun a b -> a <> b));
    add _str2 (_cmp_str (fun a b -> a <> b));
    f
;;
let op_gt =
    let f, add = _op_helper ">" in
    add _int2 (_cmp_int (fun a b -> gt_big_int a b));
    add _float2 (_cmp_float (fun a b -> a > b));
    add _str2 (_cmp_str (fun a b -> a > b));
    f
;;
let op_lt =
    let f, add = _op_helper "<" in
    add _int2 (_cmp_int (fun a b -> (*Printf.printf "%d,%d\n" a b;*) lt_big_int a b));
    add _float2 (_cmp_float (fun a b -> a < b));
    add _str2 (_cmp_str (fun a b -> a < b));
    f
;;

let op_and =
    let f, add = _op_helper "&&" in
    add _bool2 (_binop_bool (fun a b -> a && b));
    f
;;
let op_or =
    let f, add = _op_helper "||" in
    add _bool2 (_binop_bool (fun a b -> a || b));
    f
;;
let op_not =
    let f, add = _op_helper "!" in
    add _bool1 (_unop_bool (fun a -> not a));
    f
;;

let op_land =
    let f, add = _op_helper "&" in
    add _int2 (_binop_int (fun a b -> and_big_int a b));
    f
;;
let op_lor =
    let f, add = _op_helper "|" in
    add _int2 (_binop_int (fun a b -> or_big_int a b));
    f
;;
let op_lnot =
    let f, add = _op_helper "!" in
    (*add _int1 (_unop_int (fun a -> lnot a));*)
    f
;;
let op_lsl =
    let f, add = _op_helper "<<" in
    add _int2 (_binop_int (fun a b -> shift_left_big_int a (int_of_big_int b)));
    f
;;
let op_asr =
    let f, add = _op_helper ">>" in
    add _int2 (_binop_int (fun a b -> shift_right_big_int a (int_of_big_int b)));
    f
;;
let op_lsr =
    let f, add = _op_helper ">>>" in
    (*add _int2 (_binop_int (fun a b -> a lsr b));*)
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
    let name = get_ns (obj_to_scope mdl) in
    name
;;

let import_all env mdl =
    let mdl_env = match mdl.v with
        | ValScope e -> e
        (*| _ -> raise (EvalErr "only imports a module")*)
    in
    let f k v = (*Printf.printf "set %s\n" k;*) Env.set env k v in
    Env.iter f mdl_env
;;

let module_builtin =
    let b = make_module_on_env env_builtin in
    b
;;

let add_obj_to_env env obj =
    let name = match obj.v with
    | ValType t -> get_basename t.name
    | ValClosure c -> get_basename c.name
    in
    Env.set env name obj
;;

let _set_obj j = add_obj_to_env env_builtin j in
_set_obj obj_o;
_set_obj type_o;

_set_obj num_t;
_set_obj int_t;
_set_obj float_t;
_set_obj str_t;
_set_obj bool_t;
_set_obj func_t;
_set_obj module_t;

_set_obj show;

_set_obj op_add;
_set_obj op_sub;
_set_obj op_mul;
_set_obj op_div;
_set_obj op_mod;

_set_obj op_eq;
_set_obj op_neq;
_set_obj op_gt;
_set_obj op_lt;

_set_obj op_and;
_set_obj op_or;
_set_obj op_not;

_set_obj op_land;
_set_obj op_lor;
_set_obj op_lnot;
_set_obj op_lsl;
_set_obj op_asr;
_set_obj op_lsr;
;;

