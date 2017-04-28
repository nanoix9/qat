open Ast
open Env
open Builtin

exception EvalErr

type eatom =
    | Sym of sym
    | Obj of obj
;;

type eexpr = eatom abs_tree;;

let imm_to_obj (i :imm) :obj =
    match i with
    | Int n -> make_int n
    | Float f -> make_float f
    | Str_ s -> make_str s
    | Bool b -> make_bool b
;;

let pre_eval_token (token :token) :eatom  =
    match token with
    | Imm i -> Obj (imm_to_obj i)
    | Id s -> Sym s
    | Op s -> Sym s
    | _ -> assert false
;;

let rec pre_eval (exp :ast) :eexpr =
    match exp with
    | Atom t -> Atom (pre_eval_token t)
    | NodeList el -> NodeList (List.map pre_eval el)
;;

let rec eval_rec (env :env) (ee :eexpr) :obj =
    match ee with
    | Atom t -> eval_atom env t
    | NodeList el -> eval_list env el
and eval_atom (env :env) (atom :eatom) :obj =
    match atom with
    | Sym s -> Env.get env s
    | Obj o -> o
and eval_list (env :env) (el :eexpr list) :obj =
    match el with
    | [] -> nil
    | [a] -> eval_rec env a
    | (Atom (Sym opr))::opd -> (match opr with
        | "do" -> eval_do env opd
        | "if" -> eval_if env opd
        | "def" -> eval_def env opd
        | "type" -> eval_type env opd
        | "func" -> eval_func env opd
        | _ -> eval_apply opr opd
    )
    | _ -> raise EvalErr
    (*| opr::opd -> eval_list env ((eval_rec env opr)::opd)*)
and eval_do env opd =
    match opd with
    | [] -> raise EvalErr
    | [last] -> eval_rec env last
    | first::rest ->
            let _ = eval_rec env first in
            eval_do env rest
and eval_if env opd =
    nil
and eval_def env opd =
    let _ = match opd with
        | (Atom (Sym sym))::body::[] ->
                Env.set env sym (eval_rec env body)
        | _ -> raise EvalErr
    in nil
and eval_type env opd =
    let name, sup = match opd with
        | [Atom (Sym name)] -> name, qobj
        | Atom (Sym name)::Atom (Sym "isa")::Atom (Sym sup_name)::[] ->
                name, (Env.get env sup_name)
        | _ -> raise EvalErr
    in
    let fname = make_fullname name (get_ns env) in
    let t = make_type fname sup in
    Env.set env name t;
    t
and eval_func env opd =
    nil
and eval_apply opr opd =
    nil
;;

let global_env = make_env "__main__" None;;

let evaluate exp =
    eval_rec global_env exp
;;

