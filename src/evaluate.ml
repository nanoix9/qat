open Ast
open Env
open Builtin

exception EvalErr

type eatom =
    | Sym of sym
    | Obj of q_obj
;;

type estmt = eatom abs_tree;;

type evalret =
    | EvalVal of q_obj
    | EvalNone
;;

let eq_evalret r1 r2 =
    match r1, r2 with
    | EvalVal o1, EvalVal o2 -> eq_q_obj o1 o2
    | _ -> r1 = r2
;;

let str_of_evalret = function
    | EvalVal o -> str_of_obj o
    | EvalNone -> "EVAL_NONE"
;;

let imm_to_obj (i :imm) :q_obj =
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

let rec pre_eval (exp :ast) :estmt =
    match exp with
    | Atom t -> Atom (pre_eval_token t)
    | NodeList el -> NodeList (List.map pre_eval el)
;;

let rec eval_rec (env :env) (ee :estmt) :evalret =
    match ee with
    | Atom t -> eval_atom env t
    | NodeList el -> eval_list env el
and eval_atom (env :env) (atom :eatom) :evalret =
    let o = match atom with
        | Sym s -> Env.get env s
        | Obj o -> o
    in
    EvalVal o
and eval_list (env :env) (el :estmt list) :evalret =
    match el with
    | [] -> EvalNone
    | [a] -> eval_rec env a
    | (Atom (Sym opr))::opd -> (match opr with
        | "do" -> eval_do env opd
        | "if" -> eval_if env opd
        | "def" -> eval_def env opd
        | "type" -> eval_type env opd
        | "func" -> eval_func env opd
        | "return" -> eval_return env opd
        | "scope" -> eval_scope env opd
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
    let eval_to_bool (cond :estmt) :bool =
        match eval_rec env cond with
        | EvalVal v when v.t == bool_t ->
                (match v.v with
                | ValBool b -> b
                | _ -> raise EvalErr)
        | _ -> raise EvalErr
    in
    match opd with
    | [cond; stmt_true;] ->
            if eval_to_bool cond then
                eval_rec env stmt_true
            else
                EvalNone
    | [cond; stmt_true; stmt_false;] ->
            if eval_to_bool cond then
                eval_rec env stmt_true
            else
                eval_rec env stmt_false
    | _ -> raise EvalErr
and eval_def env opd =
    let _ = match opd with
        | (Atom (Sym sym))::body::[] ->
                (match (eval_rec env body) with
                    | EvalVal o ->
                            if not (Env.mem env sym) then
                                Env.set env sym o
                            else
                                raise EvalErr
                    | _ -> raise EvalErr)
        | _ -> raise EvalErr
    in
    EvalNone
and eval_type env opd =
    let name, sup = match opd with
        | [Atom (Sym name)] -> name, obj_o
        | Atom (Sym name)::Atom (Sym sup_name)::[] ->
                name, (Env.get env sup_name)
        | _ -> raise EvalErr
    in
    let fname = make_fullname name (get_ns env) in
    let t = make_type fname sup in
    (*Env.set env name t;*)
    EvalVal t
and eval_func env opd =
    EvalNone
and eval_return env opd =
    match opd with
    | [stmt] -> eval_rec env stmt
    | _ -> raise EvalErr
and eval_scope env opd =
    let name, stmt = match opd with
        | Atom (Sym name)::[stmt] -> name, stmt
        | [stmt] -> "", stmt
    in
    eval_rec (make_sub_env name env) stmt
and eval_apply opr opd =
    EvalNone
;;

type evaluator = {global_env :env};;

let make_evaluator () =
    {global_env=make_env "__main__" None}
;;

let eval_estmt evaluator e =
    eval_rec evaluator.global_env e;;

let evaluate evaluator ast =
    eval_rec evaluator.global_env (pre_eval ast)
;;


