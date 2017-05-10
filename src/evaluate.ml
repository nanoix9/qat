open Ast
open Env
open Builtin
module BN = Builtin

exception EvalErr of string;;

type evalret =
    | EvalVal of q_obj
    | EvalNone
;;

let get_obj_from_evalret r :q_obj =
    match r with
    | EvalVal o -> o
    | _ -> raise (EvalErr "the return of evaluation is not a value")
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
    (*let _ = Printf.printf "========\n"; Env.iter (fun k v -> Printf.printf "%s\n" k) env in*)
    let o = match atom with
        | Sym s ->
            (try Env.get_deep env s
            with Not_found ->
                raise (EvalErr (Printf.sprintf "undefined symbol: %s" s)))
        | Obj o -> o
    in
    EvalVal o
and eval_list (env :env) (el :estmt list) :evalret =
    match el with
    | [] -> EvalNone
    | [a] -> eval_rec env a
    | (Atom (Sym opr) as aopr)::opd -> (match opr with
        | "do" -> eval_do env opd
        | "if" -> eval_if env opd
        | "def" -> eval_def env opd
        | "type" -> eval_type env opd
        | "func" -> eval_func env opd
        | "return" -> eval_return env opd
        | "scope" -> eval_scope env opd
        | _ -> eval_apply env aopr opd
    )
    | aopr::opd -> eval_apply env aopr opd
    (*| opr::opd -> eval_list env ((eval_rec env opr)::opd)*)
and eval_do env opd =
    match opd with
    | [] -> raise (EvalErr "DO: empty statement list")
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
                | _ -> raise (EvalErr "IF: condition should be bool"))
        | _ -> raise (EvalErr "IF: condition does not return a value")
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
    | _ -> raise (EvalErr "IF: must have 2 or 3 sub statements")
and eval_def env opd =
    let _ = match opd with
        | (Atom (Sym sym))::body::[] ->
            (match (eval_rec env body) with
            | EvalVal o ->
                if not (Env.mem env sym) then
                    Env.set env sym o
                else
                    raise (EvalErr (Printf.sprintf
                        "DEF: symbol \"%s\" is already defined" sym))
            | _ -> raise (EvalErr "DEF: assignee is not a value"))
        | _ -> raise (EvalErr "DEF: incorrect syntax")
    in
    EvalNone
and eval_type env opd =
    let name, sup = match opd with
        | [Atom (Sym name)] -> name, obj_o
        | Atom (Sym name)::Atom (Sym sup_name)::[] ->
                name, (Env.get_deep env sup_name)
        | _ -> raise (EvalErr "TYPE: incorrect syntax")
    in
    let fname = make_fullname name (get_ns env) in
    let t = make_type fname sup in
    (*Env.set env name t;*)
    EvalVal t
and eval_func env opd =
    let ns = (get_ns env) in
    match opd with
    | (Atom (Sym name))::(NodeList param_stmts)::body::[] ->
        let f =
            try Env.get_deep env name
            with Not_found ->
                let f = make_func_o (make_fullname name ns) in
                Env.set env name f;
                f
        in
        let params = List.map
            (function
                | NodeList ((Atom (Sym p))::(Atom (Sym tp))::[]) ->
                    (Env.get_deep env tp, p)
                | _ -> raise (EvalErr "FUNC: incorrect parameter definition"))
            param_stmts
        in
        let _add = make_func_impl_estmt_adder ns env in
        _add f name params body;
        EvalNone
    | _ -> raise (EvalErr "FUNC: incorrect syntax")
and eval_return env opd =
    match opd with
    | [stmt] -> eval_rec env stmt
    | _ -> raise (EvalErr "RETURN: the operand should be a single statement")
and eval_scope env opd =
    let name, stmt = match opd with
        | Atom (Sym name)::[stmt] -> name, stmt
        | [stmt] -> "", stmt
        | _ -> raise (EvalErr "SCOPE: incorrect syntax")
    in
    eval_rec (make_sub_env name env) stmt
and eval_apply env opr opd =
    let func = eval_to_obj env opr in
    let args = List.map (eval_to_obj env) opd in
    let types = List.map (fun a -> a.t) args in
    match get_func_impl_o func types with
    | None -> raise (EvalErr (Printf.sprintf
        "APPLY: implementation for %s not found: %s"
        (str_of_value func.v) (str_of_type_list types)))
    | Some impl -> (match impl.body with
        | FuncBodyEstmt stmt -> let sub_env =
            make_sub_env (get_basename impl.name) impl.env
            in
            let f (t, s) v =
                Env.set sub_env s v
            in
            List.iter2 f impl.params args;
            eval_rec sub_env stmt
        | FuncBodyInst inst -> EvalVal (inst args))
and eval_to_obj env stmt =
    (*let _ = Printf.printf "====\n"; Env.iter (fun k v -> Printf.printf "%s\n" k) env in*)
    get_obj_from_evalret (eval_rec env stmt)
;;

type evaluator = {global_env :env};;

let make_evaluator () =
    let ev = {global_env=make_env "__main__" None} in
    import_all ev.global_env module_builtin;
    import_all ev.global_env Io.module_io;
    ev
;;

let eval_estmt evaluator e =
    eval_rec evaluator.global_env e;;

let evaluate evaluator ast =
    eval_rec evaluator.global_env (pre_eval ast)
;;


