open Expr
open Env

let rec eval_rec (env :env) (exp :expr) :obj =
    match exp with
    | Atom t -> eval_token env t
    | ExprList el -> eval_list env el
and eval_token (env :env) (token :token) :obj =
    match token with
    | Id id -> Env.get env id
and eval_list (env :env) (el :expr list) :obj =
    match el with
    | [] -> none
    | (Atom opr)::opd -> (match opr with
        | Id "def" -> eval_def env opd
    )
    (*| opr::opd -> eval_list env ((eval_rec env opr)::opd)*)
and eval_def env opd =
    none
;;

let global_env = make_env None;;

let evaluate exp =
    eval_rec global_env exp
;;
