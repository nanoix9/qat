open Expr
open Eexpr

let trans_atom (token :Expr.token) :eatom  =
    match token with
    | Imm i -> EImm i
    | Id s -> Sym s
    | Op s -> Sym s
    | _ -> assert false
;;

let rec translate (expr :Expr.expr) :eexpr =
    match expr with
    | Atom a -> Atom (trans_atom a)
    | ExprList el -> ExprList (List.map translate el)
;;

