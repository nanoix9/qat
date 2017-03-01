open Expr;;

type associativity =
    | Left
    | Right
    | Non
;;

type fixity =
    | Prefix
    | Infix of associativity
    | Postfix
    | Closed
;;

type macro_elem =
    | Literal of token
    | Variable of string
;;

type macro_expr = macro_elem abs_expr;;

type 'm macro = {
    fix: fixity;
    pattern: 'm abs_expr;
    body: 'm abs_expr }
;;

