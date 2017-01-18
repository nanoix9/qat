open Expr
open Lex

exception ParseErr

let unparse e = str_of_expr e

let rec expr_list strm :expr =
    match look_ahead strm with
        | Op "(" -> let _ = pop_token strm in expr_group strm
        | Op ")" -> raise ParseErr
        | _ -> expr_seq strm

and expr_elem strm =
    match look_ahead strm with
        | EOS -> raise ParseErr
        | Op "(" -> let _ = pop_token strm in expr_group strm
        | Op ")" -> raise ParseErr
        | Op ";" -> ExprList []
        | _ as t -> Atom t

and expr_group strm =
    let rec expr_group_helper (visited:expr list) (strm:stream) :expr =
        match look_ahead strm with
            | EOS -> raise ParseErr
            | Op ";" -> expr_seq_group (List.rev visited) strm
            | Op ")" -> let _ = pop_token strm in ExprList (List.rev visited)
            | _ -> expr_group_helper (Atom (pop_token strm)::visited) strm
    in expr_group_helper [] strm

and expr_seq_group first strm =
    let rec helper strm =
        match look_ahead strm with
            | Op ")" -> let _ = pop_token strm in []
            | _ -> (expr_seq strm)::helper strm
    in ExprList ((ExprList first)::(helper strm))

and expr_seq strm =
    let rec expr_seq_helper strm =
        match look_ahead strm with
            | Op ";" -> let _ = pop_token strm in []
            | _ -> (expr_elem strm)::expr_seq_helper strm
    in ExprList (expr_seq_helper strm)

let parse_stream strm = None

let parse str =
    let strm = new_stream str in (expr_list strm)

