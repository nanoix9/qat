open Expr
open Lex

exception ParseErr

let unparse e = str_of_expr e

let rec expr_elem strm =
    match look_ahead strm with
        | EOS -> raise ParseErr
        | LPAREN -> let _ = pop_token strm in expr_group strm
        | RPAREN -> raise ParseErr
        | TERMIN -> ExprList []
        | _ as t -> Printf.printf "elem: %s\n" (str_of_token t); Atom (pop_token strm)

and expr_group strm =
    let rec expr_group_helper (visited:expr list) (strm:stream) :expr =
        match look_ahead strm with
            | EOS -> raise ParseErr
            | LPAREN -> let _ = pop_token strm in
                        let e = expr_group strm in
                        expr_group_helper (e::visited) strm
            | RPAREN -> let _ = pop_token strm in ExprList (List.rev visited)
            | TERMIN -> let _ = pop_token strm in expr_seq_group (List.rev visited) strm
            | _ as t -> let a = Atom (pop_token strm) in (Printf.printf "group: %s\n" (str_of_token t);
                        expr_group_helper (a::visited) strm)
    in expr_group_helper [] strm

and expr_seq_group first strm =
    let rec helper strm =
        match look_ahead strm with
            | RPAREN -> let _ = pop_token strm in []
            | _ -> let e = expr_seq strm in e::helper strm
    in ExprList ((ExprList first)::(helper strm))

and expr_seq strm =
    let rec expr_seq_helper strm =
        match look_ahead strm with
            | TERMIN -> Printf.printf "seq ends with ;\n"; let _ = pop_token strm in []
            | _ -> let e = expr_elem strm in (Printf.printf "seq: %s \n" (str_of_expr e); e::expr_seq_helper strm)
    in ExprList (expr_seq_helper strm)

let rec expr_list strm :expr =
    match look_ahead strm with
        | LPAREN -> let _ = pop_token strm in expr_group strm
        | RPAREN -> raise ParseErr
        | _ -> expr_seq strm

let parse_stream strm = None

let parse str =
    let strm = new_stream str in (Printf.printf "%s\n" str; expr_seq strm)

