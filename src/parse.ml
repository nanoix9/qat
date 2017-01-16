open Ast
open Lex

let unparse ast = str_of_ast ast

let rec next_expr strm =
    match next_token strm with
        | None -> []
        | Some t -> Token(Id(t))::next_expr strm

let parse_stream strm =
    match next_token strm with
        | None -> None
        | Some t -> Some(Expr(Token(Id(t)) :: next_expr strm))

let parse str =
    let strm = new_stream str in
    parse_stream strm

