open Expr
open Lex

exception ParseErr

let unparse (e:expr) :string = str_of_expr e

let match_group opn cls =
    if opn = "(" && cls = ")" then
        None
    else if opn = "[" && cls = "]" then
        Some "[]"
    else if opn = "{" && cls = "}" then
        Some "{}"
    else raise ParseErr
;;

let add_group_prefix lst opn cls =
    match match_group opn cls with
      | None -> lst
      | Some s -> (Atom (Op s))::lst
;;

let rec expr_elem (strm:stream) :expr =
    match look_ahead strm with
        | EOS -> raise ParseErr
        | OpenGroup t -> let _ = pop_token strm in expr_group t strm
        | CloseGroup _ -> raise ParseErr
        | Terminator _ -> ExprList []
        | _ as t -> Printf.printf "elem: %s\n" (str_of_token t); Atom (pop_token strm)

and expr_group start_token strm :expr =
    let rec expr_group_helper st (visited:expr list) (strm:stream) :expr =
        match look_ahead strm with
            | EOS -> raise ParseErr
            | OpenGroup _ -> let e = expr_elem strm in
                        expr_group_helper st (e::visited) strm
            | CloseGroup t -> let _ = pop_token strm in
                              ExprList (add_group_prefix (List.rev visited) st t)
            | Terminator _ -> let _ = pop_token strm in
                              expr_seq_group st (List.rev visited) strm
            | _ as t -> let a = Atom (pop_token strm) in
                        (Printf.printf "group%s: %s\n" st (str_of_token t);
                        expr_group_helper st (a::visited) strm)
    in expr_group_helper start_token [] strm

and expr_seq_group start_token (first:expr list) (strm:stream) :expr =
    let rec helper st strm =
        match look_ahead strm with
            | CloseGroup t -> let _ = pop_token strm in match_group st t, []
            | _ -> let e = expr_seq strm in
                   let prefix, rest = helper st strm
                   in prefix, e::rest in
    let prefix, rest = helper start_token strm in
    let tail = match first with
        | [] -> rest
        | _ -> (ExprList first)::rest in
    let lst = match prefix with
        | None -> tail
        | Some t -> Atom (Op t)::tail
    in ExprList lst

and expr_seq (strm:stream) :expr =
    let rec expr_seq_helper strm =
        match look_ahead strm with
            | Terminator _ -> Printf.printf "seq ends with ;\n"; let _ = pop_token strm in []
            | _ -> let e = expr_elem strm in (Printf.printf "seq: %s \n" (str_of_expr e); e::expr_seq_helper strm)
    in ExprList (expr_seq_helper strm)

let rec expr_list (strm:stream) :expr =
    match look_ahead strm with
        | OpenGroup t -> let _ = pop_token strm in expr_group t strm
        | CloseGroup _ -> raise ParseErr
        | _ -> expr_seq strm

let parse_stream strm = None

let parse (str:string) :expr =
    let strm = new_stream str in (Printf.printf "%s\n" str; expr_elem strm)

