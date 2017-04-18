open Ast
open Lex

exception ParseErr

let unparse (e:ast) :string = str_of_ast e

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

let rec ast_elem (strm:stream) :ast =
    match look_ahead strm with
        | EOS -> raise ParseErr
        | OpenGroup t -> let _ = pop_token strm in ast_group t strm
        | CloseGroup _ -> raise ParseErr
        | Terminator _ -> NodeList []
        | _ (*as t*) ->
                (*Printf.printf "elem: %s\n" (str_of_token t);*)
                Atom (pop_token strm)

and ast_group start_token strm :ast =
    let rec ast_group_helper st (visited:ast list) (strm:stream) :ast =
        match look_ahead strm with
            | EOS -> raise ParseErr
            | OpenGroup _ -> let e = ast_elem strm in
                        ast_group_helper st (e::visited) strm
            | CloseGroup t -> let _ = pop_token strm in
                              NodeList (add_group_prefix (List.rev visited) st t)
            | Terminator _ -> let _ = pop_token strm in
                              ast_seq_group st (List.rev visited) strm
            | _ (*as t*) -> (let a = Atom (pop_token strm) in
                        (*Printf.printf "group%s: %s\n" st (str_of_token t);*)
                        ast_group_helper st (a::visited) strm)
    in ast_group_helper start_token [] strm

and ast_seq_group start_token (first:ast list) (strm:stream) :ast =
    let rec helper st strm =
        match look_ahead strm with
            | CloseGroup t -> let _ = pop_token strm in match_group st t, []
            | _ -> let e = ast_seq strm in
                   let prefix, rest = helper st strm
                   in prefix, e::rest in
    let prefix, rest = helper start_token strm in
    let tail = Atom (Id "do")::(match first with
        | [] -> rest
        | _ -> (NodeList first)::rest) in
    let lst = match prefix with
        | None -> tail
        | Some t -> Atom (Op t)::tail
    in NodeList lst

and ast_seq (strm:stream) :ast =
    let rec ast_seq_helper strm =
        match look_ahead strm with
            | Terminator _ ->
                    (*Printf.printf "seq ends with ;\n";*)
                    let _ = pop_token strm in []
            | _ -> let e = ast_elem strm in (
                    (*Printf.printf "seq: %s \n" (str_of_ast e); *)
                    e::ast_seq_helper strm)
    in NodeList (ast_seq_helper strm)

let rec ast_list (strm:stream) :ast =
    match look_ahead strm with
        | OpenGroup t -> let _ = pop_token strm in ast_group t strm
        | CloseGroup _ -> raise ParseErr
        | _ -> ast_seq strm

let parse_stream strm = None

let parse (str:string) :ast =
    let strm = new_stream str in (
        (*Printf.printf "%s\n" str; *)
        ast_elem strm)

