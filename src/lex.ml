open Expr

exception LexErr

type stream = { mutable curr: token option; buf: string; mutable pos: int; mutable line_num: int }

let re_int = Str.regexp "-?[0-9]+"
let re_hex = Str.regexp "0x[0-9a-fA-F]+"
let re_float = Str.regexp "-?[0-9]+\\.[0-9]*\\([eE][+-]?[0-9]+\\)?"
let re_bool = Str.regexp "true\\|false"
let re_str_double_quotes = Str.regexp "\"\\(\\(\\\\.\|[^\\\\\"]\\)*\\)\""
let re_str_single_quotes = Str.regexp "'\\(\\(\\\\.\|[^\\\\']\\)*\\)'"
let re_id = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*"
let re_op = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*"
let re_whitespace = Str.regexp "[ \t\n]+"

let new_stream str = { curr=None; buf=str; pos=0; line_num=1; }

let read_next stream :token =
    let pos = stream.pos in
    let buf = stream.buf in
    if pos >= String.length buf then
        EOS
    else let rec read_next_helper buf pos =
        let c = String.get buf pos in
        if c = '(' then (pos+1, LPAREN)
        else if c = ')' then (pos+1, RPAREN)
        else if c = ';' then (pos+1, TERMIN)
        else if (Str.string_match re_float buf pos) then
            let token = Str.matched_string buf in
            (Str.match_end (), Imm (Float (float_of_string token)))
        else if (Str.string_match re_hex buf pos)
                || (Str.string_match re_int buf pos) then
            let token = Str.matched_string buf in
            (Str.match_end (), Imm (Int (int_of_string token)))
        else if (Str.string_match re_bool buf pos) then
            let token = Str.matched_string buf in
            (Str.match_end (), Imm (Bool (bool_of_string token)))
        else if (Str.string_match re_str_double_quotes buf pos) then
            let token = Str.replace_matched "\\1" buf in
            (Str.match_end (), Imm (Str_ token))
        else if (Str.string_match re_str_single_quotes buf pos) then
            let token = Str.replace_matched "\\1" buf in
            (Str.match_end (), Imm (Str_ token))
        else if (Str.string_match re_id buf pos) then
            let token = Str.matched_string buf in
            (Str.match_end (), Id token)
        else if (Str.string_match re_whitespace buf pos) then
            let p, t = read_next_helper buf (Str.match_end ()) in p, t
        else
            raise LexErr
    in (
        Printf.printf "buf %d~: %s\n" pos (String.sub buf pos (String.length buf - pos));
        let new_pos, token = read_next_helper buf pos in
        stream.pos <- new_pos; token;
        )

let look_ahead stream :token =
    match stream.curr with
        | None -> let t = read_next stream in
                  (stream.curr <- Some t; t)
        | Some t -> Printf.printf "look ahead: %s\n" (str_of_token t); t

let pop_token stream :token =
    match stream.curr with
        | None -> read_next stream
        | Some t -> Printf.printf "pop: %s\n" (str_of_token t); stream.curr <- None; t

