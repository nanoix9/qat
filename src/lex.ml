open Expr

exception LexErr

type stream = { mutable curr: token option; buf: string; mutable pos: int; mutable line_num: int }

let new_stream str = { curr=None; buf=str; pos=0; line_num=1; }

let read_next stream =
    let pos = stream.pos in
    let buf = stream.buf in
    if stream.pos >= String.length buf then
        stream.curr <- Some EOS
    else begin
        stream.curr <- Some (Id(String.sub buf pos 1));
        stream.pos <- pos + 1;
    end

let look_ahead stream :token =
    match stream.curr with
        | None ->
            begin
                read_next stream;
                match stream.curr with
                    | None -> raise LexErr
                    | Some t -> t
            end
        | Some t -> t

let pop_token stream :token =
    let t = stream.curr in begin
        if t = None then read_next stream;
        match stream.curr with
            | None -> raise LexErr
            | Some t -> t
    end

