type stream = { buf: string; mutable pos: int; mutable line_num: int }

let new_stream str = { buf=str; pos=0; line_num=1; }

let next_token stream =
    let pos = stream.pos in
    let buf = stream.buf in
    if stream.pos >= String.length buf then
        None
    else begin
        let _ = stream.pos <- pos + 1 in Some (String.sub buf pos 1)
    end

