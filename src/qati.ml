open Qatlib
open Printf
open Evaluate

let parse_file fn =
    let code = Util.read_file fn in
    printf "%s\n" (run_parse code)
;;

let expand_file fn =
    let interp = make_interp () in
    let code = Util.read_file fn in
    printf "%s\n" (run_expand interp code)
;;

let run_file_no_macro fn =
    let interp = make_interp () in
    let code = Util.read_file fn in
    let _ = run_no_macro interp code in ()
    (*let _ =  printf "%s\n" code in ()*)
;;

let run_file fn =
    let interp = make_interp () in
    let code = Util.read_file fn in
    let _ = run interp code in ()
    (*let _ =  printf "%s\n" code in ()*)
;;

let repl () =
    let interp = make_interp () in
    let cont = ref true in
    while !cont do
        try
            printf ">";
            let line = read_line () in
            printf "%s\n" line;
            let res = run interp line in
            let res_obj = match res with
                | EvalVal obj -> obj
                | _ -> Builtin.nil
            in
            printf "%s\n" (Builtin.str_of_obj res_obj)
        with
        | End_of_file -> cont := false
        | e -> printf "ERROR\n"
    done
;;

let main () =
    let do_parse = ref false in
    let do_expand = ref false in
    let do_trans = ref false in
    let disable_macro = ref false in
    let fn = ref "" in
    let arg_spec = [
        ("-p", Arg.Set do_parse, "parse");
        ("-x", Arg.Set do_expand, "macro expand");
        ("-t", Arg.Set do_trans, "translate");
        ("--disable-macro", Arg.Set disable_macro, "disable macro expansion");
    ] in
    let usage = "Qat Interpreter" in
    Arg.parse arg_spec (fun s -> fn := s) usage;
    let fnn = !fn in
    if fnn <> "" then begin
        if !do_parse then
            parse_file fnn
        else if !do_expand then
            expand_file fnn
        else if !do_trans then
            ()
        else if !disable_macro then
            run_file_no_macro !fn
        else
            run_file !fn
    end
    else
        repl ()
;;


let () = main ();;

