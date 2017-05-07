open Qatlib
open Printf
open Evaluate
module DA = DynArray

let run_file fn =
    let interp = make_interp () in
    let lines = DA.make 10 in
    let cont = ref true in
    let _ =
        let ic = open_in fn in
        while !cont do
            try
                let line = input_line ic in
                DA.add lines line
            with
            | End_of_file -> cont := false
            | e -> raise e
        done;
        close_in ic
    in
    let code = "(" ^ (Util.join_da "\n" lines) ^ ")" in
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
    if Array.length Sys.argv > 1 then
        let fn = Sys.argv.(1) in
        run_file fn
    else
        repl ()
;;


let () = main ();;

