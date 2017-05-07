open Qatlib
open Printf
open Evaluate

let main () =
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

let () = main ();;

