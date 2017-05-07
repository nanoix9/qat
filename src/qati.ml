open Parse
open Expand
open Evaluate

type interp = {mmngr: (Macro.macro_elem, Ast.ast) macro_manager;
    evaluator: evaluator}
;;

let make_interp () =
    {mmngr = create_macro_manager ();
    evaluator = make_evaluator () }
;;

let run interp (code :string) :evalret =
    code |> parse
        |> (expand interp.mmngr)
        |> (evaluate interp.evaluator)
;;

let main () =
    let interp = make_interp () in
    let cont = ref true in
    while !cont do
        try
            Printf.printf ">";
            let line = read_line () in
            Printf.printf "%s\n" line;
            let res = run interp line in
            let res_obj = match res with
                | EvalVal obj -> obj
                | _ -> Builtin.nil
            in
            Printf.printf "%s\n" (Builtin.str_of_obj res_obj)
        with
        | End_of_file -> cont := false
        | e -> Printf.printf "ERROR\n"
    done
;;

let () = main ();;
