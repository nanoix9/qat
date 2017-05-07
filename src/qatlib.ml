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
