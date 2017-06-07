open Ast
open Parse
open Expand
open Sementic
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
        |> sementic_analyze
        |> (evaluate interp.evaluator)
;;

let run_no_macro interp (code :string) :evalret =
    code |> parse
        |> sementic_analyze
        |> (evaluate interp.evaluator)
;;

let run_parse (code :string) :string =
    code |> parse |> pretty_sexp_of_ast
;;

let run_expand interp (code :string) :string =
    code |> parse
        |> (expand interp.mmngr)
        |> pretty_sexp_of_ast
;;

