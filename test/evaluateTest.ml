open OUnit2
open Ast
open Env
open Evaluate
open Builtin

let assert_eval c expect stmt =
    assert_equal ~ctxt:c
        ~cmp:eq_evalret
        ~printer:str_of_evalret
        expect (evaluate stmt)
;;

let assert_eval_val c expect stmt =
    assert_eval c (EvalVal expect) stmt
;;

let e lst :ast = NodeList lst;;
let id s :ast = Atom (Id s);;
let op s :ast = Atom (Op s);;
let i n :ast = Atom (Imm (Int n));;

(* Name the test cases and group them together *)
let suite =
    "evaluate" >::: [
        "test_id" >:: (fun c -> assert_eval_val c
            (make_int 10)
            (i 11));

    ]


